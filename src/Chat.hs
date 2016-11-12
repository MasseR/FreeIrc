{-# Language OverloadedStrings #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveFunctor #-}
module Chat where

import Control.Monad.State
import Control.Monad.Reader
import Data.Text (Text)
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Control.Monad.Free
import Data.Functor.Sum
import Data.Maybe (maybeToList)

default (Text)

data ActionF a = Respond Text a deriving (Functor)
data VoidF a

type Action f a = ReaderT SentenceState (Free (Sum ActionF f)) a
type SentenceState = Map Text [Text]


-- |Respond to the client
respond :: Functor f => Text -> Action f ()
respond r = liftF (InL (Respond r ()))


newtype Normalized = Normalized [Text]
newtype SentenceStateB a = SentenceStateB {unSentenceStateB :: State (Map Text [Text]) a}
    deriving (Functor, Applicative, Monad, MonadState (Map Text [Text]))

-- |Describe different matching types
data Matcher = WordMatch !Text -- ^ The most basic matcher, matches the word under cursor
             | Star -- ^ Matches any word and is recorded under '*' in the state
             | NamedStar !Text -- ^ Matches any word and is recorded under the name given in constructor
             deriving (Show, Eq, Ord)

{-|
MatchTree f is the foundation for matching content. It is basically a trie,
with matchers as the leafs. The leafs should be tried against normalized words.
|-}
data MatchTree f = Root [MatchTree f]
                 | Leaf !Matcher [MatchTree f] (Maybe (Action f ()))

-- |Match a matcher against a normalized word.
match :: Matcher -> Text -> SentenceStateB Bool
match (WordMatch x) w | x == w = return True
                      | otherwise = return False
match Star w = modify (M.insertWith (<>) "*" [w]) >> return True
match (NamedStar n) w = modify (M.insertWith (<>) n [w]) >> return True

{-|
Combine tries together. The trie in the second argument is tried against all
the tries in the first argument. If there is a match, the subtrees are joined
recursively, otherwise the new trie is prepended to the list of tries.
|-}
add :: Functor f => [MatchTree f] -> MatchTree f -> [MatchTree f]
add [] l = [l]
add (Root cs : _) l = [Root (add cs l)]
add (o@(Leaf w cs f) : xs) l@(Leaf w' cs' _)
    | w == w' = Leaf w (concatMap (add cs) cs') f : xs
    | otherwise = o : add xs l
add l (Root cs) = concatMap (add l) cs


{-|
Match a normalized wordlist against a ready tree. Return a list of actions
while building the sentence state at the same time.
|-}
matchTree :: Normalized -> MatchTree f -> SentenceStateB [Action f ()]
matchTree (Normalized xs) = go xs
    where
        go :: [Text] -> MatchTree f -> SentenceStateB [Action f ()]
        go [] (Leaf _ _ f) = return (maybeToList f)
        go ws (Root cs) = concat <$> mapM (go ws) cs
        go [w] (Leaf m _ f) = match m w >>=
            \matched -> if matched then return (maybeToList f) else return []
        go (w:ws) (Leaf m cs _) = match m w >>=
            \matched -> if matched then concat <$> mapM (go ws) cs else return []


build :: Functor f => [(Action f (), Text)] -> MatchTree f
build ws =
    case foldr (flip add . uncurry fromText) [] ws of
         [Root cs] -> Root cs
         _ -> Root []


{-|
Build a tree *spine* from a sentence.

The given sentence is uppercased and parsed for some tokens. Tokens '*' and
':variablename:' are understood, creating Star and NamedStar respectively
|-}
fromText :: Action f () -> Text -> MatchTree f
fromText f = Root . addAction . go . T.words . T.toUpper
    where
        go ("*":ws) = [Leaf Star (go ws) Nothing]
        go (w:ws) | isVar w = [Leaf (NamedStar w) (go ws) Nothing]
                  | otherwise = [Leaf (WordMatch w) (go ws) Nothing]
        go [] = []
        isVar w = all ($ w) [(":" `T.isSuffixOf`), (":" `T.isPrefixOf`)]
        addAction [Leaf w [] _] = [Leaf w [] (Just f)]
        addAction [Leaf w cs _] = [Leaf w (addAction cs) Nothing]
        addAction x = x

-- |Run the matcher against input text. The text is normalized.
runMatcher :: Functor f => Text -> MatchTree f -> Free (Sum ActionF f) ()
runMatcher sentence tree =
    let (f,s) = runState (unSentenceStateB (matchTree normalizedSentence tree)) M.empty
        normalizedSentence = Normalized . T.words . T.toUpper $ sentence
    in runReaderT (sequence_ f) s
