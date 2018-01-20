{-# Language OverloadedStrings #-}
{-# Language RankNTypes #-}
{-# Language FlexibleContexts #-}
module Chat  where

import Control.Monad.State
import Control.Monad.Reader
import Data.Text (Text)
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Maybe (maybeToList)

default (Text)

type SentenceState = Map Text [Text]




newtype Normalized = Normalized [Text]
-- newtype SentenceStateB a = SentenceStateB {unSentenceStateB :: State (Map Text [Text]) a}
--     deriving (Functor, Applicative, Monad, MonadState (Map Text [Text]))
type SentenceStateB a = forall m. MonadState (Map Text [Text]) m => m a
type Ctx = Map Text [Text]

-- |Describe different matching types
data Matcher = WordMatch !Text -- ^ The most basic matcher, matches the word under cursor
             | Star -- ^ Matches any word and is recorded under '*' in the state
             | NamedStar !Text -- ^ Matches any word and is recorded under the name given in constructor
             deriving (Show, Eq, Ord)

{-|
MatchTree f is the foundation for matching content. It is basically a trie,
with matchers as the leafs. The leafs should be tried against normalized words.
|-}
data MatchTree m = Root [MatchTree m]
                 | Leaf !Matcher [MatchTree m] (Maybe (Ctx -> m ()))

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
matchTree :: Monad m => Normalized -> MatchTree m -> SentenceStateB [Ctx -> m ()]
matchTree (Normalized xs) = go xs
    where
        go :: Monad m => [Text] -> MatchTree m -> SentenceStateB [Ctx -> m ()]
        go [] (Leaf _ _ f) = return (maybeToList f)
        go ws (Root cs) = concat <$> mapM (go ws) cs
        go [w] (Leaf m _ f) = match m w >>=
            \matched -> if matched then return (maybeToList f) else return []
        go (w:ws) (Leaf m cs _) = match m w >>=
            \matched -> if matched then concat <$> mapM (go ws) cs else return []


build :: Monad m => [(Ctx -> m (), Text)] -> MatchTree m
build ws =
    case foldr (flip add . uncurry fromText) [] ws of
         [Root cs] -> Root cs
         _ -> Root []


{-|
Build a tree *spine* from a sentence.

The given sentence is uppercased and parsed for some tokens. Tokens '*' and
':variablename:' are understood, creating Star and NamedStar respectively
|-}
fromText :: Monad m => (Ctx -> m ()) -> Text -> MatchTree m
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
runMatcher :: Monad m => Text -> MatchTree m -> m ()
runMatcher sentence tree =
    let (fs,s) = runState (matchTree normalizedSentence tree) M.empty
        normalizedSentence = Normalized . T.words . T.toUpper $ sentence
    in mapM_ (\f -> f s) fs
