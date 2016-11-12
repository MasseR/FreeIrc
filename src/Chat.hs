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


respond :: Functor f => Text -> Action f ()
respond r = liftF (InL (Respond r ()))


-- inspired by aiml
newtype Normalized = Normalized [Text]
newtype SentenceStateB a = SentenceStateB {unSentenceStateB :: State (Map Text [Text]) a}
    deriving (Functor, Applicative, Monad, MonadState (Map Text [Text]))
data Matcher = WordMatch !Text
             | Star
             | NamedStar !Text
             deriving (Show, Eq, Ord)

data MatchTree f = Root [MatchTree f]
                 | Leaf !Matcher [MatchTree f] (Maybe (Action f ()))

match :: Matcher -> Text -> SentenceStateB Bool
match (WordMatch x) w | x == w = return True
                      | otherwise = return False
match Star w = modify (M.insertWith (<>) "*" [w]) >> return True
match (NamedStar n) w = modify (M.insertWith (<>) n [w]) >> return True

add :: Functor f => [MatchTree f] -> MatchTree f -> [MatchTree f]
add [] l = [l]
add (Root cs : _) l = [Root (add cs l)]
add (o@(Leaf w cs f) : xs) l@(Leaf w' cs' _)
    | w == w' = Leaf w (concatMap (add cs) cs') f : xs
    | otherwise = o : add xs l
add l (Root cs) = concatMap (add l) cs

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM f = go
    where
        go [] = return Nothing
        go (x:xs) = f x >>= \x' -> if x' then return (Just x) else go xs

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM f = go
    where
        go [] = return False
        go (x:xs) = f x >>= \x' -> if x' then return True else go xs

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

evaluateMatcher :: Functor f => Text -> MatchTree f -> Free (Sum ActionF f) ()
evaluateMatcher sentence tree =
    let (f,s) = runState (unSentenceStateB (matchTree normalizedSentence tree)) M.empty
        normalizedSentence = Normalized . T.words . T.toUpper $ sentence
    in runReaderT (sequence_ f) s
