{-# Language OverloadedStrings #-}
{-# Language FlexibleContexts #-}
module Hooks.PlusOne where


import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Hooks.Algebra
import Network.IRC
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Text.StringLike (castString)
import Text.HTML.TagSoup
import Data.Maybe (listToMaybe)
import Data.Char (isSpace)
import Data.List (find)
import Data.Acid.Database
import Data.Monoid
import Types

plusOneHook (PrivMsg nick target msg) =
  case T.words msg of
       (which:"+1":_) -> update (PlusOne (T.filter ((/= ':')) which)) >> respondTo nick target "+1'd"
       ("!top":n:[]) -> query (TopOnes (read $ T.unpack n)) >>= respondTo nick target . format
       _ -> return ()
  where
    format = T.intercalate ", "
