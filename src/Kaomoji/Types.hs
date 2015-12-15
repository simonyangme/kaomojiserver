{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Kaomoji.Types
       ( KaomojiEntry(..)
       , ProcessedKaomoji(..))
       where

import Control.DeepSeq (NFData)
import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

data KaomojiEntry = KaomojiEntry
  { categories :: [T.Text]
  , kaomoji :: T.Text
  } deriving (Eq, Show, Generic)

instance FromJSON KaomojiEntry

data ProcessedKaomoji = ProcessedKaomoji
  { keywords :: [T.Text]
  , kaomojiText :: T.Text
  } deriving (Eq, Show, Generic, NFData)

instance ToJSON ProcessedKaomoji
