{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kaomoji.Docs
       ( apiDocs
       , docsBS)
       where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Kaomoji.Server as S
import Kaomoji.Types
import Servant.Docs
import Servant.API

instance ToParam (QueryParam "keyword" String) where
  toParam _ =
    DocQueryParam "keyword"
                  ["cat", "positive", "animal", "..."]
                  "Keyword describing kaomoji"
                  Normal

instance ToSample String String where
  toSample _ = Just "application/json; charset=utf-8"

instance ToSample (Maybe T.Text) (Maybe T.Text) where
  toSample _ = Just $ Just "=^‥^="

instance ToSample (Maybe ProcessedKaomoji) (Maybe ProcessedKaomoji) where
  toSample _ = Just . Just $ ProcessedKaomoji ["cute", "cat"] ["Cute", "Cats"] "=^‥^="

apiDocs :: API
apiDocs = docs S.api

docsBS :: BS.ByteString
docsBS = encodeUtf8 . LT.pack . markdown
       $ docsWithIntros [intro] S.api
       where
         intro = DocIntro "Welcome" ["This is our super 顔文字 API.", "Enjoy!"]
