{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Kaomoji.Server
       ( API
       , server
       , api)
       where

import Control.Monad.Reader
import qualified Data.Text as T
import Glider.NLP.Language.English.Porter
import Kaomoji.Types
import Servant
import System.Random (randomRIO)

-- The Content-type header is a temporary fix until Servant solves this issue
-- See issue https://github.com/haskell-servant/servant/issues/263 for latest
type API = "kaomoji" :> QueryParam "keyword" String :> Get '[JSON] (Headers '[Header "Content-type" String] (Maybe T.Text))
      :<|> "entry" :> QueryParam "keyword" String :> Get '[JSON] (Headers '[Header "Content-type" String] (Maybe ProcessedKaomoji))

serverT :: ServerT API (ReaderT [ProcessedKaomoji] IO)
serverT = randomKaomoji :<|> randomEntry

server :: [ProcessedKaomoji] -> Server API
server pk = enter (Nat $ liftIO . (`runReaderT` pk)) serverT

api :: Proxy API
api = Proxy

randomKaomoji :: Maybe String -> ReaderT [ProcessedKaomoji] IO (Headers '[Header "Content-type" String] (Maybe T.Text))
randomKaomoji = (fmap.fmap.fmap) kaomojiText . randomEntry

randomEntry :: Maybe String -> ReaderT [ProcessedKaomoji] IO (Headers '[Header "Content-type" String] (Maybe ProcessedKaomoji))
randomEntry Nothing = do
  ks <- ask
  randomElem <- liftIO $ randomFromList ks
  return $ addHeader "application/json; charset=utf-8" randomElem
randomEntry (Just []) = randomEntry Nothing
randomEntry (Just keyword) = do
  ks <- ask
  let kw = stem . T.toLower . T.pack $ keyword
  let newKs = filter (\(ProcessedKaomoji kws _ _) -> elem kw kws) ks
  randomElem <- liftIO $ randomFromList newKs
  return $ addHeader "application/json; charset=utf-8" randomElem

randomFromList :: [a] -> IO (Maybe a)
randomFromList xs = do
  let l = length xs
  case l of
    0 -> return Nothing
    _ -> do
      ind <- liftIO $ randomRIO (0, l - 1)
      return . Just $ xs !! ind
