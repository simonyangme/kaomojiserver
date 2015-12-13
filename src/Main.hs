{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.DeepSeq
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List (elem)
import qualified Data.Text as T
import GHC.Generics
import Glider.NLP.Language.English.Porter
import Glider.NLP.Language.English.StopWords
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Random (randomRIO)

main :: IO ()
main = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [KaomojiEntry])
  case d of
    Left err -> putStrLn err
    Right ks -> do
      putStrLn "Processing..."
      let pk = filter ((/=0) . T.length . kaomojiText) . fmap processEntries $ ks
      deepseq pk $ putStrLn "Done! Starting server..."
      run 8081 (app pk)
  putStrLn "hello world"

serverT :: ServerT API (ReaderT [ProcessedKaomoji] IO)
serverT = randomKaomoji :<|> randomEntry

server :: [ProcessedKaomoji] -> Server API
server pk = enter (Nat $ liftIO . (`runReaderT` pk)) serverT

app :: [ProcessedKaomoji] -> Application
app pk = serve (Proxy :: Proxy API) (server pk)

data KaomojiEntry = KaomojiEntry
  { categories :: [T.Text]
  , kaomoji :: T.Text
  } deriving (Eq, Show, Generic)

instance FromJSON KaomojiEntry

type API = QueryParam "keyword" String :> Get '[JSON] (Maybe T.Text)
      :<|> "entry" :> QueryParam "keyword" String :> Get '[JSON] (Maybe ProcessedKaomoji)

data ProcessedKaomoji = ProcessedKaomoji
  { keywords :: [T.Text]
  , kaomojiText :: T.Text
  } deriving (Eq, Show, Generic, NFData)

instance ToJSON ProcessedKaomoji

randomKaomoji :: Maybe String -> ReaderT [ProcessedKaomoji] IO (Maybe T.Text)
randomKaomoji = (fmap.fmap) kaomojiText . randomEntry

randomEntry :: Maybe String -> ReaderT [ProcessedKaomoji] IO (Maybe ProcessedKaomoji)
randomEntry Nothing = do
  ks <- ask
  randomElem <- liftIO $ randomFromList ks
  return randomElem
randomEntry (Just []) = randomEntry Nothing
randomEntry (Just keyword) = do
  ks <- ask
  let kw = stem . T.toLower . T.pack $ keyword
  let newKs = filter (\(ProcessedKaomoji kws _) -> elem kw kws) ks
  randomElem <- liftIO $ randomFromList newKs
  return randomElem

randomFromList :: [a] -> IO (Maybe a)
randomFromList xs = do
  let l = length xs
  case l of
    0 -> return Nothing
    _ -> do
      ind <- liftIO $ randomRIO (0, l - 1)
      return . Just $ xs !! ind

processEntries :: KaomojiEntry -> ProcessedKaomoji
processEntries (KaomojiEntry cats kmj) = ProcessedKaomoji processed kmj
  where
    processed = processWords . T.words . T.toLower . T.unwords $ cats

processWords :: [T.Text] -> [T.Text]
processWords = fmap stem . filter (not . isStopWord)

jsonFile :: FilePath
jsonFile = "kaomoji.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

