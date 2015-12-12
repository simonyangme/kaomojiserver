{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

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
    Right ks -> run 8081 (app . fmap processEntries $ ks)
  putStrLn "hello world"

server :: ServerT API (Reader [KaomojiEntry])
server = randomKaomoji

app :: [ProcessedKaomoji] -> Application
app pk = serve (Proxy :: Proxy API) (server pk)

data KaomojiEntry = KaomojiEntry
  { categories :: [T.Text]
  , kaomoji :: T.Text
  } deriving (Eq, Show, Generic)

instance FromJSON KaomojiEntry
instance ToJSON KaomojiEntry

type API = QueryParam "keyword" String :> Get '[JSON] (Maybe T.Text)

data ProcessedKaomoji = ProcessedKaomoji
  { keywords :: [T.Text]
  , kaomojiText :: T.Text
  } deriving (Eq, Show, Generic)

randomKaomoji :: [ProcessedKaomoji] -> Maybe String -> EitherT ServantErr IO (Maybe T.Text)
randomKaomoji ks Nothing = do
  randomElem <- liftIO $ randomFromList ks
  return . Just . kaomojiText $ randomElem
randomKaomoji ks (Just []) = randomKaomoji ks Nothing
randomKaomoji ks (Just keyword) = do
  let kw = T.pack keyword
  let newKs = filter (\(ProcessedKaomoji kws _) -> elem kw kws) ks
  randomElem <- liftIO $ randomFromList newKs
  return . Just . kaomojiText $ randomElem

randomFromList :: [a] -> IO a
randomFromList xs = do
  let l = length xs
  ind <- liftIO $ randomRIO (0, l - 1)
  return $ xs !! ind

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

