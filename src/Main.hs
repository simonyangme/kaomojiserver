{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Control.DeepSeq
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Glider.NLP.Language.English.Porter
import Glider.NLP.Language.English.StopWords
import Kaomoji.Docs
import Kaomoji.Server
import Kaomoji.Types
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Utils.StaticFiles (serveDirectory)

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

type DocsAPI = "api" :> Raw :<|> API :<|> Raw

app :: [ProcessedKaomoji] -> Application
app pk = serve (Proxy :: Proxy DocsAPI) (serveDocs :<|> server pk :<|> serveDirectory "./root/")
  where serveDocs _ respond =
          respond $ responseLBS ok200 [plain] docsBS
        plain = ("Content-Type", "text/plain; charset=utf8")

processEntries :: KaomojiEntry -> ProcessedKaomoji
processEntries (KaomojiEntry cats kmj) = ProcessedKaomoji processed cats kmj
  where
    processed = processWords . T.words . T.toLower . T.unwords $ cats

processWords :: [T.Text] -> [T.Text]
processWords = fmap stem . filter (not . isStopWord)

jsonFile :: FilePath
jsonFile = "kaomoji.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

