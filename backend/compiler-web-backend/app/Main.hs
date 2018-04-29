{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config

-- import Data.Monoid
import Data.IORef
import Data.Text (Text)

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

main :: IO ()
main = do
  ref <- newIORef 0
  spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
  runSpock 8080 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app = do
  get root $ text "Use GET /compile?code=${code} to compile LazyScript code."
  get ("/compile") $ do
    setHeader "Access-Control-Allow-Headers" "*"
    setHeader "Access-Control-Allow-Origin" "*"
    code <- param "code"
    case compileCode <$> code of
      Just compiledCode -> text compiledCode
      Nothing -> text "ERROR: code query param missing."

compileCode :: Text -> Text
compileCode _ =
  "const a = () => [1, () => a()];"
