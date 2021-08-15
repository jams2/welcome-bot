{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import qualified Data.Text.Lazy as T
import Data.Yaml (decodeFileThrow)
import Network.HTTP.Types (status403, status501)
import Network.Wai.Middleware.RequestLogger
import Options.Applicative
import Types
  ( AppServiceConfig (..),
    ErrorResponse (..),
    RoomEvents (..),
    emptyObject,
  )
import Web.Scotty

data Opts = Opts {configPath :: FilePath, port :: Int} deriving (Show)

parseConfigPath :: Parser Opts
parseConfigPath =
  Opts
    <$> strOption (long "config-path" <> short 'c' <> metavar "CONFIG_PATH")
    <*> option auto (long "port" <> short 'p' <> metavar "PORT" <> value 3000)

forbidden :: T.Text -> ActionM ()
forbidden why = do
  status status403
  json $ ErrorResponse "M_FORBIDDEN" why

handler :: ReaderT AppServiceConfig ScottyM ()
handler = do
  knownToken :: T.Text <- asks hsToken
  lift $ middleware logStdoutDev
  lift $
    put "/transactions/:txId" $ do
      token :: T.Text <- param "access_token" `rescue` \_ -> return ""
      if knownToken /= token
        then forbidden "Invalid access token"
        else do
          roomEvents :: RoomEvents <- jsonData
          liftIO $ print roomEvents
          json emptyObject
  lift $
    matchAny (regex "^.*$") $ do
      status status501
      json emptyObject

main :: IO ()
main = do
  opts <-
    execParser $
      info
        (helper <*> parseConfigPath)
        (fullDesc <> Options.Applicative.header "welcome-bot")
  config <- (decodeFileThrow $ configPath opts) :: IO AppServiceConfig
  scotty (port opts) $ runReaderT handler config
