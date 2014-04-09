{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Untitled
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Starts a server and writes random the 'Data.ITCH.Types.UnitHeader' messages
-- to any client that connects.
-----------------------------------------------------------------------------
module Main where

import           Control.Applicative       (pure, (<$>), (<*>))
import           Control.Monad             (forM_)
import           Control.Monad.IO.Class    (liftIO)
import qualified Data.ByteString.Lazy      as LBS
import           Data.Conduit
import qualified Data.Conduit.Network      as CN
import qualified Data.ITCH.ITCH51          as ITCH
import qualified Data.ITCH.Types           as ITypes
import           Data.Monoid               ((<>))
import           System.Environment        (getArgs)
import           System.Exit               (ExitCode (..), exitWith)
import qualified System.IO                 as SIO
import           System.Log.Formatter      (simpleLogFormatter)
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (streamHandler)
import qualified System.Log.Logger         as HSL
import           System.Log.Logger.TH      (deriveLoggers)
import qualified Test.QuickCheck.Arbitrary as Arbitrary
import qualified Test.QuickCheck.Gen       as Gen
import           Text.Read                 (readMaybe)

-- | Derive the loggers.
$(deriveLoggers "HSL" [HSL.INFO])

server :: CN.AppData -> IO ()
server a = produceMessages $$ CN.appSink a
  where produceMessages = do
          infoM "Starting to pipe messages"
          msgs :: [ITCH.ITCHMessage] <- liftIO $ Gen.sample' Arbitrary.arbitrary
          infoM $ "Going to push " <> show (length msgs) <> " messages"
          forM_ msgs $ \msg -> do
            infoM $ "Pushing: " <> show msg
            yield . LBS.toStrict $ ITypes.writeMessages 1 (ITypes.uint32 2) [msg]

runServer :: Int -> IO ()
runServer port = CN.runTCPServer settings server
  where settings = CN.serverSettings port "*"

-- | Prints the usage information.
usage :: IO ()
usage = putStrLn "Usage: thebook-server [port]"

parse :: [String] -> IO ()
parse [port] = case readMaybe port of
  Nothing      -> putStrLn $ "Cannot parse the port: " <> port
  Just portInt -> infoM ("Running on port: " <> port) >> runServer portInt
parse _ = usage >> exitWith ExitSuccess

main :: IO ()
main = do
  -- create handler
  let format = simpleLogFormatter "$time $prio [$loggername] - $msg"
  stdH <- setFormatter <$> streamHandler SIO.stdout HSL.INFO <*> pure format

  -- set levels and handler
  let loggerSetup = HSL.setLevel HSL.INFO . HSL.setHandlers [stdH]
  HSL.updateGlobalLogger HSL.rootLoggerName loggerSetup

  getArgs >>= parse
