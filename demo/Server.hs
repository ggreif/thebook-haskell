{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import           Control.Monad          ((>>=), forM_)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as LBS
import           Data.Conduit
import qualified Data.Conduit.List      as CL
import qualified Data.Conduit.Network   as CN
import qualified Data.ITCH.ITCH51       as ITCH
import qualified Data.ITCH.Types        as ITypes
import           Data.Monoid            ((<>))
import           System.Environment     (getArgs)
import           System.Exit            (ExitCode (..), exitWith)
import qualified Test.QuickCheck.Gen    as Gen
import qualified Test.QuickCheck.Arbitrary as Arbitrary
import           Text.Read              (readMaybe)

server :: CN.AppData IO -> IO ()
server a = produceMessages $$ CN.appSink a
  where bla = undefined
        produceMessages = do
          liftIO $ putStrLn "Starting to pipe messages"
          msgs :: [ITCH.ITCHMessage] <- liftIO $ Gen.sample' Arbitrary.arbitrary
          liftIO . putStrLn $ "Going to push " <> (show $ length msgs) <> " messages"
          forM_ msgs $ \msg -> do
            liftIO . putStrLn $ "Pushing: " <> (show msg)
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
  Just portInt -> putStrLn ("Running on port: " <> port) >> runServer portInt
parse _ = usage >> exitWith ExitSuccess

main :: IO ()
main = getArgs >>= parse
