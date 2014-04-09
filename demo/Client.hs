{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Client
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Connects to a server and reads the 'Data.ITCH.Types.UnitHeader' messages
-- and prints them to stdout.
-----------------------------------------------------------------------------
module Main where

import           Control.Applicative          (pure, (<$>), (<*>))
import           Control.Exception            (Exception)
import           Control.Monad.Trans.Resource (MonadThrow, monadThrow)
import qualified Data.Binary                  as B
import qualified Data.Binary.Get              as Get
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as B8
import           Data.Conduit
import qualified Data.Conduit.List            as CL
import qualified Data.Conduit.Network         as CN (AppData, appSource,
                                                     clientSettings,
                                                     runTCPClient)
import qualified Data.ITCH.ITCH51             as ITCH
import qualified Data.ITCH.Types              as ITypes
import           Data.Monoid                  ((<>))
import           Data.Typeable                (Typeable)
import           System.Environment           (getArgs)
import           System.Exit                  (ExitCode (..), exitWith)
import qualified System.IO                    as SIO
import           System.Log.Formatter         (simpleLogFormatter)
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple    (streamHandler)
import qualified System.Log.Logger            as HSL
import           System.Log.Logger.TH         (deriveLoggers)
import           Text.Read                    (readMaybe)

-- | Derive the loggers.
$(deriveLoggers "HSL" [HSL.INFO])

readITCH :: B.Get (ITypes.UnitHeader ITCH.ITCHMessage)
readITCH = ITypes.readMessages

client :: CN.AppData -> IO ()
client a = CN.appSource a $$ conduitGet readITCH
                          =$ CL.mapM_ printHeader
  where printHeader header = do
          infoM $ "Received: " <> show header

runClient :: String -> Int -> IO ()
runClient hostname port = do
    infoM $ "Going to connect to: " <> hostname <> ":" <> show port
    CN.runTCPClient settings client
  where settings = CN.clientSettings port (B8.pack hostname)

-- | Prints the usage information.
usage :: IO ()
usage = putStrLn "Usage: thebook-client [hostname] [port]"

parse :: [String] -> IO ()
parse [hostname, port] = case readMaybe port of
  Nothing      -> putStrLn $ "Cannot parse the port: " <> port
  Just portInt -> runClient hostname portInt
parse _ = usage >> exitWith ExitSuccess

main :: IO ()
main = do
  -- set the logger level
  HSL.updateGlobalLogger HSL.rootLoggerName (HSL.setLevel HSL.INFO)

  -- set the log format
  let format = simpleLogFormatter "$time $prio [$loggername] - $msg"
  stdH <- setFormatter <$> streamHandler SIO.stdout HSL.INFO <*> pure format
  HSL.updateGlobalLogger HSL.rootLoggerName (HSL.setHandlers [stdH])

  -- run the client
  getArgs >>= parse

-- * Helpers

-- | Runs getter repeatedly on a input stream.
conduitGet :: MonadThrow m => Get.Get b -> Conduit B.ByteString m b
conduitGet g = start
  where
    start = do mx <- await
               case mx of
                  Nothing -> return ()
                  Just x -> go (Get.runGetIncremental g `Get.pushChunk` x)
    go (Get.Done bs _ v) = do yield v
                              if B.null bs
                                then start
                                else go (Get.runGetIncremental g `Get.pushChunk` bs)
    go (Get.Fail u o e)  = monadThrow (ParseError u o e)
    go (Get.Partial n)   = await >>= (go . n)

data ParseError = ParseError
  { -- | Data left unconsumed in single stream input value.
    unconsumed :: B.ByteString

    -- | Number of bytes consumed from single stream input value.
  , offset     :: Get.ByteOffset

    -- | Error content.
  , content    :: String
  } deriving (Show, Typeable)

instance Exception ParseError
