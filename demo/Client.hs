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

import qualified Data.Binary           as B
import qualified Data.Binary.Get       as Get
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as LBS
import           Data.Conduit
import qualified Data.Conduit.List     as CL
import qualified Data.Conduit.Network  as CN (AppData, appSource,
                                              clientSettings, runTCPClient)
import qualified Data.ITCH.ITCH51      as ITCH
import qualified Data.ITCH.Types       as ITypes
import           Data.Monoid           ((<>))
import           System.Environment    (getArgs)
import           System.Exit           (ExitCode (..), exitWith)
import           Text.Read             (readMaybe)

readITCH :: B.Get (ITypes.UnitHeader ITCH.ITCHMessage)
readITCH = ITypes.readMessages

client :: CN.AppData -> IO ()
client a = CN.appSource a $$ CL.map toMessage
                          =$ CL.mapM_ print
  where toMessage = Get.runGet readITCH . LBS.fromStrict

runClient :: String -> Int -> IO ()
runClient hostname port = CN.runTCPClient settings client
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
main = getArgs >>= parse
