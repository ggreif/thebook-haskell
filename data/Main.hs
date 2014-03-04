{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Data.Text (Text)
import Data.String
import Data.Word (Word8)
import Control.Applicative ((<$>), (<*>), (<*), (<|>), pure)
import Control.Monad
import Data.Char (toLower)
import Data.Map (Map)
import Data.List (intercalate, partition, stripPrefix, sortBy)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Debug.Trace (trace)
import qualified Text.XML as XML
import Text.XML (Node, Element, elementAttributes)
import Text.XML.Cursor (fromNode, node, attribute, fromDocument, child, element, descendant, content,  ($/), (&//), (&/), (&|))
import qualified Data.ByteString as B
import Language.Haskell.Exts
import System.Environment (getArgs)
import Text.Read (readMaybe)

-- | A single ITCH message.
data Message = Message Text Word8 [Field] deriving (Show)

-- | A single field of ITCH message.
data Field = Field Text FieldType deriving (Show)

-- | Allowed field types.
data FieldType
  = UInt8
  | UInt16
  | UInt32
  | UInt64
  | Byte
  | Price Int Int
  | BitField
  | Alpha Int
  | Date Int
  | Time Int
  deriving (Show, Read)

onElement :: (XML.Element -> Maybe a) -> XML.Node -> Maybe a
onElement f (XML.NodeElement e) = f e
onElement _ _ = Nothing

-- | Tries to parse 'Message' from 'XML.Element'.
parseMessage :: XML.Element -> Maybe Message
parseMessage e@(XML.Element _ attrs fields)
  = Message <$> M.lookup "name" attrs
            <*> (readMaybe . T.unpack =<< M.lookup "msgtype" attrs)
            <*> pure (catMaybes $ map navigate fields)
    where navigate :: XML.Node -> Maybe Field
          navigate = onElement parseField

-- | Attempts to parse 'Field' from 'XML.Element'.
parseField :: XML.Element -> Maybe Field
parseField (XML.Element _ attrs _)
  = Field <$> M.lookup "name" attrs
          <*> (readMaybe . T.unpack =<< M.lookup "datatype" attrs)

main :: IO ()
main = do
   args <- getArgs
   if length args /= 1
    then do putStrLn "Usage: Main <Path-to-xml>"
            return ()
    else do let path = args !! 0
            -- Get the cursor
            document <- XML.readFile XML.def (fromString path)
            let cursor = fromDocument document

            -- Start descending
            print $ cursor $/ element "msgs"
                           &/ element "msg" &| (onElement parseMessage) . node

            return ()

