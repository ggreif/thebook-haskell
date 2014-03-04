{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Data.Text (Text)
import Data.String
import Data.Word (Word8)
import Control.Applicative ((<$>), (<*>), (<*), (<|>), pure)
import Control.Monad ((=<<))
import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Text.XML as XML
import Text.XML (Node, Element, elementAttributes)
import Text.XML.Cursor (fromNode, node, attribute, fromDocument, child, element, ($/), (&//), (&/), (&|))
import qualified Data.ByteString as B
import Language.Haskell.Exts hiding (parseType)
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

lookupRead :: Read a => XML.Name -> Map XML.Name Text -> Maybe a
lookupRead name attrs = readMaybe . T.unpack =<< M.lookup name attrs

-- | Tries to parse 'Message' from 'XML.Element'.
parseMessage :: XML.Element -> Maybe Message
parseMessage e@(XML.Element _ attrs fields)
  = Message <$> M.lookup "name" attrs
            <*> lookupRead "msgtype" attrs
            <*> pure (catMaybes $ map navigate fields)
    where navigate :: XML.Node -> Maybe Field
          navigate = onElement parseField

-- | Attempts to parse 'Field' from 'XML.Element'.
parseField :: XML.Element -> Maybe Field
parseField (XML.Element _ attrs _)
  = Field <$> M.lookup "name" attrs
          <*> (parseType attrs =<< M.lookup "datatype" attrs)

-- | Attempts to parse 'FieldType' from attributes.
parseType :: Map XML.Name Text -> Text -> Maybe FieldType
parseType _ "UInt8"     = Just UInt8
parseType _ "UInt16"    = Just UInt16
parseType _ "UInt32"    = Just UInt32
parseType _ "UInt64"    = Just UInt64
parseType _ "Byte"      = Just Byte
parseType attrs "Price" = Price <$> lookupRead "length" attrs
                                <*> lookupRead "decimal_points" attrs
parseType _ "BitField"  = Just BitField
parseType attrs "Alpha" = Alpha <$> lookupRead "length" attrs
parseType attrs "Date"  = Date  <$> lookupRead "length" attrs
parseType attrs "Time"  = Time  <$> lookupRead "length" attrs
parseType _ _           = Nothing

srcLoc :: SrcLoc
srcLoc = SrcLoc {srcFilename = "foo.hs", srcLine = 1, srcColumn = 1}

fixName :: Text -> Text
fixName = T.replace "ITCH" "" . T.replace " " ""

recordDecl :: Message -> ConDecl
recordDecl (Message name _ fields) = ConDecl ident []
  where ident        = Ident (T.unpack (fixName name))
        --args         = map x fields
        --grp fty      = UnBangedTy $ strongFIXTyCon fty
        --ty True fty  = UnBangedTy $ strongFIXTyCon fty
        --ty False fty = unbangedMaybeFIXTyCon fty
        --x (g@Group{}, _)   = grp g
        --x (f@Field{}, reqd)= ty reqd f

generateMessageConDecl :: Message -> QualConDecl
generateMessageConDecl msg = QualConDecl srcLoc tyVarBind context ctor
  where tyVarBind = []
        context   = []
        ctor      = recordDecl msg

generateMessageDecl :: [Message] -> Decl
generateMessageDecl msgs = decl where
  decl      = DataDecl srcLoc DataType context name' tyVarBind decls derived
  decls     = map generateMessageConDecl msgs
  name'     = Ident "ITCHMessage"
  context   = []
  tyVarBind = []
  derived   = map ((\v -> (v, [])) . UnQual . Ident) ["Generic","Show","Eq"]

generateMessageModule :: [Message] -> Module
generateMessageModule msgs = Module srcLoc modName pragmas warningText exports imports decls
  where modName = ModuleName $ "Data.ITCH"
        pragmas = []
        warningText = Nothing
        exports = Nothing
        imports = []
        decls = messagesDecl
        messagesDecl  = [generateMessageDecl msgs]

main :: IO ()
main = do
   args <- getArgs
   if length args /= 1
    then do
      putStrLn "Usage: Main <Path-to-xml>"
      return ()
    else do
      let path = args !! 0
      -- Get the cursor
      document <- XML.readFile XML.def (fromString path)
      let cursor = fromDocument document

          -- Parse messages
          messages = catMaybes $ cursor $/ element "msgs"
                               &/ element "msg" &| (onElement parseMessage) . node

          types = generateMessageModule messages
          ppr = prettyPrintStyleMode style defaultMode

      putStrLn (ppr types)
      print messages
      return ()

