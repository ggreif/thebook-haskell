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
import Data.Monoid ((<>))
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Text.XML as XML
import Text.XML (Node, Element, elementAttributes)
import Text.XML.Cursor (fromNode, node, attribute, fromDocument, child, element, ($/), (&//), (&/), (&|))
import qualified Data.ByteString as B
import Language.Haskell.Exts hiding (parseType)
import System.Environment (getArgs)
import System.FilePath (joinPath)
import System.IO (hFlush, stdout)
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

fixNameCamel :: Text -> Text
fixNameCamel n = if T.length n > 0
                  then let fixedName = fixName n
                       in  T.cons (toLower . T.head $ fixedName) (T.drop 1 fixedName)
                  else n

fieldTypeToName :: FieldType -> Name
fieldTypeToName ft = case ft of
  UInt8        -> Ident "UInt8"
  UInt16       -> Ident "UInt8"
  UInt32       -> Ident "UInt32"
  UInt64       -> Ident "UInt64"
  Byte         -> Ident "Byte"
  Price _ _    -> Ident "Data.Decimal"
  BitField     -> Ident "BitField"
  Alpha length -> Ident "Data.ByteString.Char8.ByteString"
  Date _       -> Ident "Data.Time.Calendar.Day"
  Time _       -> Ident "Data.Time.Clock.DiffTime"

fieldDecl :: Message -> Field -> ([Name], BangType)
fieldDecl (Message msg _ _) (Field name t) =
  let fieldName = Ident . T.unpack $ "_" <> fixNameCamel msg <> fixName name
  in ([fieldName], UnpackedTy . TyCon . UnQual . fieldTypeToName $ t)

--strongFIXTyCon :: Field -> Type
--strongFIXTyCon f@(Field name t) =
--  let fieldTy   = foldl1 TyApp [TyCon fieldQN, TyCon (typeNat fieldID), TyCon (strongFIXQName f)]
--      fieldQN
--        | isEnumField f = Qual (ModuleName "AlphaHeavy.FIX") $ Ident "Enumeration"
--        | otherwise     = Qual (ModuleName "AlphaHeavy.FIX") $ Ident "Field"
--      optField
--        | requiredField = id
--        | otherwise     = TyApp (TyCon (Qual (ModuleName "Prelude") (Ident "Maybe")))
--  in optField fieldTy

recordDecl :: Message -> ConDecl
recordDecl m@(Message name _ fields) = RecDecl ident args
  where ident        = Ident . T.unpack . fixName $ name
        args         = map (fieldDecl m) fields

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

generateMessageModule :: String -> [Message] -> Module
generateMessageModule version msgs = Module srcLoc modName pragmas warningText exports imports decls
  where modName = ModuleName $ "Data.ITCH.ITCH" <> version
        pragmas = []
        warningText = Nothing
        exports = Nothing
        imports = [
            ImportDecl srcLoc (ModuleName "Data.ITCH.Types") False False Nothing Nothing Nothing
          ]
        decls = messagesDecl
        messagesDecl  = [generateMessageDecl msgs]

main :: IO ()
main = do
   args <- getArgs
   if length args /= 2
    then do
      putStrLn "Usage: Main <Path-to-xml> <Version>"
      return ()
    else do
      let path    = args !! 0
          version = args !! 1
      -- Get the cursor
      document <- XML.readFile XML.def (fromString path)
      let cursor = fromDocument document

          -- Parse messages
          messages = catMaybes $ cursor $/ element "msgs"
                               &/ element "msg" &| (onElement parseMessage) . node

          types = generateMessageModule version messages
          ppr = prettyPrintStyleMode style defaultMode

      putStr (ppr types)
      putStrLn "Hello"
      -- hFlush stdout
      -- print messages
      return ()

