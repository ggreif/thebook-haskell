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
import qualified Language.Haskell.Exts as Hs
import qualified Language.Haskell.Exts.Syntax as Hs
import qualified Language.Haskell.Exts.Build as Hs
import qualified Language.Haskell.Exts.SrcLoc as Hs
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

-- | Attempts to parse 'Message' from 'XML.Element'.
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

itchMessageADT :: Hs.Name
itchMessageADT = Hs.Ident "ITCHMessage"

-- | Names in the name tag are of the form "ITCH Add Order"
-- This is not a valid haskell data constructor, so we need
-- to fix that.
fixName :: Text -> Text
fixName = T.replace "ITCH" "" . T.replace " " ""

fixNameCamel :: Text -> Text
fixNameCamel n = if T.length n > 0
                  then let fixedName = fixName n
                       in  T.cons (toLower . T.head $ fixedName) (T.drop 1 fixedName)
                  else n

fieldTypeToName :: FieldType -> Hs.Name
fieldTypeToName ft = case ft of
  UInt8        -> Hs.sym "UInt8"
  UInt16       -> Hs.sym "UInt8"
  UInt32       -> Hs.sym "UInt32"
  UInt64       -> Hs.sym "UInt64"
  Byte         -> Hs.sym "Byte"
  Price _ _    -> Hs.sym "Data.Decimal"
  BitField     -> Hs.sym "BitField"
  Alpha length -> Hs.sym "Data.ByteString.Char8.ByteString"
  Date _       -> Hs.sym "Data.Time.Calendar.Day"
  Time _       -> Hs.sym "Data.Time.Clock.DiffTime"

fieldDecl :: Message -> Field -> ([Hs.Name], Hs.BangType)
fieldDecl (Message msg _ _) (Field name t) =
  let fieldName = Hs.sym . T.unpack $ "_" <> fixNameCamel msg <> fixName name
  in ([fieldName], Hs.UnpackedTy . Hs.TyCon . Hs.UnQual . fieldTypeToName $ t)

messageConstr :: Message -> Hs.Name
messageConstr m@(Message name _ _)
  = Hs.Ident . T.unpack . fixName $ name

recordDecl :: Message -> Hs.ConDecl
recordDecl m@(Message name _ fields) = Hs.RecDecl ident args
  where ident        = messageConstr m
        args         = map (fieldDecl m) fields

generateMessageConDecl :: Message -> Hs.QualConDecl
generateMessageConDecl msg = Hs.QualConDecl Hs.noLoc tyVarBind context ctor
  where tyVarBind = []
        context   = []
        ctor      = recordDecl msg

generateMessageDecl :: [Message] -> Hs.Decl
generateMessageDecl msgs = decl where
  decl      = Hs.DataDecl Hs.noLoc Hs.DataType context itchMessageADT tyVarBind decls derived
  decls     = map generateMessageConDecl msgs
  context   = []
  tyVarBind = []
  derived   = map ((\v -> (v, [])) . Hs.UnQual . Hs.name) ["Generic","Show","Eq"]

arbitraryFunctionName :: Message -> String
arbitraryFunctionName (Message msg _ _) = T.unpack $ " arbitrary" <> fixName msg

generateArbitraryInstance :: [Message] -> Hs.Decl
generateArbitraryInstance msgs = decl where
  decl = Hs.InstDecl Hs.noLoc [] name [type'] [decls]
  decls = Hs.InsDecl . Hs.FunBind $ [arbitraryDef]
  arbitraryDef = Hs.Match Hs.noLoc (Hs.Ident "arbitrary") [] Nothing arbitraryRhs (Hs.BDecls [])
  arbitraryRhs = Hs.UnGuardedRhs $ Hs.App (Hs.Var . Hs.UnQual . Hs.name $ "oneof") (Hs.List (map arbitraryFunName msgs))
  arbitraryFunName = Hs.Var . Hs.UnQual . Hs.name . arbitraryFunctionName
  name = Hs.UnQual . Hs.name $ "Arbitrary"
  type' = Hs.TyCon . Hs.UnQual $ itchMessageADT

fM :: Hs.QName
fM = Hs.UnQual . Hs.name $ "<$>"

fS :: Hs.QName
fS = Hs.UnQual . Hs.name $ "<*>"

arbitraryApp :: [Field] -> Hs.Exp
arbitraryApp []     = error "Sorry"
arbitraryApp [f]    = Hs.Var . Hs.UnQual . Hs.name $ "arbitrary"
arbitraryApp (f:fs) = Hs.App (Hs.Var . Hs.UnQual . Hs.name $ "arbitrary") $ Hs.App (Hs.Var fS) (arbitraryApp fs)

generateArbitraryFunction :: Message -> [Hs.Decl]
generateArbitraryFunction msg@(Message name _ fields) = decl where
  decl = [typeDef, body]
  name' = Hs.Ident $ arbitraryFunctionName msg
  typeDef = Hs.TypeSig Hs.noLoc [name'] (Hs.TyApp (Hs.TyVar . Hs.name $ "Arbitrary") (Hs.TyVar . Hs.name $ "ITCHMessage"))
  body = Hs.FunBind $ [Hs.Match Hs.noLoc name' [] Nothing arbitraryRhs (Hs.BDecls [])]
  arbitraryRhs = if null fields
                  then Hs.UnGuardedRhs $ Hs.App (Hs.Var . Hs.UnQual . Hs.name $ "pure") (Hs.Con . Hs.UnQual . messageConstr $ msg)
                  else Hs.UnGuardedRhs $ Hs.App (Hs.Con . Hs.UnQual . messageConstr $ msg) (Hs.App (Hs.Var fM) (arbitraryApp fields))

generateMessageModule :: String -> [Message] -> Hs.Module
generateMessageModule version msgs = Hs.Module Hs.noLoc modName pragmas warningText exports imports decls
  where modName = Hs.ModuleName $ "Data.ITCH.ITCH" <> version
        pragmas = []
        warningText = Nothing
        exports = Just [
            Hs.EThingAll . Hs.UnQual $ itchMessageADT
          ]
        imports = importDecl <$> [
            "Data.ITCH.Types"
          , "Data.Decimal"
          , "Data.ByteString.Char8"
          , "Data.Time.Calendar"
          , "Data.Time.Clock"
          , "Test.QuickCheck.Arbitrary"
          , "Test.QuickCheck.Gen"
          , "Data.Binary.Put"
          , "Data.Binary.Get"
          , "Control.Applicative"
          ]
        decls = (concat $ map generateArbitraryFunction msgs) ++ [generateArbitraryInstance msgs, generateMessageDecl msgs]
        importDecl name = Hs.ImportDecl Hs.noLoc (Hs.ModuleName name) False False Nothing Nothing Nothing

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
          ppr = Hs.prettyPrintStyleMode Hs.style Hs.defaultMode

      putStr (ppr types)
      putStrLn "Hello"
      -- hFlush stdout
      -- print messages
      return ()

