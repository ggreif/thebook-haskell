{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Data.Text (Text)
import Data.String
import Data.Word (Word8)
import Control.Applicative ((<$>), (<*>), (<*), (<|>), pure)
import Control.Monad (void, (=<<), forever)
import Data.Char (toLower)
import Data.Map (Map)
import Data.Monoid ((<>))
import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe)
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
import Control.Concurrent (threadDelay)

-- * Datatypes

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

-- * XML schema parser

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
            <*> pure (mapMaybe navigate fields)
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

-- * AST generation

srcLoc :: Hs.SrcLoc
srcLoc = Hs.SrcLoc {Hs.srcFilename = "foo.hs", Hs.srcLine = 1, Hs.srcColumn = 1}

fM :: Hs.QName
fM = Hs.UnQual . Hs.name $ "(<$>)"

fS :: Hs.QName
fS = Hs.UnQual . Hs.name $ "(<*>)"

fSeq :: Hs.QName
fSeq = Hs.UnQual . Hs.name $ "(*>)"

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
  UInt8        -> Hs.name "UInt8"
  UInt16       -> Hs.name "UInt8"
  UInt32       -> Hs.name "UInt32"
  UInt64       -> Hs.name "UInt64"
  Byte         -> Hs.name "Byte"
  Price _ _    -> Hs.name "Price"
  BitField     -> Hs.name "BitField"
  Alpha length -> Hs.name "Alpha"
  Date _       -> Hs.name "Date"
  Time _       -> Hs.name "Time"

fieldName :: Message -> Field -> String
fieldName (Message msg _ _) (Field name _) = T.unpack $ "_" <> fixNameCamel msg <> fixName name

fieldDecl :: Message -> Field -> ([Hs.Name], Hs.BangType)
fieldDecl m@(Message msg _ _) f@(Field name t) 
  = ([Hs.name $ fieldName m f], Hs.UnpackedTy . Hs.TyCon . Hs.UnQual . fieldTypeToName $ t)

messageConstr :: Message -> Hs.Name
messageConstr m@(Message name _ _)
  = Hs.name . T.unpack . fixName $ name

recordDecl :: Message -> Hs.ConDecl
recordDecl m@(Message name _ fields) = Hs.RecDecl ident args
  where ident        = messageConstr m
        args         = map (fieldDecl m) fields

generateMessageConDecl :: Message -> Hs.QualConDecl
generateMessageConDecl msg = Hs.QualConDecl srcLoc tyVarBind context ctor
  where tyVarBind = []
        context   = []
        ctor      = recordDecl msg

generateMessageDecl :: [Message] -> Hs.Decl
generateMessageDecl msgs = decl where
  decl      = Hs.DataDecl srcLoc Hs.DataType context itchMessageADT tyVarBind decls derived
  decls     = map generateMessageConDecl msgs
  context   = []
  tyVarBind = []
  derived   = map ((\v -> (v, [])) . Hs.UnQual . Hs.name) ["Show","Eq"]

-- ** Binary instance and functions generation for a message.

getFunctionName :: Message -> String
getFunctionName (Message msg _ _) = T.unpack $ "get" <> fixName msg

putFunctionName :: Message -> String
putFunctionName (Message msg _ _) = T.unpack $ "put" <> fixName msg

getApp :: Message -> [Field] -> Hs.Exp
getApp _ []     = error "Sorry"
getApp msg [Field _ t]  = Hs.App (Hs.App (Hs.Var fM) (Hs.Con . Hs.UnQual . messageConstr $ msg)) (Hs.Var . Hs.UnQual . Hs.name $ "get")
getApp msg (f:fs) = Hs.App (Hs.App (Hs.Var fS) (getApp msg fs)) (Hs.Var . Hs.UnQual . Hs.name $ "get")

generateGetFunction :: Message -> [Hs.Decl]
generateGetFunction msg@(Message name _ fields) = decl where
  decl = [typeDef, body]
  name' = Hs.Ident $ getFunctionName msg
  typeDef = Hs.TypeSig srcLoc [name'] (Hs.TyApp (Hs.TyVar . Hs.name $ "Get") (Hs.TyVar . Hs.name $ "ITCHMessage"))
  body = Hs.FunBind [Hs.Match srcLoc name' [] Nothing arbitraryRhs (Hs.BDecls [])]
  arbitraryRhs = Hs.UnGuardedRhs $ if null fields
                  then Hs.App (Hs.Var . Hs.UnQual . Hs.name $ "pure") (Hs.Con . Hs.UnQual . messageConstr $ msg)
                  else getApp msg fields

putApp :: Message -> [Field] -> Hs.Exp
putApp _ []     = error "Sorry"
putApp msg [f@(Field n t)]  = Hs.App (Hs.Var . Hs.UnQual . Hs.name $ "put") (Hs.App (Hs.Var . Hs.UnQual . Hs.name $ fieldName msg f) (Hs.Var . Hs.UnQual . Hs.name $ "msg"))
putApp msg (f:fs) = Hs.App (Hs.App (Hs.Var fSeq) (putApp msg fs)) (Hs.App (Hs.Var . Hs.UnQual . Hs.name $ "put") (Hs.App (Hs.Var . Hs.UnQual . Hs.name $ fieldName msg f) (Hs.Var . Hs.UnQual . Hs.name $ "msg")))

generatePutFunction :: Message -> Hs.Exp
generatePutFunction msg@(Message name _ fields)
  = if null fields
      then Hs.App (Hs.Var . Hs.UnQual . Hs.name $ "return") (Hs.Tuple Hs.Boxed [])
      else putApp msg fields

 -- | Generates a 'Data.Binary' instance for the messages.
generateBinaryInstance :: [Message] -> Hs.Decl
generateBinaryInstance msgs = decl where
  decl = Hs.InstDecl srcLoc [] name [type'] [decls]
  decls = Hs.InsDecl . Hs.FunBind $ [getDef] ++ map putDef msgs ++ [putUnknown]
  getDef = Hs.Match srcLoc (Hs.name "get") [] Nothing getRhs (Hs.BDecls [])
  getRhs = Hs.UnGuardedRhs getDo
  msgTypeVar = Hs.name $ "msgType"
  getDo  = Hs.Do [Hs.Generator srcLoc (Hs.PVar msgTypeVar) (Hs.Var . Hs.UnQual . Hs.name $ "getMessageType"), getDoCase]
  getDoCase = Hs.Qualifier $ Hs.Case (Hs.Var . Hs.UnQual $ msgTypeVar) (getDoCases ++ [getEmptyDoCase])
  getDoCases = map (\msg@(Message n t _) -> Hs.Alt srcLoc (Hs.PLit . Hs.Int . fromIntegral $ t) (Hs.UnGuardedAlt (Hs.Var . Hs.UnQual . Hs.name . getFunctionName $ msg)) (Hs.BDecls [])) msgs
  getEmptyDoCase = Hs.Alt srcLoc Hs.PWildCard (Hs.UnGuardedAlt (Hs.App (Hs.Var . Hs.UnQual . Hs.name $ "fail") (Hs.Lit . Hs.String $ "Unknown msg type"))) (Hs.BDecls [])
  msgName = Hs.name $ "msg"
  putDef msg = Hs.Match srcLoc (Hs.name "put") [Hs.PAsPat msgName (Hs.PRec (Hs.UnQual . messageConstr $ msg) [])] Nothing (putRhs msg) (Hs.BDecls [])
  putRhs msg@(Message _ t _) = Hs.UnGuardedRhs $ Hs.App (Hs.App (Hs.Var fSeq) (Hs.App (Hs.Var . Hs.UnQual . Hs.name $ "putMessageType") (Hs.Lit . Hs.Int . fromIntegral $ t))) (generatePutFunction msg)
  putUnknown = Hs.Match srcLoc (Hs.name "put") [Hs.PWildCard] Nothing (Hs.UnGuardedRhs $ Hs.App (Hs.Var . Hs.UnQual . Hs.name $ "fail") (Hs.Lit . Hs.String $ "Unknown msg type")) (Hs.BDecls [])
  arbitraryFunName = Hs.Var . Hs.UnQual . Hs.name . arbitraryFunctionName
  name = Hs.UnQual . Hs.name $ "Binary"
  type' = Hs.TyCon . Hs.UnQual $ itchMessageADT

-- ** Arbitrary instance and functions generation for a message.

arbitraryFunctionName :: Message -> String
arbitraryFunctionName (Message msg _ _) = T.unpack $ "arbitrary" <> fixName msg

arbitraryApp :: Message -> [Field] -> Hs.Exp
arbitraryApp _ []     = error "Sorry"
arbitraryApp msg [Field _ t]  = Hs.App (Hs.App (Hs.Var fM) (Hs.Con . Hs.UnQual . messageConstr $ msg)) (Hs.Var . Hs.UnQual . Hs.name $ "arbitrary")
arbitraryApp msg (f:fs) = Hs.App (Hs.App (Hs.Var fS) (arbitraryApp msg fs)) (Hs.Var . Hs.UnQual . Hs.name $ "arbitrary")

generateArbitraryFunction :: Message -> [Hs.Decl]
generateArbitraryFunction msg@(Message name _ fields) = decl where
  decl = [typeDef, body]
  name' = Hs.Ident $ arbitraryFunctionName msg
  typeDef = Hs.TypeSig srcLoc [name'] (Hs.TyApp (Hs.TyVar . Hs.name $ "Gen") (Hs.TyVar . Hs.name $ "ITCHMessage"))
  body = Hs.FunBind [Hs.Match srcLoc name' [] Nothing arbitraryRhs (Hs.BDecls [])]
  arbitraryRhs = Hs.UnGuardedRhs $ if null fields
                  then Hs.App (Hs.Var . Hs.UnQual . Hs.name $ "pure") (Hs.Con . Hs.UnQual . messageConstr $ msg)
                  else arbitraryApp msg fields

-- | Generates 'Test.QuickCheck.Arbitrary' instance for the messages.
generateArbitraryInstance :: [Message] -> Hs.Decl
generateArbitraryInstance msgs = decl where
  decl = Hs.InstDecl srcLoc [] name [type'] [decls]
  decls = Hs.InsDecl . Hs.FunBind $ [arbitraryDef]
  arbitraryDef = Hs.Match srcLoc (Hs.name "arbitrary") [] Nothing arbitraryRhs (Hs.BDecls [])
  arbitraryRhs = Hs.UnGuardedRhs $ Hs.App (Hs.Var . Hs.UnQual . Hs.name $ "oneof") (Hs.List (map arbitraryFunName msgs))
  arbitraryFunName = Hs.Var . Hs.UnQual . Hs.name . arbitraryFunctionName
  name = Hs.UnQual . Hs.name $ "Arbitrary"
  type' = Hs.TyCon . Hs.UnQual $ itchMessageADT

-- ** Factory function for a message

factoryFunctionName :: Message -> String
factoryFunctionName (Message msg _ _) = T.unpack $ fixNameCamel msg

generateFactoryFunction :: Message -> [Hs.Decl]
generateFactoryFunction msg@(Message name _ fields) = decl where
  decl = [typeDef, body]
  name' = Hs.Ident $ factoryFunctionName msg
  typeDef = Hs.TypeSig srcLoc [name'] types
  types = foldr1 Hs.TyFun $ fieldsT ++ [Hs.TyVar . Hs.name $ "ITCHMessage"]
  fieldsT = map (Hs.TyVar . (\(Field _  t) -> fieldTypeToName t)) fields
  body = Hs.FunBind [Hs.Match srcLoc name' [] Nothing arbitraryRhs (Hs.BDecls [])]
  arbitraryRhs = Hs.UnGuardedRhs . Hs.Con . Hs.UnQual . messageConstr $ msg

generateMessageModule :: String -> [Message] -> Hs.Module
generateMessageModule version msgs = Hs.Module srcLoc modName pragmas warningText exports imports decls
  where modName = Hs.ModuleName $ "Data.ITCH.ITCH" <> version
        pragmas = []
        warningText = Nothing
        exports = Just [
            Hs.EThingAll . Hs.UnQual $ itchMessageADT
          ]
        imports = importDecl <$> [
            "Data.ITCH.Types"
          , "Test.QuickCheck.Arbitrary"
          , "Test.QuickCheck.Gen"
          , "Data.Binary"
          , "Data.Binary.Put"
          , "Data.Binary.Get"
          , "Control.Applicative"
          ]
        decls = getFunctions <> [binaryInstance, arbitraryInstance, messageDecl] <> factoryFunctions <> arbitraryFunctions
        factoryFunctions = concatMap generateFactoryFunction msgs
        arbitraryFunctions = concatMap generateArbitraryFunction msgs
        arbitraryInstance = generateArbitraryInstance msgs
        getFunctions = concatMap generateGetFunction msgs
        binaryInstance = generateBinaryInstance msgs
        messageDecl = generateMessageDecl msgs
        importDecl name = Hs.ImportDecl srcLoc (Hs.ModuleName name) False False Nothing Nothing Nothing

main :: IO ()
main = void $ do
   args <- getArgs
   if length args /= 2
    then do
      putStrLn "Usage: Main <Path-to-xml> <Version>"
      return ()
    else do
      let path    = head args
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

      threadDelay (20 * 1000000)

      putStrLn "Hello1"

