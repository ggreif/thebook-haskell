{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Applicative             (pure, (<$>), (<*>))
import           Data.Char                       (toLower)
import           Data.Map                        (Map)
import qualified Data.Map                        as M
import           Data.Maybe                      (catMaybes, mapMaybe)
import           Data.Monoid                     (Sum (..), mconcat, (<>))
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Word                       (Word8)
import           Distribution.ModuleName         (fromString)
import           Distribution.PackageDescription
import           Distribution.Simple
import           Distribution.Simple.Setup
import           Filesystem.Path.CurrentOS       (decodeString)
import qualified Language.Haskell.Exts           as Hs
import           Text.Read                       (readMaybe)
import qualified Text.XML                        as XML
import           Text.XML.Cursor                 (element, fromDocument, node,
                                                  ($/), (&/), (&|))

-- | thebook.cabal - defined here programatically,
-- as otherwise defining all the dependencies multiple times
-- is tedious.

ver :: [Int] -> Version
ver version
  = Version { versionBranch = version
            , versionTags = []
            }

anyV :: String -> Dependency
anyV name = Dependency (PackageName name) anyVersion

within :: String -> [Int] -> Dependency
within name verI = Dependency (PackageName name) (withinVersion $ ver verI)

exactly :: String -> [Int] -> Dependency
exactly name versionInts = Dependency (PackageName name) (thisVersion version)
  where version = Version { versionBranch = versionInts
                          , versionTags = []
                          }

orLater :: String -> [Int] -> Dependency
orLater name verI = Dependency (PackageName name) (orLaterVersion $ ver verI )

thebookDeps :: [Dependency]
thebookDeps =
  [
    Dependency (PackageName "base") (intersectVersionRanges (orLaterVersion $ ver [4]) (earlierVersion $ ver [5]))
  , anyV "mtl"
  , orLater "bytestring" [0, 10]
  , anyV "text"
  , anyV "time"
  , anyV "containers"
  , orLater "binary" [0, 6, 3]
  , anyV "lens"
  , within "QuickCheck" [2, 6]
  , anyV "Decimal"
  , anyV "old-locale"
  , orLater "fixhs" [0,1,4]
    -- Codegen deps
  , anyV "directory"
  , anyV "xml-conduit"
  , anyV "filepath"
  , anyV "text"
  , anyV "bytestring"
  , anyV "haskell-src-exts"
  , anyV "containers"
  , anyV "filepath"
  , anyV "system-filepath"
  ]

thebookDemoDeps :: [Dependency]
thebookDemoDeps
  = thebookDeps ++ [

    ]


-- | library: thebook
thebookLibrary :: Library
thebookLibrary
  = Library {
      exposedModules = map fromString [
          "Data.ITCH.Types"
        , "Data.ITCH.ITCH51"
        , "Data.TheBook.MarketData"
        , "Data.TheBook.OUCH"
        , "Data.TheBook.Order"
        , "Data.TheBook.Types"
        , "Data.TheBook.Book"
      ]
    , libExposed = True
    , libBuildInfo = buildInfo
    }
  where buildInfo = emptyBuildInfo {
              buildTools = thebookDeps
            , hsSourceDirs = ["src", "generated"]
            , defaultLanguage = Just Haskell2010
            , options = [(GHC, ["-Wall", "-fwarn-incomplete-patterns"])]
            }

thebookExecutable :: Executable
thebookExecutable
  = Executable {
      exeName = "thebook"
    , modulePath = "Main.hs"
    , buildInfo = buildInfo
    }
  where buildInfo = emptyBuildInfo {
            buildTools = thebookDeps
          , hsSourceDirs = ["src", "generated"]
          , defaultLanguage = Just Haskell2010
          , options = [(GHC, [
                       "-Wall"
                     , "-O2"
                     , "-fwarn-incomplete-patterns"
                     , "-rtsopts"
                     , "-threaded"
                     ])]
          }

-- | Location of ITCH xmls.
itchXmlDir :: String
itchXmlDir = "generated"

-- | Generates the haskell code, for now the xml path is hardcoded.
generateMessages
  :: Args
  -> BuildFlags
  -> IO HookedBuildInfo
generateMessages _ _ = do
  let path = decodeString "ITCH51.xml"
      version = "51"
  document <- XML.readFile XML.def path
  let cursor = fromDocument document
      -- Parse messages
      messages = catMaybes $ cursor $/ element "msgs"
                               &/ element "msg" &| (onElement parseMessage) . node
      types = generateMessageModule version messages
      ppr = Hs.prettyPrintStyleMode Hs.style Hs.defaultMode

  putStrLn "Generating"

  -- Don't bother creating the dirs
  writeFile (itchXmlDir <> "/Data/ITCH/ITCH51.hs") (ppr types)
  return emptyHookedBuildInfo

main :: IO ()
main = do
  defaultMainWithHooks simpleUserHooks {
    preBuild = generateMessages
  }

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
parseMessage (XML.Element _ attrs fields)
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

dataITCHTypes :: Hs.ModuleName
dataITCHTypes = Hs.ModuleName "Data.ITCH.Types"

srcLoc :: Hs.SrcLoc
srcLoc = Hs.SrcLoc {Hs.srcFilename = "foo.hs", Hs.srcLine = 1, Hs.srcColumn = 1}

fM :: Hs.QName
fM = Hs.UnQual . Hs.name $ "(<$>)"

fS :: Hs.QName
fS = Hs.UnQual . Hs.name $ "(<*>)"

fSeq :: Hs.QName
fSeq = Hs.UnQual . Hs.name $ "(*>)"

fSeqB :: Hs.QName
fSeqB = Hs.UnQual . Hs.name $ "(<*)"

fMinus :: Hs.QName
fMinus = Hs.UnQual . Hs.name $ "(-)"

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
  Alpha _      -> Hs.name "Alpha"
  Date _       -> Hs.name "Date"
  Time _       -> Hs.name "Time"

fieldTypeToByteSize :: FieldType -> Integer
fieldTypeToByteSize ft = case ft of
  UInt8     -> 1
  UInt16    -> 2
  UInt32    -> 4
  UInt64    -> 8
  Byte      -> 1
  Price _ _ -> 8
  BitField  -> 1
  Alpha l   -> fromIntegral l
  Date _    -> 8
  Time _    -> 8

-- | Returns 'Hs.PrimInt' with the length of the message in bytes including msgLength and msgType fields.
messageToByteSize :: Message -> Integer
messageToByteSize (Message _ _ fields) = getSum len
  where len = (Sum 2) {- msgLength (1 byte)  + msgType (1 byte) = 2 bytes -} <> fieldsLen
        fieldsLen = mconcat $ map (\(Field _ ft) -> Sum $ fieldTypeToByteSize ft) fields

messageSizeExp :: Message -> Hs.Exp
messageSizeExp = Hs.Lit . Hs.Int . messageToByteSize

fieldName :: Message -> Field -> String
fieldName (Message msg _ _) (Field name _) = T.unpack $ "_" <> fixNameCamel msg <> fixName name

fieldDecl :: Message -> Field -> ([Hs.Name], Hs.BangType)
fieldDecl m@(Message _ _ _) f@(Field _ t)
  = ([Hs.name $ fieldName m f], Hs.UnpackedTy . Hs.TyCon . Hs.Qual dataITCHTypes . fieldTypeToName $ t)

messageConstr :: Message -> Hs.Name
messageConstr (Message name _ _)
  = Hs.name . T.unpack . fixName $ name

recordDecl :: Message -> Hs.ConDecl
recordDecl m@(Message _ _ fields) = Hs.RecDecl ident args
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

getFunc :: Field -> Hs.Exp
getFunc (Field _ (Alpha l)) = Hs.App (Hs.Var . Hs.Qual dataITCHTypes . Hs.name $ "getAlpha") (Hs.Lit . Hs.Int . fromIntegral $ l)
getFunc _ = Hs.Var . Hs.UnQual . Hs.name $ "get"

getApp :: Message -> [Field] -> Hs.Exp
getApp _ []     = error "Sorry"
getApp msg [f@(Field _ _)]  = Hs.App (Hs.App (Hs.Var fM) (Hs.Con . Hs.UnQual . messageConstr $ msg)) (getFunc f)
getApp msg (f:fs) = Hs.App (Hs.App (Hs.Var fS) (getApp msg fs)) (getFunc f)

generateGetFunction :: Message -> [Hs.Decl]
generateGetFunction msg@(Message _ _ fields) = decl where
  decl = [typeDef, body]
  name' = Hs.Ident $ getFunctionName msg
  typeDef = Hs.TypeSig srcLoc [name'] (Hs.TyApp (Hs.TyVar . Hs.name $ "Get") (Hs.TyVar . Hs.name $ "ITCHMessage"))
  body = Hs.FunBind [Hs.Match srcLoc name' [] Nothing arbitraryRhs (Hs.BDecls [])]
  arbitraryRhs = Hs.UnGuardedRhs $ if null fields
                  then Hs.App (Hs.Var . Hs.UnQual . Hs.name $ "pure") (Hs.Con . Hs.UnQual . messageConstr $ msg)
                  else getApp msg (reverse fields)

putFunc :: Field -> Message -> Hs.Exp
putFunc f@(Field _ (Alpha l)) msg = Hs.App (Hs.App (Hs.Var . Hs.Qual dataITCHTypes . Hs.name $ "putAlpha") (Hs.Lit . Hs.Int . fromIntegral $ l)) (Hs.App (Hs.Var . Hs.UnQual . Hs.name $ fieldName msg f) (Hs.Var . Hs.UnQual . Hs.name $ "msg"))
putFunc f msg = Hs.App (Hs.Var . Hs.UnQual . Hs.name $ "put") (Hs.App (Hs.Var . Hs.UnQual . Hs.name $ fieldName msg f) (Hs.Var . Hs.UnQual . Hs.name $ "msg"))

putApp :: Message -> [Field] -> Hs.Exp
putApp _ []     = error "Sorry"
putApp msg [f]  = putFunc f msg
putApp msg (f:fs) = Hs.App (Hs.App (Hs.Var fSeq) (putApp msg fs)) (putFunc f msg)

generatePutFunction :: Message -> Hs.Exp
generatePutFunction msg@(Message _ _ fields)
  = if null fields
      then Hs.App (Hs.Var . Hs.UnQual . Hs.name $ "return") (Hs.Tuple Hs.Boxed [])
      else putApp msg (reverse fields)

 -- | Generates a 'Data.Binary' instance for the messages.
generateBinaryInstance :: [Message] -> Hs.Decl
generateBinaryInstance msgs = decl where
  decl = Hs.InstDecl srcLoc [] name [type'] [decls]
  decls = Hs.InsDecl . Hs.FunBind $ [getDef] ++ map putDef msgs
  getDef = Hs.Match srcLoc (Hs.name "get") [] Nothing getRhs (Hs.BDecls [])
  getRhs = Hs.UnGuardedRhs getDo
  msgTypeVar = Hs.name "msgType"
  msgLengthVar = Hs.name "msgLength"
  getDo  = Hs.Do [
      Hs.Generator srcLoc (Hs.PVar msgLengthVar) (Hs.Var . Hs.Qual dataITCHTypes . Hs.name $ "getMessageLength")
    , Hs.Generator srcLoc (Hs.PVar msgTypeVar) (Hs.Var . Hs.Qual dataITCHTypes . Hs.name $ "getMessageType")
    , getDoCase
    ]
  getDoCase = Hs.Qualifier $ Hs.Case (Hs.Var . Hs.UnQual $ msgTypeVar) (getDoCases ++ [getEmptyDoCase])
  getDoCases = map (\msg@(Message _ t _) -> Hs.Alt srcLoc (Hs.PLit . Hs.Int . fromIntegral $ t) (Hs.UnGuardedAlt (getMessageAndRemainingBytes msg)) (Hs.BDecls [])) msgs
  getMessageAndRemainingBytes msg = Hs.App (Hs.App (Hs.Var fSeqB) (Hs.Var . Hs.UnQual . Hs.name . getFunctionName $ msg)) (Hs.App (Hs.App (Hs.Var . Hs.Qual dataITCHTypes . Hs.name $ "skipRemaining") (Hs.Var . Hs.UnQual $ msgLengthVar)) (messageSizeExp msg))
  getEmptyDoCase = Hs.Alt srcLoc Hs.PWildCard (Hs.UnGuardedAlt (Hs.App (Hs.Var . Hs.UnQual . Hs.name $ "fail") (Hs.Lit . Hs.String $ "Unknown msg type"))) (Hs.BDecls [])
  msgName = Hs.name $ "msg"
  putPattern msg@(Message _ _ fields) = if null fields then Hs.PRec (Hs.UnQual . messageConstr $ msg) [] else Hs.PAsPat msgName (Hs.PRec (Hs.UnQual . messageConstr $ msg) [])
  putDef msg = Hs.Match srcLoc (Hs.name "put") [putPattern msg] Nothing (putRhs msg) (Hs.BDecls [])
  putRhs msg = Hs.UnGuardedRhs $ Hs.App (Hs.App (Hs.Var fSeq) (Hs.App (Hs.Var . Hs.Qual dataITCHTypes . Hs.name $ "putMessageLength") (messageSizeExp msg))) (putMessageType msg)
  putMessageType msg@(Message _ t _) = Hs.App (Hs.App (Hs.Var fSeq) (Hs.App (Hs.Var . Hs.Qual dataITCHTypes . Hs.name $ "putMessageType") (Hs.Lit . Hs.Int . fromIntegral $ t))) (generatePutFunction msg)
  name = Hs.UnQual . Hs.name $ "Binary"
  type' = Hs.TyCon . Hs.UnQual $ itchMessageADT

-- ** Arbitrary instance and functions generation for a message.

arbitraryFunctionName :: Message -> String
arbitraryFunctionName (Message msg _ _) = T.unpack $ "arbitrary" <> fixName msg

arbitraryFunc :: Field -> Hs.Exp
arbitraryFunc (Field _ (Alpha l)) = Hs.App (Hs.Var . Hs.Qual dataITCHTypes . Hs.name $ "arbitraryAlpha") (Hs.Lit . Hs.Int . fromIntegral $ l)
arbitraryFunc _ = (Hs.Var . Hs.UnQual . Hs.name $ "arbitrary")

arbitraryApp :: Message -> [Field] -> Hs.Exp
arbitraryApp _ []     = error "Sorry"
arbitraryApp msg [f]  = Hs.App (Hs.App (Hs.Var fM) (Hs.Con . Hs.UnQual . messageConstr $ msg)) (arbitraryFunc f)
arbitraryApp msg (f:fs) = Hs.App (Hs.App (Hs.Var fS) (arbitraryApp msg fs)) (arbitraryFunc f)

generateArbitraryFunction :: Message -> [Hs.Decl]
generateArbitraryFunction msg@(Message _ _ fields) = decl where
  decl = [typeDef, body]
  name' = Hs.Ident $ arbitraryFunctionName msg
  typeDef = Hs.TypeSig srcLoc [name'] (Hs.TyApp (Hs.TyVar . Hs.name $ "Gen") (Hs.TyVar . Hs.name $ "ITCHMessage"))
  body = Hs.FunBind [Hs.Match srcLoc name' [] Nothing arbitraryRhs (Hs.BDecls [])]
  arbitraryRhs = Hs.UnGuardedRhs $ if null fields
                  then Hs.App (Hs.Var . Hs.UnQual . Hs.name $ "pure") (Hs.Con . Hs.UnQual . messageConstr $ msg)
                  else arbitraryApp msg (reverse fields)

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
factoryFunctionName (Message msg _ _) = T.unpack $ "_" <> fixNameCamel msg

generateFactoryFunction :: Message -> [Hs.Decl]
generateFactoryFunction msg@(Message _ _ fields) = decl where
  decl = [typeDef, body]
  name' = Hs.Ident $ factoryFunctionName msg
  typeDef = Hs.TypeSig srcLoc [name'] types
  types = foldr1 Hs.TyFun $ fieldsT ++ [Hs.TyVar . Hs.name $ "ITCHMessage"]
  fieldsT = map (Hs.TyCon . Hs.Qual dataITCHTypes . (\(Field _  t) -> fieldTypeToName t)) fields
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
        imports = [dataItchTypesImport] <> (importDecl <$> [
            "Test.QuickCheck.Arbitrary"
          , "Test.QuickCheck.Gen"
          , "Data.Binary"
          , "Control.Applicative"
          ])
        decls = getFunctions <> [binaryInstance, arbitraryInstance, messageDecl] <> factoryFunctions <> arbitraryFunctions
        factoryFunctions = concatMap generateFactoryFunction msgs
        arbitraryFunctions = concatMap generateArbitraryFunction msgs
        arbitraryInstance = generateArbitraryInstance msgs
        getFunctions = concatMap generateGetFunction msgs
        binaryInstance = generateBinaryInstance msgs
        messageDecl = generateMessageDecl msgs
        importDecl name = Hs.ImportDecl srcLoc (Hs.ModuleName name) False False Nothing Nothing Nothing
        dataItchTypesImport = Hs.ImportDecl srcLoc (Hs.ModuleName "Data.ITCH.Types") True False Nothing Nothing Nothing


