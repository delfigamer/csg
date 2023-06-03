module Pak.Parser
    ( loadGeometryFromFile
    )
where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Foldable
import Data.IORef
import Data.Int
import Data.Sequence (Seq (..))
import Data.Word
import Pak.Polys
import qualified Data.Attoparsec.ByteString as Atp
import qualified Data.Attoparsec.ByteString.Lazy as Atp.Lazy
import qualified Data.Attoparsec.Internal.Types as Atp.Internal
import qualified Data.Array as A
import qualified Data.Array.IO as IOA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Encoding.Error as Text.Encoding.Error
import qualified Data.Sequence as Seq
import qualified GHC.Float

decodeUtf8Lenient :: BS.ByteString -> Text.Text
decodeUtf8Lenient =
    Text.Encoding.decodeUtf8With Text.Encoding.Error.lenientDecode

substream :: Int -> Atp.Parser a -> Atp.Parser a
substream maxlen inner = do
    substr <- Atp.take maxlen
    handleResult $ Atp.parse inner substr
  where
    handleResult (Atp.Fail leftover ctx msg) =
        Atp.Internal.Parser $ \t (Atp.Internal.Pos pos) more lose _suc -> do
            let pos' = pos - BS.length leftover
            lose t (Atp.Internal.Pos pos') more (reverse ctx) msg
    handleResult (Atp.Partial cont) =
        handleResult $ cont ""
    handleResult (Atp.Done _ result) =
        pure result

data PakHeader = PakHeader
    { phPackageFlags :: Word32
    , phNameCount :: Int32
    , phNameOffset :: Int32
    , phExportCount :: Int32
    , phExportOffset :: Int32
    , phImportCount :: Int32
    , phImportOffset :: Int32
    }
  deriving (Show)

data Name = Name
    { nameString :: BS.ByteString
    , nameFlags :: Word32
    }
  deriving (Show)

type NameTable = A.Array Int32 Name

data ImportHeader = ImportHeader
    { ihClassPackage :: Name
    , ihClassName :: Name
    , ihPackageIndex :: Int32
    , ihObjectName :: Name
    }

data ExportHeader = ExportHeader
    { ehClassObjectIndex :: Int32
    , ehSuperObjectIndex :: Int32
    , ehPackageIndex :: Int32
    , ehObjectName :: Name
    , ehObjectFlags :: Word32
    , ehSerialSize :: Int32
    , ehSerialOffset :: Int32
    }

buildDWord ::
    Word8 ->
    Word8 ->
    Word8 ->
    Word8 ->
    Word32
buildDWord a0 a1 a2 a3 =
    (fromIntegral a0) .|.
    (fromIntegral a1 `shiftL` 8) .|.
    (fromIntegral a2 `shiftL` 16) .|.
    (fromIntegral a3 `shiftL` 24)

buildQWord ::
    Word8 -> Word8 ->
    Word8 -> Word8 ->
    Word8 -> Word8 ->
    Word8 -> Word8 ->
    Word64
buildQWord a0 a1 a2 a3 a4 a5 a6 a7 =
    (fromIntegral a0) .|.
    (fromIntegral a1 `shiftL` 8) .|.
    (fromIntegral a2 `shiftL` 16) .|.
    (fromIntegral a3 `shiftL` 24) .|.
    (fromIntegral a4 `shiftL` 32) .|.
    (fromIntegral a5 `shiftL` 40) .|.
    (fromIntegral a6 `shiftL` 48) .|.
    (fromIntegral a7 `shiftL` 56)

bytes :: Int -> Atp.Parser [Word8]
bytes n = do
    BS.unpack <$> Atp.take n

byte :: Atp.Parser Word8
byte = Atp.anyWord8

word :: Atp.Parser Word16
word = do
    [a0, a1] <- bytes 2
    pure $
        (fromIntegral a0) .|.
        (fromIntegral a1 `shiftL` 8)

dword :: Atp.Parser Word32
dword = do
    [a0, a1, a2, a3] <- bytes 4
    pure $ buildDWord a0 a1 a2 a3

qword :: Atp.Parser Word64
qword = do
    [a0, a1, a2, a3, a4, a5, a6, a7] <- bytes 8
    pure $ buildQWord a0 a1 a2 a3 a4 a5 a6 a7

short :: Atp.Parser Int16
short = fromIntegral <$> word

long :: Atp.Parser Int32
long = fromIntegral <$> dword

longlong :: Atp.Parser Int64
longlong = fromIntegral <$> qword

float :: Atp.Parser Float
float = GHC.Float.castWord32ToFloat <$> dword

varint :: Atp.Parser Int32
varint = down0
  where
    down0 = do
        b0 <- byte
        if b0 .&. 0x40 /= 0
            then down1 b0
            else pure $ up1 0 b0
    down1 b0 = do
        b1 <- byte
        if b1 .&. 0x80 /= 0
            then down2 b0 b1
            else pure $ up2 0 b0 b1
    down2 b0 b1 = do
        b2 <- byte
        if b2 .&. 0x80 /= 0
            then down3 b0 b1 b2
            else pure $ up3 0 b0 b1 b2
    down3 b0 b1 b2 = do
        b3 <- byte
        if b3 .&. 0x80 /= 0
            then down4 b0 b1 b2 b3
            else pure $ up4 0 b0 b1 b2 b3
    down4 b0 b1 b2 b3 = do
        b4 <- byte
        pure $ up4 (fromIntegral b4) b0 b1 b2 b3
    up4 x4 b0 b1 b2 b3 = do
        let x3 = x4 `shiftL` 7 .|. (fromIntegral b3 .&. 0x7f)
        up3 x3 b0 b1 b2
    up3 x3 b0 b1 b2 = do
        let x2 = x3 `shiftL` 7 .|. (fromIntegral b2 .&. 0x7f)
        up2 x2 b0 b1
    up2 x2 b0 b1 = do
        let x1 = x2 `shiftL` 7 .|. (fromIntegral b1 .&. 0x7f)
        up1 x1 b0
    up1 x1 b0 = do
        let x0 = x1 `shiftL` 6 .|. (fromIntegral b0 .&. 0x3f)
        if b0 .&. 0x80 /= 0
            then -x0
            else x0

varstring :: Atp.Parser BS.ByteString
varstring = do
    strlen <- varint
    str <- Atp.take (fromIntegral strlen - 1)
    void $ Atp.word8 0
    pure str

varname :: NameTable -> Atp.Parser BS.ByteString
varname nameTable = do
    idx <- varint
    pure $ nameString (nameTable A.! idx)

vector :: Atp.Parser Vector
vector =
    Vector
        <$> float
        <*> float
        <*> float

rotator :: Atp.Parser Rotator
rotator =
    Rotator
        <$> long
        <*> long
        <*> long

scale :: Atp.Parser Scale
scale =
    Scale
        <$> float
        <*> float
        <*> float
        <*> float
        <*> fmap (toEnum . fromEnum) (Atp.satisfy (\b -> b < 7))

parseHeader :: Atp.Parser PakHeader
parseHeader = do
    void $ Atp.string "\xc1\x83\x2a\x9e" <|> fail "bad signature"
    packageVersion <- dword
    when (packageVersion < 68) $ do
        fail "bad version"
    PakHeader
        <$> dword
        <*> long
        <*> long
        <*> long
        <*> long
        <*> long
        <*> long

parseNameTable :: Int32 -> Atp.Parser NameTable
parseNameTable nameCount = do
    nameList <-
        forM [0 .. nameCount-1] $ \_ -> do
            Name
                <$> varstring
                <*> dword
    pure $ A.listArray (0, nameCount - 1) nameList

parseImportHeaderTable ::
    NameTable ->
    Int32 ->
    Atp.Parser (A.Array Int32 ImportHeader)
parseImportHeaderTable nameTable importCount = do
    importList <-
        forM [0 .. importCount-1] $ \_ -> do
            ImportHeader
                <$> ((nameTable A.!) <$> varint)
                <*> ((nameTable A.!) <$> varint)
                <*> long
                <*> ((nameTable A.!) <$> varint)
    pure $ A.listArray (0, importCount - 1) importList

parseExportHeaderTable ::
    NameTable ->
    Int32 ->
    Atp.Parser (A.Array Int32 ExportHeader)
parseExportHeaderTable nameTable exportCount = do
    exportList <-
        forM [0 .. exportCount-1] $ \_ -> do
            partial <-
                ExportHeader
                    <$> varint
                    <*> varint
                    <*> long
                    <*> ((nameTable A.!) <$> varint)
                    <*> dword
            serialSize <- varint
            if serialSize > 0
                then partial serialSize <$> varint
                else partial serialSize <$> pure 0
    pure $ A.listArray (0, exportCount - 1) exportList

data ObjectRef = ObjectRef
    { refClassName :: BS.ByteString
    , refObjectName :: BS.ByteString
    , refOrigin :: ObjectOrigin
    }

instance Show ObjectRef where
    show = BS.Char8.unpack . objectRefString

data ObjectOrigin
    = ObjectOriginImport
    | ObjectOriginExport ObjectExport

data ObjectExport = ObjectExport
    { eSuper :: Maybe ObjectRef
    , eObjectFlags :: Word32
    , eSerialSize :: Int32
    , eSerialOffset :: Int32
    }

type ObjectRefTable = A.Array Int32 (Maybe ObjectRef)

objectRefString :: ObjectRef -> BS.ByteString
objectRefString ObjectRef {..} =
    refClassName <> "'" <> refObjectName <> "'"

data ObjectRefTableIntermediate
    = Waiting
    | Processing
    | Done (Maybe ObjectRef)

buildObjectRefTable ::
    A.Array Int32 ImportHeader ->
    A.Array Int32 ExportHeader ->
    IO ObjectRefTable
buildObjectRefTable importHeaderTable exportHeaderTable = do
    (0, importMax) <- pure $ A.bounds importHeaderTable
    (0, exportMax) <- pure $ A.bounds exportHeaderTable
    workbuf <-
        IOA.newArray @IOA.IOArray (-importMax-1, exportMax+1) Waiting
    IOA.writeArray workbuf 0 $ Done Nothing
    fillObjectRefTable importHeaderTable exportHeaderTable workbuf

fillObjectRefTable ::
    A.Array Int32 ImportHeader ->
    A.Array Int32 ExportHeader ->
    IOA.IOArray Int32 ObjectRefTableIntermediate ->
    IO ObjectRefTable
fillObjectRefTable importHeaderTable exportHeaderTable workbuf = do
    elems <- forM [-importMax-1 .. exportMax+1] $ \i -> do
        mbRef <- deref i
        pure (i, mbRef)
    pure $ A.array (-importMax-1, exportMax+1) elems
  where
    (_, importMax) = A.bounds importHeaderTable
    (_, exportMax) = A.bounds exportHeaderTable
    deref i = do
        current <- IOA.readArray workbuf i
        case current of
            Done mbRef -> pure mbRef
            Processing -> fail @IO "imports/exports circular reference"
            Waiting -> do
                IOA.writeArray workbuf i Processing
                mbRef <-
                    if i < 0
                        then buildImportRef (importHeaderTable A.! (- i - 1))
                        else buildExportRef (exportHeaderTable A.! (i - 1))
                IOA.writeArray workbuf i $ Done mbRef
                pure mbRef
    buildImportRef ImportHeader {..} = do
        let qcn =
                nameString ihClassPackage <> "." <> nameString ihClassName
        mbPackageRef <- deref ihPackageIndex
        qon <-
            case mbPackageRef of
                Nothing ->
                    pure $ nameString ihObjectName
                Just packageRef ->
                    pure $
                        refObjectName packageRef <> "." <>
                        nameString ihObjectName
        pure $ Just $ ObjectRef
            { refClassName = qcn
            , refObjectName = qon
            , refOrigin = ObjectOriginImport
            }
    buildExportRef ExportHeader {..} = do
        mbClassRef <- deref ehClassObjectIndex
        qcn <-
            case mbClassRef of
                Nothing -> pure "None"
                Just classRef -> pure $ refObjectName classRef
        mbPackageRef <- deref ehPackageIndex
        qon <-
            case mbPackageRef of
                Nothing ->
                    pure $ "MyLevel." <> nameString ehObjectName
                Just packageRef ->
                    pure $
                        refObjectName packageRef <> "." <>
                        nameString ehObjectName
        mbSuper <- deref ehSuperObjectIndex
        pure $ Just $ ObjectRef
            { refClassName = qcn
            , refObjectName = qon
            , refOrigin = ObjectOriginExport $
                ObjectExport
                    { eSuper = mbSuper
                    , eObjectFlags = ehObjectFlags
                    , eSerialSize = ehSerialSize
                    , eSerialOffset = ehSerialOffset
                    }
            }

data Property = Property BS.ByteString Int32 PropValue

instance Show Property where
    showsPrec _ (Property name 0 value) =
        showString "\n    " .
        showString (BS.Char8.unpack name) .
        showString " = " .
        showsPrec 0 value
    showsPrec _ (Property name idx value) =
        showString "\n    " .
        showString (BS.Char8.unpack name) .
        showString "[" .
        showsPrec 0 idx .
        showString "] = " .
        showsPrec 0 value

data PropValue
    = PropByte Word8
    | PropLong Int32
    | PropBool Bool
    | PropFloat Float
    | PropObjectRef (Maybe ObjectRef)
    | PropName BS.ByteString
    | PropString BS.ByteString
    | PropVector Vector
    | PropRotator Rotator
    | PropScale Scale
    | PropOther Word8 (Maybe BS.ByteString) BS.ByteString

instance Show PropValue where
    showsPrec d (PropByte x) = showsPrec d x . showString "_u8"
    showsPrec d (PropLong x) = showsPrec d x
    showsPrec d (PropBool x) = showsPrec d x
    showsPrec d (PropFloat x) = showsPrec d x
    showsPrec _ (PropObjectRef Nothing) = showString "None"
    showsPrec d (PropObjectRef (Just ref)) = showsPrec d ref
    showsPrec _ (PropName name) = showString (BS.Char8.unpack name)
    showsPrec d (PropString x) = showsPrec d x
    showsPrec d (PropVector x) = showsPrec d x
    showsPrec d (PropRotator x) = showsPrec d x
    showsPrec d (PropScale x) = showsPrec d x
    showsPrec _ (PropOther marker mbTypeName propData) =
        showString "PropOther " .
        showsPrec 11 marker .
        showString " " .
        showsPrec 11 mbTypeName .
        showString " " .
        showsPrec 11 propData

parsePropertyList ::
    NameTable ->
    ObjectRefTable ->
    Atp.Parser (Seq Property)
parsePropertyList nameTable refTable = do
    go Empty
  where
    go buf = do
        propName <- varname nameTable
        if propName == "None"
            then pure buf
            else do
                (idx, value) <- parsePropIndexAndValue nameTable refTable
                go (buf :|> Property propName idx value)

parsePropIndexAndValue ::
    NameTable ->
    ObjectRefTable ->
    Atp.Parser (Int32, PropValue)
parsePropIndexAndValue nameTable refTable = do
    propMarker <- byte
    let propMarkerType = propMarker .&. 0xf
    let propMarkerSize = propMarker .&. 0x70
    mbPropTypeName <-
        if propMarkerType == 8 || propMarkerType == 10
            then Just <$> varname nameTable
            else pure Nothing
    let propMarkerArray = propMarker .&. 0x80 /= 0
    propSize <-
        case propMarkerSize of
            0 -> pure 1
            0x10 -> pure 2
            0x20 -> pure 4
            0x30 -> pure 12
            0x40 -> pure 16
            0x50 -> fromIntegral <$> byte
            0x60 -> fromIntegral <$> word
            _ -> fromIntegral <$> dword
    propIndex <-
        if propMarkerType /= 3 && propMarkerArray
            then do
                b0 <- byte
                if
                    | b0 .&. 0xc0 == 0xc0 -> do
                        b1 <- byte
                        w2 <- word
                        pure $
                            (fromIntegral (b0 .&. 0x3f) `shiftL` 24) .|.
                            (fromIntegral  b1 `shiftL` 16) .|.
                            fromIntegral w2
                    | b0 .&. 0x80 == 0x80 -> do
                        b1 <- byte
                        pure $
                            (fromIntegral (b0 .&. 0x7f) `shiftL` 8) .|.
                            fromIntegral b1
                    | otherwise -> do
                        pure (fromIntegral b0)
            else do
                pure 0
    substream propSize $ do
        propValue <-
            case propMarkerType of
                1 -> do
                    PropByte
                        <$> byte
                2 -> do
                    PropLong
                        <$> long
                3 -> do
                    pure $ PropBool propMarkerArray
                4 -> do
                    PropFloat
                        <$> float
                5 -> do
                    i <- varint
                    pure $ PropObjectRef (refTable A.! i)
                6 -> do
                    PropName
                        <$> varname nameTable
                10
                    | mbPropTypeName == Just "Vector" -> do
                        PropVector <$> vector
                    | mbPropTypeName == Just "Rotator" -> do
                        PropRotator <$> rotator
                    | mbPropTypeName == Just "Scale" -> do
                        PropScale <$> scale
                13 -> do
                    PropString
                        <$> varstring
                _ -> do
                    bs <- Atp.takeByteString
                    pure $ PropOther propMarker mbPropTypeName bs
        pure (propIndex, propValue)

data UnrealObject = UnrealObject
    { objectPropList :: Seq Property
    , objectData :: BS.ByteString
    }
  deriving (Show)

data ObjectData
    = LevelData Level
    | NoData
  deriving (Show)

data Level = Level
    { levelActorRefs :: Seq ObjectRef
    }
  deriving (Show)

parseUnrealObject ::
    ObjectExport ->
    NameTable ->
    ObjectRefTable ->
    Atp.Parser UnrealObject
parseUnrealObject ObjectExport {..} nameTable refTable = do
    when (eObjectFlags .&. 0x02000000 /= 0) $ do
        void varint
        void varint
        void longlong
        void long
        void varint
    props <- parsePropertyList nameTable refTable
    objectData <- Atp.takeByteString
    pure $ UnrealObject props objectData

parseLevelData ::
    NameTable ->
    ObjectRefTable ->
    Atp.Parser Level
parseLevelData _nameTable refTable = do
    void long
    listSize <- long
    go Empty listSize
  where
    go buf count
        | count <= 0 = pure $ Level buf
        | otherwise = do
            idx <- varint
            case refTable A.! idx of
                Nothing -> go buf (count - 1)
                Just ref -> go (buf :|> ref) (count - 1)

data Polys = Polys
    { polysFaces :: Seq PolysFace
    }
  deriving (Show)

parsePolysData ::
    NameTable ->
    ObjectRefTable ->
    Atp.Parser Polys
parsePolysData _nameTable refTable = do
    void long
    faceCount <- long
    faces <-
        replicateM (fromIntegral faceCount) $ do
            vertexCount <- varint
            origin <- vector
            normal <- vector
            textureU <- vector
            textureV <- vector
            vertices <- replicateM (fromIntegral vertexCount) vector
            flags <- dword
            void varint
            mbTextureRef <- (refTable A.!) <$> varint
            let textureName =
                    case mbTextureRef of
                        Nothing -> ""
                        Just ref ->
                            decodeUtf8Lenient $
                                refClassName ref <>
                                "'" <> refObjectName ref <> "'"
            void varint
            void varint
            void varint
            upan <- short
            vpan <- short
            pure $! PolysFace
                origin normal textureU textureV (Seq.fromList vertices)
                flags textureName upan vpan
    pure $ Polys $ Seq.fromList faces

data BrushModel = BrushModel
    { bmPolysRef :: Maybe ObjectRef
    }
  deriving (Show)

parseModelData ::
    NameTable ->
    ObjectRefTable ->
    Atp.Parser BrushModel
parseModelData _nameTable refTable = do
    void $ Atp.take 41
    countA <- varint
    replicateM_ (fromIntegral countA) $ do
        void $ Atp.take 12
    countB <- varint
    replicateM_ (fromIntegral countB) $ do
        void $ Atp.take 12
    countC <- varint
    replicateM_ (fromIntegral countC) $ do
        void $ Atp.take 25
        replicateM_ 10 $ void varint
        void $ Atp.take 8
    countD <- varint
    replicateM_ (fromIntegral countD) $ do
        void varint
        void $ Atp.take 4
        replicateM_ 6 $ void varint
        void $ Atp.take 4
        void varint
    countE <- varint
    replicateM_ (fromIntegral countE * 2) $ do
        void varint
    void $ Atp.take 4
    countG <- long
    replicateM_ (fromIntegral countG) $ do
        void varint
        void $ Atp.take 16
    polysRef <- (refTable A.!) <$> varint
    pure $ BrushModel polysRef

data BrushInstanceInfo = BrushInstanceInfo
    { biCsgOper :: Word8
    , biPolyFlags :: Word32
    , biPrePivot :: Vector
    , biMainScale :: Scale
    , biRotation :: Rotator
    , biPostScale :: Scale
    , biLocation :: Vector
    , biModelRef :: Maybe ObjectRef
    }
  deriving (Show)

takeBrushInstanceInfo ::
    Seq Property ->
    BrushInstanceInfo
takeBrushInstanceInfo = foldl' apply defaultValue
  where
    defaultValue = BrushInstanceInfo
        { biCsgOper = 0
        , biPolyFlags = 0
        , biPrePivot = Vector 0 0 0
        , biMainScale = Scale 1 1 1 0 SANone
        , biRotation = Rotator 0 0 0
        , biPostScale = Scale 1 1 1 0 SANone
        , biLocation = Vector 0 0 0
        , biModelRef = Nothing
        }
    apply bi (Property "CsgOper" 0 (PropByte x)) =
        bi { biCsgOper = x }
    apply bi (Property "PolyFlags" 0 (PropLong x)) =
        bi { biPolyFlags = fromIntegral x }
    apply bi (Property "PrePivot" 0 (PropVector x)) =
        bi { biPrePivot = x }
    apply bi (Property "MainScale" 0 (PropScale x)) =
        bi { biMainScale = x }
    apply bi (Property "Rotation" 0 (PropRotator x)) =
        bi { biRotation = x }
    apply bi (Property "PostScale" 0 (PropScale x)) =
        bi { biPostScale = x }
    apply bi (Property "Location" 0 (PropVector x)) =
        bi { biLocation = x }
    apply bi (Property "Brush" 0 (PropObjectRef x)) =
        bi { biModelRef = x }
    apply bi _ =
        bi

takeLong :: Int32 -> BSL.ByteString -> BSL.ByteString
takeLong i = BSL.take (fromIntegral i)

dropLong :: Int32 -> BSL.ByteString -> BSL.ByteString
dropLong i = BSL.drop (fromIntegral i)

parseIO ::
    Atp.Parser a ->
    BSL.ByteString ->
    IO a
parseIO parser source = do
    case Atp.Lazy.parse parser source of
        Atp.Lazy.Fail _ _ msg -> fail msg
        Atp.Lazy.Done _ r -> pure r

parseUnrealObjectIO ::
    ObjectExport ->
    NameTable ->
    ObjectRefTable ->
    BSL.ByteString ->
    IO UnrealObject
parseUnrealObjectIO objectExport nameTable refTable source = do
    let substr =
            takeLong (eSerialSize objectExport) $
            dropLong (eSerialOffset objectExport) $
            source
    parseIO
        (parseUnrealObject objectExport nameTable refTable)
        substr

parseStrictIO ::
    Atp.Parser a ->
    BS.ByteString ->
    IO a
parseStrictIO parser source = do
    handleResult $ Atp.parse parser source
  where
    handleResult (Atp.Fail _ _ msg) = fail msg
    handleResult (Atp.Partial cont) = handleResult $ cont ""
    handleResult (Atp.Done _ result) = pure result

findObjectExport ::
    BS.ByteString ->
    BS.ByteString ->
    ObjectRefTable ->
    Maybe ObjectExport
findObjectExport qcname qoname refTable =
    go (A.elems refTable)
  where
    go [] = Nothing
    go (Just ObjectRef {..} : others)
        | refClassName == qcname && refObjectName == qoname
        , ObjectOriginExport objectExport <- refOrigin =
            Just objectExport
        | otherwise =
            go others
    go (_ : others) =
        go others

makeInstance ::
    BS.ByteString ->
    BrushInstanceInfo ->
    NameTable ->
    ObjectRefTable ->
    BSL.ByteString -> IO (Maybe Instance)
makeInstance instanceName BrushInstanceInfo {..} nameTable refTable source = do
    if
        | biCsgOper == 1 && (biPolyFlags .&. 8 /= 0) ->
            goOper OperAddNonSolid
        | biCsgOper == 1 && (biPolyFlags .&. 0x20 /= 0) ->
            goOper OperAddSemiSolid
        | biCsgOper == 1 ->
            goOper OperAddSolid
        | biCsgOper == 2 ->
            goOper OperSubtractSolid
        | otherwise ->
            pure Nothing
  where
    goOper oper
        | Just modelRef <- biModelRef
        , refClassName modelRef == "Engine.Model"
        , ObjectOriginExport modelExport <- refOrigin modelRef = do
            modelObject <-
                parseUnrealObjectIO
                    modelExport nameTable refTable source
            modelData <-
                parseStrictIO
                    (parseModelData nameTable refTable)
                    (objectData modelObject)
            goOperAndModel oper modelData
        | otherwise = do
            pure Nothing
    goOperAndModel oper modelData
        | Just polysRef <- bmPolysRef modelData
        , refClassName polysRef == "Engine.Polys"
        , ObjectOriginExport polysExport <- refOrigin polysRef = do
            polysObject <-
                parseUnrealObjectIO
                    polysExport nameTable refTable source
            polysData <-
                parseStrictIO
                    (parsePolysData nameTable refTable)
                    (objectData polysObject)
            goOperAndPolys oper polysData
        | otherwise = do
            pure Nothing
    goOperAndPolys oper polysData = do
        pure $ Just $ Instance
            { iOper = oper
            , iTransform = InstanceTransform
                { itPrePivot = biPrePivot
                , itMainScale = biMainScale
                , itRotation = biRotation
                , itPostScale = biPostScale
                , itLocation = biLocation
                }
            , iIsSemisolid = biPolyFlags .&. 0x20 /= 0
            , iFaces =
                fmap
                    (\f -> f { fFlags = fFlags f .|. biPolyFlags })
                    (polysFaces polysData)
            , iObjectName = decodeUtf8Lenient instanceName
            }

loadGeometryFromFile :: FilePath -> IO (Seq Instance)
loadGeometryFromFile filePath = do
    source <- BSL.readFile filePath
    PakHeader {..} <- parseIO parseHeader source
    nameTable <-
        parseIO
            (parseNameTable phNameCount)
            (dropLong phNameOffset source)
    importHeaderTable <-
        parseIO
            (parseImportHeaderTable nameTable phImportCount)
            (dropLong phImportOffset source)
    exportHeaderTable <-
        parseIO
            (parseExportHeaderTable nameTable phExportCount)
            (dropLong phExportOffset source)
    refTable <- buildObjectRefTable importHeaderTable exportHeaderTable
    levelData <-
        case findObjectExport "Engine.Level" "MyLevel.MyLevel" refTable of
            Nothing -> do
                fail "Engine.Level'MyLevel.MyLevel' is not exported"
            Just objectExport -> do
                uo <-
                    parseUnrealObjectIO
                        objectExport nameTable refTable source
                parseStrictIO
                    (parseLevelData nameTable refTable)
                    (objectData uo)
    instanceListRef <- newIORef Empty
    forM_ (levelActorRefs levelData) $ \ref -> do
        if
            | refClassName ref == "Engine.Brush"
            , ObjectOriginExport objectExport <- refOrigin ref -> do
                uo <-
                    parseUnrealObjectIO
                        objectExport nameTable refTable source
                let bi = takeBrushInstanceInfo (objectPropList uo)
                mbInstance <- makeInstance
                    (refObjectName ref) bi nameTable refTable source
                case mbInstance of
                    Nothing -> pure ()
                    Just !inst -> modifyIORef' instanceListRef (:|> inst)
            | otherwise -> do
                pure ()
    readIORef instanceListRef
