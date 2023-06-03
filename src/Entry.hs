module Entry where

import Common.Types
import Control.Applicative
import Control.Monad
import Export.Bvh
import Export.Obj
import Geometry.Brush
import Geometry.PlaneSpace
import Geometry.Triangle
import Data.Type.Equality
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import qualified Options.Applicative as OA
import qualified Pak.Importer
import qualified Pak.Parser
import qualified Pak.Polys

data Options = Options
    { optInputPath :: FilePath
    , optOutputs :: [FilePath]
    }
    deriving (Show)

optionsParserInfo :: OA.ParserInfo Options
optionsParserInfo =
    OA.info
        (Options
            <$> inputPathParser
            <*> outputsParser
        )
        (OA.progDesc "Constructive solid geometry builder")
  where
    inputPathParser =
        OA.strArgument
            (   OA.metavar "<input file>"
            )
    outputsParser =
        many $
            OA.strArgument
                (   OA.metavar "<output file base path>"
                )

entryMain :: IO ()
entryMain = do
    options <- OA.execParser optionsParserInfo
    processPlan <- constructProcessPlan options
    printProcessPlan processPlan
    executeProcessPlan processPlan

stripSuffix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripSuffix s0 x0 =
    go (List.reverse s0) (List.reverse x0)
  where
    go (p : ps) (x : xs)
        | p == x =
            go ps xs
        | otherwise =
            Nothing
    go [] xs =
        Just $ List.reverse xs
    go _ [] =
        Nothing

parseFilePath :: FilePath -> Maybe (SomeContentTag, FilePath)
parseFilePath path
    | Just base <- stripSuffix ".unr" path =
        Just (SomeContentTag TagUnrContent, base)
    | Just base <- stripSuffix ".solid.yaml" path =
        Just (SomeContentTag TagSolidContent, base)
    | Just base <- stripSuffix ".zoned.yaml" path =
        Just (SomeContentTag TagZonedContent, base)
    | Just base <- stripSuffix ".triag.yaml" path =
        Just (SomeContentTag TagTriangulatedContent, base)
    | Just base <- stripSuffix ".obj" path =
        Just (SomeContentTag TagObjContent, base)
    | Just base <- stripSuffix ".bvh" path =
        Just (SomeContentTag TagBvhContent, base)
    | otherwise =
        Nothing

constructProcessPlan :: Options -> IO ProcessPlan
constructProcessPlan (Options inputPath outputPathList) = do
    (SomeContentTag inputTag, inputPathBase) <-
        maybe (fail $ "unknown extension for input " <> inputPath) pure $
            parseFilePath inputPath
    inputFunc <-
        maybe (fail $ "input format unsupported " <> inputPath) pure $
            contentInputFunc inputPath inputTag
    buildPlan <-
        case outputPathList of
            [] -> constructDefaultBuildPlan inputPathBase inputTag
            _ -> constructUserBuildPlan outputPathList inputTag
    pure $ ProcessPlan inputFunc buildPlan

constructUserBuildPlan :: [FilePath] -> ContentTag a -> IO (BuildPlan a)
constructUserBuildPlan outputPathList inputTag = do
    foldM appendOutputPath (BuildPlan mempty []) outputPathList
  where
    appendOutputPath buildPlan outputPath = do
        (SomeContentTag outputTag, _) <-
            maybe (fail $ "unknown extension for output " <> outputPath) pure $
                parseFilePath outputPath
        outputFunc <-
            maybe (fail $ "output format unsupported " <> outputPath) pure $
                contentOutputFunc outputPath outputTag
        maybe (fail $ "output not available from input " <> outputPath) pure $
            insertOutputFunc outputTag outputFunc inputTag buildPlan

constructDefaultBuildPlan :: FilePath -> ContentTag a -> IO (BuildPlan a)
constructDefaultBuildPlan basePath = \case
    TagUnrContent ->
        pure $ BuildPlan mempty [xformStageSolid]
    TagSolidContent ->
        pure $ BuildPlan mempty [xformStageZoned]
    TagZonedContent ->
        pure $ BuildPlan mempty [xformStageTriangulated]
    TagTriangulatedContent ->
        pure $ BuildPlan mempty [xformStageObj, xformStageBvh]
    _ -> fail "unsupported input format"
  where
    xformStageSolid =
        TransformStage TagSolidContent processUnrToSolid $
            BuildPlan
                (saveSolid $ basePath <> ".solid.yaml")
                [xformStageZoned]
    xformStageZoned =
        TransformStage TagZonedContent processSolidToZoned $
            BuildPlan
                (saveZoned $ basePath <> ".zoned.yaml")
                [xformStageTriangulated]
    xformStageTriangulated =
        TransformStage TagTriangulatedContent processZonedToTriangulated $
            BuildPlan
                (saveTriangulated $ basePath <> ".triag.yaml")
                [xformStageObj, xformStageBvh]
    xformStageObj =
        TransformStage TagObjContent processTriangulatedToObj $
            BuildPlan
                (saveObj $ basePath <> ".obj")
                []
    xformStageBvh =
        TransformStage TagBvhContent processTriangulatedToBvh $
            BuildPlan
                (saveBvh $ basePath <> ".bvh")
                []

data ProcessPlan
    = forall a.
    ProcessPlan
        (InputFunc a)
        (BuildPlan a)

instance Show ProcessPlan where
    showsPrec d (ProcessPlan input output) =
        showParen (d > 10) $
            showString "ProcessPlan " .
            showsPrec 11 input .
            showString " " .
            showsPrec 11 output

printProcessPlan ::
    ProcessPlan ->
    IO ()
printProcessPlan (ProcessPlan input output) = do
    putStrLn "Process plan:"
    putStrLn $ "Input " <> inputFuncName input
    printBuildPlan output

executeProcessPlan ::
    ProcessPlan ->
    IO ()
executeProcessPlan (ProcessPlan input output) = do
    x <- runInputFunc input
    executeBuildPlan x output

type UnrContent =
    Seq.Seq Pak.Polys.Instance

type SolidContent =
    Brush (WithBrushSides Material) (WithSideAtInfinity ())

type ZonedContent =
    Brush (WithZoneNumbers Material) (WithZoneAtInfinity ())

type TriangulatedContent =
    (WithZoneAtInfinity (), Seq.Seq (Triangle (WithZoneNumbers Material)))

type ObjContent =
    BS.Lazy.ByteString

type BvhContent =
    BS.Lazy.ByteString

data ContentTag a where
    TagUnrContent :: ContentTag UnrContent
    TagSolidContent :: ContentTag SolidContent
    TagZonedContent :: ContentTag ZonedContent
    TagTriangulatedContent :: ContentTag TriangulatedContent
    TagObjContent :: ContentTag ObjContent
    TagBvhContent :: ContentTag BvhContent

deriving instance Show (ContentTag a)

instance TestEquality ContentTag where
    testEquality TagUnrContent TagUnrContent = Just Refl
    testEquality TagSolidContent TagSolidContent = Just Refl
    testEquality TagZonedContent TagZonedContent = Just Refl
    testEquality TagTriangulatedContent TagTriangulatedContent = Just Refl
    testEquality TagObjContent TagObjContent = Just Refl
    testEquality TagBvhContent TagBvhContent = Just Refl
    testEquality _ _ = Nothing

data SomeContentTag = forall a. SomeContentTag (ContentTag a)

data TransformFunc a b
    = TransformFunc
        { transformFuncName :: String
        , runTransformFunc :: a -> IO b
        }

instance Show (TransformFunc a b) where
    showsPrec _ (TransformFunc fname _) =
        showString "<TransformFunc " .
        showString fname .
        showString ">"

data PrerequisiteTransform a
    = forall b.
        PrerequisiteTransform
            (ContentTag b)
            (TransformFunc b a)

instance Show (PrerequisiteTransform a) where
    showsPrec d (PrerequisiteTransform ctag func) =
        showParen (d > 10) $
            showString "PrerequisiteTransform " .
            showsPrec 11 ctag .
            showString " " .
            showsPrec 11 func

data InputFunc a
    = InputFunc
        { inputFuncName :: String
        , runInputFunc :: IO a
        }

instance Show (InputFunc a) where
    showsPrec _ (InputFunc name _) =
        showString "<InputFunc " .
        showsPrec 11 name .
        showString ">"

contentInputFunc ::
    FilePath ->
    ContentTag a ->
    Maybe (InputFunc a)
contentInputFunc filePath = \case
    TagUnrContent -> Just $ loadUnr filePath
    TagSolidContent -> Just $ loadSolid filePath
    TagZonedContent -> Just $ loadZoned filePath
    TagTriangulatedContent -> Just $ loadTriangulated filePath
    TagObjContent -> Nothing
    TagBvhContent -> Nothing

prerequisiteTransform ::
    ContentTag a ->
    Maybe (PrerequisiteTransform a)
prerequisiteTransform = \case
    TagUnrContent -> Nothing
    TagSolidContent ->
        Just $
            PrerequisiteTransform
                TagUnrContent
                processUnrToSolid
    TagZonedContent ->
        Just $
            PrerequisiteTransform
                TagSolidContent
                processSolidToZoned
    TagTriangulatedContent ->
        Just $
            PrerequisiteTransform
                TagZonedContent
                processZonedToTriangulated
    TagObjContent ->
        Just $
            PrerequisiteTransform
                TagTriangulatedContent
                processTriangulatedToObj
    TagBvhContent ->
        Just $
            PrerequisiteTransform
                TagTriangulatedContent
                processTriangulatedToBvh

contentOutputFunc ::
    FilePath ->
    ContentTag a ->
    Maybe (OutputFunc a)
contentOutputFunc filePath = \case
    TagUnrContent -> Nothing
    TagSolidContent -> Just $ saveSolid filePath
    TagZonedContent -> Just $ saveZoned filePath
    TagTriangulatedContent -> Just $ saveTriangulated filePath
    TagObjContent -> Just $ saveObj filePath
    TagBvhContent -> Just $ saveBvh filePath

data OutputFunc a
    = OutputFunc
        { outputFuncNames :: [String]
        , runOutputFunc :: a -> IO ()
        }

instance Show (OutputFunc a) where
    showsPrec _ (OutputFunc names _) =
        showString "<OutputFunc " .
        showsPrec 11 names .
        showString ">"

instance Semigroup (OutputFunc a) where
    OutputFunc na sta <> OutputFunc nb stb =
        OutputFunc (na <> nb) (sta >> stb)

instance Monoid (OutputFunc a) where
    mempty = OutputFunc [] (\_ -> pure ())

data TransformStage a
    = forall b.
    TransformStage
        (ContentTag b)
        (TransformFunc a b)
        (BuildPlan b)

instance Show (TransformStage a) where
    showsPrec d (TransformStage ctag func bplan) =
        showParen (d > 10) $
            showString "TransformStage " .
            showsPrec 11 ctag .
            showString " " .
            showsPrec 11 func .
            showString " " .
            showsPrec 11 bplan

data BuildPlan a =
    BuildPlan
        (OutputFunc a)
        [TransformStage a]

instance Show (BuildPlan a) where
    showsPrec d (BuildPlan outstage xformlist) =
        showParen (d > 10) $
            showString "BuildPlan " .
            showsPrec 11 outstage .
            showString " " .
            showsPrec 11 xformlist

prependOutputFunc ::
    OutputFunc a ->
    BuildPlan a ->
    BuildPlan a
prependOutputFunc stage (BuildPlan outputFunc xformStageList) =
    BuildPlan (stage <> outputFunc) xformStageList

setTransformStage ::
    ContentTag b ->
    TransformFunc a b ->
    (Maybe (BuildPlan b) -> BuildPlan b) ->
    [TransformStage a] ->
    [TransformStage a]
setTransformStage outputTag xform userFn (xformStage : rest) =
    case xformStage of
        TransformStage stageOutputTag oldXform buildPlan ->
            case testEquality outputTag stageOutputTag of
                Just Refl -> do
                    let newXformStage =
                            TransformStage
                                stageOutputTag
                                oldXform
                                (userFn $ Just buildPlan)
                    newXformStage : rest
                Nothing ->
                    xformStage : setTransformStage outputTag xform userFn rest
setTransformStage outputTag xform userFn [] =
    [TransformStage outputTag xform (userFn Nothing)]

printBuildPlan ::
    BuildPlan a ->
    IO ()
printBuildPlan (BuildPlan outputFunc xformStageList) = do
    forM_ (outputFuncNames outputFunc) $ \name -> do
        putStrLn $ "Output " <> name
    forM_ xformStageList $ \(TransformStage _ xform nextBuildPlan) -> do
        putStrLn $ "Transform " <> transformFuncName xform
        printBuildPlan nextBuildPlan

executeBuildPlan ::
    a ->
    BuildPlan a ->
    IO ()
executeBuildPlan a (BuildPlan outputFunc xformStageList) = do
    runOutputFunc outputFunc a
    forM_ xformStageList $ \(TransformStage _ xform nextBuildPlan) -> do
        b <- runTransformFunc xform a
        executeBuildPlan b nextBuildPlan

trySetStage ::
    ContentTag b ->
    ContentTag a ->
    BuildPlan a ->
    Maybe ((Maybe (BuildPlan b) -> BuildPlan b) -> BuildPlan a)
trySetStage requestedTag sourceTag sourcePlan
    | Just Refl <- testEquality requestedTag sourceTag =
        Just $ \userFn -> userFn $ Just sourcePlan
    | Just (PrerequisiteTransform preqTag xform) <- prerequisiteTransform requestedTag
    , Just builderFn <- trySetStage preqTag sourceTag sourcePlan =
            Just $ \userFn ->
                builderFn $ \case
                    Nothing ->
                        BuildPlan
                            mempty
                            [TransformStage requestedTag xform (userFn Nothing)]
                    Just (BuildPlan preqOutputFunc preqXformStageList) ->
                        BuildPlan
                            preqOutputFunc
                            (setTransformStage
                                requestedTag
                                xform
                                userFn
                                preqXformStageList
                            )
    | otherwise =
        Nothing

insertOutputFunc ::
    ContentTag b ->
    OutputFunc b ->
    ContentTag a ->
    BuildPlan a ->
    Maybe (BuildPlan a)
insertOutputFunc requestedTag requestedOutputFunc sourceTag sourcePlan = do
    builderFn <- trySetStage requestedTag sourceTag sourcePlan
    Just $ builderFn $ \case
        Nothing -> BuildPlan requestedOutputFunc []
        Just oldBuildPlan -> prependOutputFunc requestedOutputFunc oldBuildPlan

loadUnr :: FilePath -> InputFunc UnrContent
loadUnr path =
    InputFunc
        ("loadUnr " <> show path)
        (Pak.Parser.loadGeometryFromFile path)

loadSolid :: FilePath -> InputFunc SolidContent
loadSolid path =
    InputFunc
        ("loadSolid " <> show path)
        (Yaml.decodeFileThrow path)

loadZoned :: FilePath -> InputFunc ZonedContent
loadZoned path =
    InputFunc
        ("loadZoned " <> show path)
        (Yaml.decodeFileThrow path)

loadTriangulated :: FilePath -> InputFunc TriangulatedContent
loadTriangulated path =
    InputFunc
        ("loadTriangulated " <> show path)
        (Yaml.decodeFileThrow path)

saveSolid :: FilePath -> OutputFunc SolidContent
saveSolid path =
    OutputFunc
        ["saveSolid " <> show path]
        (Yaml.encodeFile path)

saveZoned :: FilePath -> OutputFunc ZonedContent
saveZoned path =
    OutputFunc
        ["saveZoned " <> show path]
        (Yaml.encodeFile path)

saveTriangulated :: FilePath -> OutputFunc TriangulatedContent
saveTriangulated path =
    OutputFunc
        ["saveTriangulated " <> show path]
        (Yaml.encodeFile path)

saveObj :: FilePath -> OutputFunc ObjContent
saveObj path =
    OutputFunc
        ["saveObj " <> show path]
        (BS.Lazy.writeFile path)

saveBvh :: FilePath -> OutputFunc BvhContent
saveBvh path =
    OutputFunc
        ["saveBvh " <> show path]
        (BS.Lazy.writeFile path)

processUnrToSolid :: TransformFunc UnrContent SolidContent
processUnrToSolid =
    TransformFunc "processUnrToSolid" $ \instList -> do
        Pak.Importer.importWorldToFull
            (\event -> do
                putStrLn $
                    "CSG " ++ show (Pak.Importer.ieCurrentNumber event) ++
                    "/" ++ show (Pak.Importer.ieTotalNumber event) ++
                    "\t" ++ Text.unpack (Pak.Importer.ieObjectName event)
            )
            instList

processSolidToZoned :: TransformFunc SolidContent ZonedContent
processSolidToZoned =
    TransformFunc "processSolidToZoned" $ \solidBrush -> do
        pure $! markBrushZones $ stripBrushSides solidBrush

processZonedToTriangulated :: TransformFunc ZonedContent TriangulatedContent
processZonedToTriangulated =
    TransformFunc "processZonedToTriangulated" $ \zonedBrush -> do
        triSeq <-
            triangulateBrush
                (\i planeCount ps -> do
                    putStrLn $
                        "Triangulate " ++ show i ++ "/" ++ show planeCount ++
                        "\t" ++ show (planeSpaceHalfSpace ps)
                )
                zonedBrush
        pure (brushData zonedBrush, triSeq)

processTriangulatedToObj :: TransformFunc TriangulatedContent ObjContent
processTriangulatedToObj =
    TransformFunc "processTriangulatedToObj" $ \(_, zonedFaceSeq) -> do
        pure $! exportObj zonedFaceSeq

processTriangulatedToBvh :: TransformFunc TriangulatedContent BvhContent
processTriangulatedToBvh =
    TransformFunc "processTriangulatedToBvh" $ \(bdata, zonedFaceSeq) -> do
        pure $! exportBvh bdata zonedFaceSeq
