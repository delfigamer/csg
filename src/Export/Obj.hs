module Export.Obj where

import Common.Types
import Control.Monad
import Geometry.Brush
import Geometry.Triangle
import Geometry.Vec3
import qualified Data.ByteString.Builder as BS.Builder
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

type Writer = (,)

tell :: w -> Writer w ()
tell w = (w, ())

exportObj ::
    Seq.Seq (Triangle (WithZoneNumbers Material)) ->
    BS.Lazy.ByteString
exportObj faceSeq =
    case writeWorld faceSeq of
        (builder, ()) ->
            BS.Builder.toLazyByteString builder

writeWorld ::
    Seq.Seq (Triangle (WithZoneNumbers Material)) ->
    Writer BS.Builder.Builder ()
writeWorld faceSeq = do
    vertexMap <- writeVertices faceSeq
    writeFaces vertexMap faceSeq

writeVertices ::
    Seq.Seq (Triangle (WithZoneNumbers Material)) ->
    Writer BS.Builder.Builder (Map.Map Vec3 Int)
writeVertices faceSeq = do
    let vertexSeq = foldMap triPoints faceSeq
    go Map.empty 1 vertexSeq
  where
    go ::
        Map.Map Vec3 Int ->
        Int ->
        Seq.Seq Vec3 ->
        Writer BS.Builder.Builder (Map.Map Vec3 Int)
    go m _ Seq.Empty = do
        pure m
    go m i (v@(Vec3 x y z) Seq.:<| rest) = do
        case Map.lookup v m of
            Nothing -> do
                tell $
                    "v " <> BS.Builder.doubleDec (fromRational x) <>
                    " " <> BS.Builder.doubleDec (fromRational y) <>
                    " " <> BS.Builder.doubleDec (fromRational z) <>
                    "\n"
                go (Map.insert v i m) (i + 1) rest
            Just _ -> do
                go m i rest

writeFaces ::
    Map.Map Vec3 Int ->
    Seq.Seq (Triangle (WithZoneNumbers Material)) ->
    Writer BS.Builder.Builder ()
writeFaces vertexMap faceSeq =
    forM_ faceSeq $ \face -> do
        tell "f"
        forM_ (triPoints face) $ \pt -> do
            case Map.lookup pt vertexMap of
                Just pti -> tell $ " " <> BS.Builder.intDec pti
                Nothing -> error "should not happen"
        tell "\n"
