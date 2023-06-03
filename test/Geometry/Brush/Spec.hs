module Geometry.Brush.Spec where

import Data.Ratio
import Data.Semigroup
import Geometry.Class
import Geometry.Brush
import Geometry.Face
import Geometry.Vec3
import Test.Hspec
import qualified Data.List as List

type TestBrush =
    Brush (WithBrushSides String) (WithSideAtInfinity ())

buildBrush ::
    [Face String] ->
    TestBrush
buildBrush =
    markBrushSides .
    executeBrushFaceSplit .
    foldr insertFace (emptyBrush ())

emptyWorld :: TestBrush
emptyWorld = Brush [] (WithSideAtInfinity Outside ())

fullWorld :: TestBrush
fullWorld = Brush [] (WithSideAtInfinity Inside ())

buildCube ::
    String ->
    Vec3 ->
    Vec3 ->
    TestBrush
buildCube namePrefix (Vec3 ax ay az) (Vec3 bx by bz) =
    buildBrush
        [ makeFace [aaa, aba, bba, baa] (namePrefix <> ".bottom")
        , makeFace [aab, bab, bbb, abb] (namePrefix <> ".top")
        , makeFace [aaa, baa, bab, aab] (namePrefix <> ".front")
        , makeFace [aba, abb, bbb, bba] (namePrefix <> ".back")
        , makeFace [aaa, aab, abb, aba] (namePrefix <> ".left")
        , makeFace [baa, bba, bbb, bab] (namePrefix <> ".right")
        ]
  where
    aaa = Vec3 ax ay az
    aab = Vec3 ax ay bz
    aba = Vec3 ax by az
    abb = Vec3 ax by bz
    baa = Vec3 bx ay az
    bab = Vec3 bx ay bz
    bba = Vec3 bx by az
    bbb = Vec3 bx by bz

buildHorizontalQuad ::
    String ->
    Vec3 ->
    Rational ->
    Rational ->
    TestBrush
buildHorizontalQuad namePrefix (Vec3 ax ay bz) bx by =
    buildBrush
        [ makeFace [aab, bab, bbb, abb] (namePrefix <> ".quad")
        ]
  where
    aab = Vec3 ax ay bz
    abb = Vec3 ax by bz
    bab = Vec3 bx ay bz
    bbb = Vec3 bx by bz

data CanonicalFace
    = CanonicalFace
        String
        CanonicalFaceSidedness
        [Vec3]
  deriving (Show, Eq, Ord)

data CanonicalFaceSidedness
    = CanonicalNoSided
    | CanonicalOneSided
    | CanonicalTwoSided
  deriving (Show, Eq, Ord)

canonicalizeFacePoints ::
    [Vec3] ->
    [Vec3]
canonicalizeFacePoints [] = []
canonicalizeFacePoints (p0 : suf0) =
    go id p0 suf0 (p0 :) suf0
  where
    go minBuf minPt minSuf buf (pt : suf) =
        if pt < minPt
            then go buf pt suf (buf . (pt :)) suf
            else go minBuf minPt minSuf (buf . (pt :)) suf
    go minBuf minPt minSuf _ [] =
        minPt : minSuf ++ minBuf []

canonicalizeFacePointsTwoSided ::
    [Vec3] ->
    [Vec3]
canonicalizeFacePointsTwoSided ps0 =
    case (canonicalizeFacePoints ps0, canonicalizeFacePoints (reverse ps0)) of
        (va@(_ : pa : _), vb@(_ : pb : _)) ->
            if pa <= pb
                then va
                else vb
        (va, _) -> va

canonicalizeFace ::
    Face (WithBrushSides String) ->
    CanonicalFace
canonicalizeFace face = do
    case faceData face of
        WithBrushSides Inside Inside fdata ->
            CanonicalFace
                fdata
                CanonicalNoSided
                (canonicalizeFacePointsTwoSided (facePoints face))
        WithBrushSides Inside Outside fdata ->
            CanonicalFace
                fdata
                CanonicalOneSided
                (canonicalizeFacePoints (facePoints face))
        WithBrushSides Outside Inside fdata ->
            CanonicalFace
                fdata
                CanonicalOneSided
                (canonicalizeFacePoints (reverse (facePoints face)))
        WithBrushSides Outside Outside fdata ->
            CanonicalFace
                fdata
                CanonicalTwoSided
                (canonicalizeFacePointsTwoSided (facePoints face))

canonicalizeBrush ::
    TestBrush ->
    [CanonicalFace]
canonicalizeBrush brush =
    List.sort (map canonicalizeFace (brushFaces brush))

spec :: Spec
spec = do
    describe "Geometry.Brush" $ do
        describe "brushOperAddSolid" $ do
            it "empty world and cube" $ do
                shouldBe
                    (canonicalizeBrush $
                            emptyWorld
                        `brushOperAddSolid`
                            (buildCube "a" (Vec3 0 0 0) (Vec3 1 1 1))
                    )
                    [ CanonicalFace
                        "a.back"
                        CanonicalOneSided
                        [Vec3 0 1 0, Vec3 0 1 1, Vec3 1 1 1, Vec3 1 1 0]
                    , CanonicalFace
                        "a.bottom"
                        CanonicalOneSided
                        [Vec3 0 0 0, Vec3 0 1 0, Vec3 1 1 0, Vec3 1 0 0]
                    , CanonicalFace
                        "a.front"
                        CanonicalOneSided
                        [Vec3 0 0 0, Vec3 1 0 0, Vec3 1 0 1, Vec3 0 0 1]
                    , CanonicalFace
                        "a.left"
                        CanonicalOneSided
                        [Vec3 0 0 0, Vec3 0 0 1, Vec3 0 1 1, Vec3 0 1 0]
                    , CanonicalFace
                        "a.right"
                        CanonicalOneSided
                        [Vec3 1 0 0, Vec3 1 1 0, Vec3 1 1 1, Vec3 1 0 1]
                    , CanonicalFace
                        "a.top"
                        CanonicalOneSided
                        [Vec3 0 0 1, Vec3 1 0 1, Vec3 1 1 1, Vec3 0 1 1]
                    ]
            it "full world and cube" $ do
                shouldBe
                    (canonicalizeBrush $
                            fullWorld
                        `brushOperAddSolid`
                            (buildCube "a" (Vec3 0 0 0) (Vec3 1 1 1))
                    )
                    []
            it "two cubes face-to-face" $ do
                shouldBe
                    (canonicalizeBrush $
                            emptyWorld
                        `brushOperAddSolid`
                            (buildCube "a" (Vec3 0 0 0) (Vec3 1 2 2))
                        `brushOperAddSolid`
                            (buildCube "b" (Vec3 1 0 0) (Vec3 2 2 2))
                    )
                    [ CanonicalFace
                        "a.back"
                        CanonicalOneSided
                        [Vec3 0 2 0, Vec3 0 2 2, Vec3 1 2 2, Vec3 1 2 0 ]
                    , CanonicalFace
                        "a.bottom"
                        CanonicalOneSided
                        [Vec3 0 0 0, Vec3 0 2 0, Vec3 1 2 0, Vec3 1 0 0 ]
                    , CanonicalFace
                        "a.front"
                        CanonicalOneSided
                        [Vec3 0 0 0, Vec3 1 0 0, Vec3 1 0 2, Vec3 0 0 2 ]
                    , CanonicalFace
                        "a.left"
                        CanonicalOneSided
                        [Vec3 0 0 0, Vec3 0 0 2, Vec3 0 2 2, Vec3 0 2 0 ]
                    , CanonicalFace
                        "a.top"
                        CanonicalOneSided
                        [Vec3 0 0 2, Vec3 1 0 2, Vec3 1 2 2, Vec3 0 2 2 ]
                    , CanonicalFace
                        "b.back"
                        CanonicalOneSided
                        [Vec3 1 2 0, Vec3 1 2 2, Vec3 2 2 2, Vec3 2 2 0 ]
                    , CanonicalFace
                        "b.bottom"
                        CanonicalOneSided
                        [Vec3 1 0 0, Vec3 1 2 0, Vec3 2 2 0, Vec3 2 0 0 ]
                    , CanonicalFace
                        "b.front"
                        CanonicalOneSided
                        [Vec3 1 0 0, Vec3 2 0 0, Vec3 2 0 2, Vec3 1 0 2 ]
                    , CanonicalFace
                        "b.right"
                        CanonicalOneSided
                        [Vec3 2 0 0, Vec3 2 2 0, Vec3 2 2 2, Vec3 2 0 2 ]
                    , CanonicalFace
                        "b.top"
                        CanonicalOneSided
                        [Vec3 1 0 2, Vec3 2 0 2, Vec3 2 2 2, Vec3 1 2 2 ]
                    ]
            it "two cubes face-to-face shifted" $ do
                shouldBe
                    (canonicalizeBrush $
                            emptyWorld
                        `brushOperAddSolid`
                            (buildCube "a" (Vec3 0 0 0) (Vec3 1 2 2))
                        `brushOperAddSolid`
                            (buildCube "b" (Vec3 1 0 1) (Vec3 2 2 3))
                    )
                    [ CanonicalFace
                        "a.back"
                        CanonicalOneSided
                        [Vec3 0 2 0, Vec3 0 2 2, Vec3 1 2 2, Vec3 1 2 1,
                            Vec3 1 2 0]
                    , CanonicalFace
                        "a.bottom"
                        CanonicalOneSided
                        [Vec3 0 0 0, Vec3 0 2 0, Vec3 1 2 0, Vec3 1 0 0]
                    , CanonicalFace
                        "a.front"
                        CanonicalOneSided
                        [Vec3 0 0 0, Vec3 1 0 0, Vec3 1 0 1, Vec3 1 0 2,
                            Vec3 0 0 2]
                    , CanonicalFace
                        "a.left"
                        CanonicalOneSided
                        [Vec3 0 0 0, Vec3 0 0 2, Vec3 0 2 2, Vec3 0 2 0]
                    , CanonicalFace
                        "a.right"
                        CanonicalOneSided
                        [Vec3 1 0 0, Vec3 1 2 0, Vec3 1 2 1, Vec3 1 0 1]
                    , CanonicalFace
                        "a.top"
                        CanonicalOneSided
                        [Vec3 0 0 2, Vec3 1 0 2, Vec3 1 2 2, Vec3 0 2 2]
                    , CanonicalFace
                        "b.back"
                        CanonicalOneSided
                        [Vec3 1 2 1, Vec3 1 2 2, Vec3 2 2 3, Vec3 2 2 1]
                    , CanonicalFace
                        "b.back"
                        CanonicalOneSided
                        [Vec3 1 2 2, Vec3 1 2 3, Vec3 2 2 3]
                    , CanonicalFace
                        "b.bottom"
                        CanonicalOneSided
                        [Vec3 1 0 1, Vec3 1 2 1, Vec3 2 2 1, Vec3 2 0 1]
                    , CanonicalFace
                        "b.front"
                        CanonicalOneSided
                        [Vec3 1 0 1, Vec3 2 0 1, Vec3 2 0 3, Vec3 1 0 2]
                    , CanonicalFace
                        "b.front"
                        CanonicalOneSided
                        [Vec3 1 0 2, Vec3 2 0 3, Vec3 1 0 3]
                    , CanonicalFace
                        "b.left"
                        CanonicalOneSided
                        [Vec3 1 0 2, Vec3 1 0 3, Vec3 1 2 3, Vec3 1 2 2]
                    , CanonicalFace
                        "b.right"
                        CanonicalOneSided
                        [Vec3 2 0 1, Vec3 2 2 1, Vec3 2 2 3, Vec3 2 0 3]
                    , CanonicalFace
                        "b.top"
                        CanonicalOneSided
                        [Vec3 1 0 3, Vec3 2 0 3, Vec3 2 2 3, Vec3 1 2 3]
                    ]
            it "two cubes intersecting" $ do
                shouldBe
                    (canonicalizeBrush $
                            emptyWorld
                        `brushOperAddSolid`
                            (buildCube "a" (Vec3 0 0 0) (Vec3 2 2 2))
                        `brushOperAddSolid`
                            (buildCube "b" (Vec3 1 0 1) (Vec3 3 2 3))
                    )
                    [ CanonicalFace
                        "a.back"
                        CanonicalOneSided
                        [Vec3 0 2 0, Vec3 0 2 2, Vec3 1 2 2, Vec3 1 2 1]
                    , CanonicalFace
                        "a.back"
                        CanonicalOneSided
                        [Vec3 0 2 0, Vec3 1 2 1, Vec3 2 2 1, Vec3 2 2 0]
                    , CanonicalFace
                        "a.bottom"
                        CanonicalOneSided
                        [Vec3 0 0 0, Vec3 0 2 0, Vec3 2 2 0, Vec3 2 0 0]
                    , CanonicalFace
                        "a.front"
                        CanonicalOneSided
                        [Vec3 0 0 0, Vec3 1 0 1, Vec3 1 0 2, Vec3 0 0 2]
                    , CanonicalFace
                        "a.front"
                        CanonicalOneSided
                        [Vec3 0 0 0, Vec3 2 0 0, Vec3 2 0 1, Vec3 1 0 1]
                    , CanonicalFace
                        "a.left"
                        CanonicalOneSided
                        [Vec3 0 0 0, Vec3 0 0 2, Vec3 0 2 2, Vec3 0 2 0]
                    , CanonicalFace
                        "a.right"
                        CanonicalOneSided
                        [Vec3 2 0 0, Vec3 2 2 0, Vec3 2 2 1, Vec3 2 0 1]
                    , CanonicalFace
                        "a.top"
                        CanonicalOneSided
                        [Vec3 0 0 2, Vec3 1 0 2, Vec3 1 2 2, Vec3 0 2 2]
                    , CanonicalFace
                        "b.back"
                        CanonicalOneSided
                        [Vec3 1 2 1, Vec3 1 2 2, Vec3 2 2 2, Vec3 2 2 1]
                    , CanonicalFace
                        "b.back"
                        CanonicalOneSided
                        [Vec3 1 2 2, Vec3 1 2 3, Vec3 3 2 3, Vec3 2 2 2]
                    , CanonicalFace
                        "b.back"
                        CanonicalOneSided
                        [Vec3 2 2 1, Vec3 2 2 2, Vec3 3 2 3, Vec3 3 2 1]
                    , CanonicalFace
                        "b.bottom"
                        CanonicalOneSided
                        [Vec3 2 0 1, Vec3 2 2 1, Vec3 3 2 1, Vec3 3 0 1]
                    , CanonicalFace
                        "b.front"
                        CanonicalOneSided
                        [Vec3 1 0 1, Vec3 2 0 1, Vec3 2 0 2, Vec3 1 0 2]
                    , CanonicalFace
                        "b.front"
                        CanonicalOneSided
                        [Vec3 1 0 2, Vec3 2 0 2, Vec3 3 0 3, Vec3 1 0 3]
                    , CanonicalFace
                        "b.front"
                        CanonicalOneSided
                        [Vec3 2 0 1, Vec3 3 0 1, Vec3 3 0 3, Vec3 2 0 2]
                    , CanonicalFace
                        "b.left"
                        CanonicalOneSided
                        [Vec3 1 0 2, Vec3 1 0 3, Vec3 1 2 3, Vec3 1 2 2]
                    , CanonicalFace
                        "b.right"
                        CanonicalOneSided
                        [Vec3 3 0 1, Vec3 3 2 1, Vec3 3 2 3, Vec3 3 0 3]
                    , CanonicalFace
                        "b.top"
                        CanonicalOneSided
                        [Vec3 1 0 3, Vec3 3 0 3, Vec3 3 2 3, Vec3 1 2 3]
                    ]
            it "negative cube and cube face-to-face" $ do
                shouldBe
                    (canonicalizeBrush $
                            fullWorld
                        `brushOperSubtractSolid`
                            (buildCube "a" (Vec3 0 0 0) (Vec3 2 2 2))
                        `brushOperAddSolid`
                            (buildCube "b" (Vec3 2 0 0) (Vec3 4 2 2))
                    )
                    [ CanonicalFace
                        "a.back"
                        CanonicalOneSided
                        [Vec3 0 2 0, Vec3 2 2 0, Vec3 2 2 2, Vec3 0 2 2]
                    , CanonicalFace
                        "a.bottom"
                        CanonicalOneSided
                        [Vec3 0 0 0, Vec3 2 0 0, Vec3 2 2 0, Vec3 0 2 0]
                    , CanonicalFace
                        "a.front"
                        CanonicalOneSided
                        [Vec3 0 0 0, Vec3 0 0 2, Vec3 2 0 2, Vec3 2 0 0]
                    , CanonicalFace
                        "a.left"
                        CanonicalOneSided
                        [Vec3 0 0 0, Vec3 0 2 0, Vec3 0 2 2, Vec3 0 0 2]
                    , CanonicalFace
                        "a.top"
                        CanonicalOneSided
                        [Vec3 0 0 2, Vec3 0 2 2, Vec3 2 2 2, Vec3 2 0 2]
                    , CanonicalFace
                        "b.left"
                        CanonicalOneSided
                        [Vec3 2 0 0, Vec3 2 0 2, Vec3 2 2 2, Vec3 2 2 0]
                    ]
            it "negative cube and cube face-to-face shifted" $ do
                shouldBe
                    (canonicalizeBrush $
                            fullWorld
                        `brushOperSubtractSolid`
                            (buildCube "a" (Vec3 0 0 0) (Vec3 2 2 2))
                        `brushOperAddSolid`
                            (buildCube "b" (Vec3 2 0 1) (Vec3 4 2 3))
                    )
                    [ CanonicalFace
                        "a.back"
                        CanonicalOneSided
                        [Vec3 0 2 0, Vec3 2 2 0, Vec3 2 2 1, Vec3 2 2 2,
                            Vec3 0 2 2]
                    , CanonicalFace
                        "a.bottom"
                        CanonicalOneSided
                        [Vec3 0 0 0, Vec3 2 0 0, Vec3 2 2 0, Vec3 0 2 0]
                    , CanonicalFace
                        "a.front"
                        CanonicalOneSided
                        [Vec3 0 0 0, Vec3 0 0 2, Vec3 2 0 2, Vec3 2 0 1,
                            Vec3 2 0 0]
                    , CanonicalFace
                        "a.left"
                        CanonicalOneSided
                        [Vec3 0 0 0, Vec3 0 2 0, Vec3 0 2 2, Vec3 0 0 2]
                    , CanonicalFace
                        "a.right"
                        CanonicalOneSided
                        [Vec3 2 0 0, Vec3 2 0 1, Vec3 2 2 1, Vec3 2 2 0]
                    , CanonicalFace
                        "a.top"
                        CanonicalOneSided
                        [Vec3 0 0 2, Vec3 0 2 2, Vec3 2 2 2, Vec3 2 0 2]
                    , CanonicalFace
                        "b.left"
                        CanonicalOneSided
                        [Vec3 2 0 1, Vec3 2 0 2, Vec3 2 2 2, Vec3 2 2 1]
                    ]
            it "negative cube and cube intersecting" $ do
                shouldBe
                    (canonicalizeBrush $
                            fullWorld
                        `brushOperSubtractSolid`
                            (buildCube "a" (Vec3 0 0 0) (Vec3 2 2 2))
                        `brushOperAddSolid`
                            (buildCube "b" (Vec3 1 0 1) (Vec3 3 2 3))
                    )
                    [ CanonicalFace
                        "a.back"
                        CanonicalOneSided
                        [Vec3 0 2 0, Vec3 1 2 1, Vec3 1 2 2, Vec3 0 2 2]
                    , CanonicalFace
                        "a.back"
                        CanonicalOneSided
                        [Vec3 0 2 0, Vec3 2 2 0, Vec3 2 2 1, Vec3 1 2 1]
                    , CanonicalFace
                        "a.bottom"
                        CanonicalOneSided
                        [Vec3 0 0 0, Vec3 2 0 0, Vec3 2 2 0, Vec3 0 2 0]
                    , CanonicalFace
                        "a.front"
                        CanonicalOneSided
                        [Vec3 0 0 0, Vec3 0 0 2, Vec3 1 0 2, Vec3 1 0 1]
                    , CanonicalFace
                        "a.front"
                        CanonicalOneSided
                        [Vec3 0 0 0, Vec3 1 0 1, Vec3 2 0 1, Vec3 2 0 0]
                    , CanonicalFace
                        "a.left"
                        CanonicalOneSided
                        [Vec3 0 0 0, Vec3 0 2 0, Vec3 0 2 2, Vec3 0 0 2]
                    , CanonicalFace
                        "a.right"
                        CanonicalOneSided
                        [Vec3 2 0 0, Vec3 2 0 1, Vec3 2 2 1, Vec3 2 2 0]
                    , CanonicalFace
                        "a.top"
                        CanonicalOneSided
                        [Vec3 0 0 2, Vec3 0 2 2, Vec3 1 2 2, Vec3 1 0 2]
                    , CanonicalFace
                        "b.bottom"
                        CanonicalOneSided
                        [Vec3 1 0 1, Vec3 1 2 1, Vec3 2 2 1, Vec3 2 0 1]
                    , CanonicalFace
                        "b.left"
                        CanonicalOneSided
                        [Vec3 1 0 1, Vec3 1 0 2, Vec3 1 2 2, Vec3 1 2 1]
                    ]
            it "empty world and free quad" $ do
                shouldBe
                    (canonicalizeBrush $
                            emptyWorld
                        `brushOperAddSolid`
                            (buildHorizontalQuad "q" (Vec3 0 0 0) 2 2)
                    )
                    [ CanonicalFace
                        "q.quad"
                        CanonicalTwoSided
                        [Vec3 0 0 0, Vec3 0 2 0, Vec3 2 2 0, Vec3 2 0 0]
                    ]
            it "full world and free quad" $ do
                shouldBe
                    (canonicalizeBrush $
                            fullWorld
                        `brushOperAddSolid`
                            (buildHorizontalQuad "q" (Vec3 0 0 0) 2 2)
                    )
                    []
            it "free quad and cube face-aligned" $ do
                shouldBe
                    (canonicalizeBrush $
                            emptyWorld
                        `brushOperAddSolid`
                            (buildHorizontalQuad "q" (Vec3 0 0 2) 2 2)
                        `brushOperAddSolid`
                            (buildCube "b" (Vec3 1 0 0) (Vec3 3 2 2))
                    )
                    [ CanonicalFace
                        "b.back"
                        CanonicalOneSided
                        [Vec3 1 2 0, Vec3 1 2 2, Vec3 2 2 2, Vec3 3 2 2,
                            Vec3 3 2 0]
                    , CanonicalFace
                        "b.bottom"
                        CanonicalOneSided
                        [Vec3 1 0 0, Vec3 1 2 0, Vec3 3 2 0, Vec3 3 0 0]
                    , CanonicalFace
                        "b.front"
                        CanonicalOneSided
                        [Vec3 1 0 0, Vec3 3 0 0, Vec3 3 0 2, Vec3 2 0 2,
                            Vec3 1 0 2]
                    , CanonicalFace
                        "b.left"
                        CanonicalOneSided
                        [Vec3 1 0 0, Vec3 1 0 2, Vec3 1 2 2, Vec3 1 2 0]
                    , CanonicalFace
                        "b.right"
                        CanonicalOneSided
                        [Vec3 3 0 0, Vec3 3 2 0, Vec3 3 2 2, Vec3 3 0 2]
                    , CanonicalFace
                        "b.top"
                        CanonicalOneSided
                        [Vec3 1 0 2, Vec3 2 0 2, Vec3 2 2 2, Vec3 1 2 2]
                    , CanonicalFace
                        "b.top"
                        CanonicalOneSided
                        [Vec3 2 0 2, Vec3 3 0 2, Vec3 3 2 2, Vec3 2 2 2]
                    , CanonicalFace
                        "q.quad"
                        CanonicalTwoSided
                        [Vec3 0 0 2, Vec3 0 2 2, Vec3 1 2 2, Vec3 1 0 2]
                    ]
        describe "brushOperSubtractSolid" $ do
            it "empty world and cube" $ do
                shouldBe
                    (canonicalizeBrush $
                            emptyWorld
                        `brushOperSubtractSolid`
                            (buildCube "a" (Vec3 0 0 0) (Vec3 1 1 1))
                    )
                    []
            it "full world and cube" $ do
                shouldBe
                    (canonicalizeBrush $
                            fullWorld
                        `brushOperSubtractSolid`
                            (buildCube "a" (Vec3 0 0 0) (Vec3 1 1 1))
                    )
                    [ CanonicalFace
                        "a.back"
                        CanonicalOneSided
                        [Vec3 0 1 0, Vec3 1 1 0, Vec3 1 1 1, Vec3 0 1 1]
                    , CanonicalFace
                        "a.bottom"
                        CanonicalOneSided
                        [Vec3 0 0 0, Vec3 1 0 0, Vec3 1 1 0, Vec3 0 1 0]
                    , CanonicalFace
                        "a.front"
                        CanonicalOneSided
                        [Vec3 0 0 0, Vec3 0 0 1, Vec3 1 0 1, Vec3 1 0 0]
                    , CanonicalFace
                        "a.left"
                        CanonicalOneSided
                        [Vec3 0 0 0, Vec3 0 1 0, Vec3 0 1 1, Vec3 0 0 1]
                    , CanonicalFace
                        "a.right"
                        CanonicalOneSided
                        [Vec3 1 0 0, Vec3 1 0 1, Vec3 1 1 1, Vec3 1 1 0]
                    , CanonicalFace
                        "a.top"
                        CanonicalOneSided
                        [Vec3 0 0 1, Vec3 0 1 1, Vec3 1 1 1, Vec3 1 0 1]
                    ]
