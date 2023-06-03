module Geometry.PlaneSpace where

import Control.Exception
import Data.Ratio
import Geometry.Class
import Geometry.Vec2
import Geometry.Vec3
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson

data PlaneSpace = PlaneSpace
    { planeSpaceUDir :: {-# UNPACK #-} !IVec3
    , planeSpaceUDirSqr :: !Integer
    , planeSpaceVDir :: {-# UNPACK #-} !IVec3
    , planeSpaceVDirSqr :: !Integer
    , planeSpaceHalfSpace :: {-# UNPACK #-} !HalfSpace
    , planeSpaceOrigin :: {-# UNPACK #-} !Vec3
    , planeSpaceIntersectionYDir :: {-# UNPACK #-} !Vec2
    , planeSpaceIntersectionZDir :: {-# UNPACK #-} !Vec2
    }
    deriving (Show, Eq, Ord)

Aeson.deriveJSON Aeson.defaultOptions ''PlaneSpace

planeSpaceNormal :: PlaneSpace -> IVec3
planeSpaceNormal = halfSpaceNormal . planeSpaceHalfSpace

planeSpace ::
    IVec3 ->
    IVec3 ->
    HalfSpace ->
    PlaneSpace
planeSpace udir@(IVec3 _ yu zu) vdir@(IVec3 xv yv zv) hs@(HalfSpace norm offset) =
    assert (dot udir vdir == 0) $
    assert (dot udir norm == 0) $
    assert (dot vdir norm == 0) $
    assert (det3i udir vdir norm > 0) $
    assert (xv == 0) $
    PlaneSpace
        udir
        (dotSqr udir)
        vdir
        (dotSqr vdir)
        hs
        origin
        iydir
        izdir
  where
    origin =
        vec3Rational (offset *. negateVec norm) (dotSqr norm)
    det = yu * zv - yv * zu
    (iydir, izdir)
        | det /= 0 =
            (Vec2 (zv % det) (-zu % det), Vec2 (-yv % det) (yu % det))
        | otherwise =
            (Vec2 0 0, Vec2 0 0)

toPlaneSpace :: PlaneSpace -> Vec3 -> Vec2
toPlaneSpace ps a =
    toPlaneSpaceDisp ps (a -. planeSpaceOrigin ps)

toPlaneSpaceDisp :: PlaneSpace -> Vec3 -> Vec2
toPlaneSpaceDisp ps a =
    Vec2
        (dot (vec3Rational (planeSpaceUDir ps) (planeSpaceUDirSqr ps)) a)
        (dot (vec3Rational (planeSpaceVDir ps) (planeSpaceVDirSqr ps)) a)

toPlaneSpaceLine :: PlaneSpace -> Line3 -> Line2
toPlaneSpaceLine ps (Line3 pt dir) =
    Line2
        (toPlaneSpace ps pt)
        (toPlaneSpaceDisp ps dir)

toPlaneSpaceHalfSpace :: PlaneSpace -> HalfSpace -> HalfPlane
toPlaneSpaceHalfSpace ps (HalfSpace ni wi) = do
    let rwi2 = dot (vec3FromInteger ni) (planeSpaceOrigin ps)
    halfPlane
        (dot ni (planeSpaceUDir ps) * denominator rwi2)
        (dot ni (planeSpaceVDir ps) * denominator rwi2)
        (numerator rwi2 + wi * denominator rwi2)

fromPlaneSpace :: PlaneSpace -> Vec2 -> Vec3
fromPlaneSpace ps a =
    fromPlaneSpaceDisp ps a +. planeSpaceOrigin ps

fromPlaneSpaceDisp :: PlaneSpace -> Vec2 -> Vec3
fromPlaneSpaceDisp ps (Vec2 u v) =
    u *. vec3FromInteger (planeSpaceUDir ps) +.
    v *. vec3FromInteger (planeSpaceVDir ps)

squareXCosFromPlaneSpace :: PlaneSpace -> IVec2 -> Rational
squareXCosFromPlaneSpace ps (IVec2 du dv) = do
{-
    D = du U + dv V

    D^2 = (du U + dv V).^2 = du^2 U.^2 + dv^2 V.^2 + 2 du dv (U . V)

        by construction U . V = 0

    D^2 = du^2 U.^2 + dv^2 V.^2
-}
    let IVec3 xu _ _ = planeSpaceUDir ps
    let IVec3 xv _ _ = planeSpaceVDir ps
    let dx = xu * du + xv * dv
    let lensqr = planeSpaceUDirSqr ps * du*du + planeSpaceVDirSqr ps * dv*dv
    (dx*dx) % lensqr

planeSpaceFromHalfSpace ::
    HalfSpace ->
    PlaneSpace
planeSpaceFromHalfSpace hs@(HalfSpace nivec@(IVec3 nx ny nz) _)
    | nivec == IVec3 0 0 0 =
        error "zero normal"
    | nx == 0 =
        planeSpace
            (IVec3 1 0 0)
            (IVec3 0 nz' (-ny'))
            hs
    | ny == 0 && nz == 0 =
        planeSpace
            (IVec3 0 1 0)
            (IVec3 0 0 (signum nx))
            hs
    | otherwise = do
        let vivec = IVec3 0 nz' (-ny')
        let uivec = normalizeVec3i $ cross3i vivec nivec
        planeSpace
            uivec
            vivec
            hs
    where
        ny' = ny `quot` (ny `gcd` nz)
        nz' = nz `quot` (ny `gcd` nz)

data XRayPlaneSpaceIntersection
    = XRayPlaneSpaceIntersectionPoint
        !XRay3Half
        {-# UNPACK #-} !Rational
        {-# UNPACK #-} !Vec2
    | XRayPlaneSpaceIntersectionInside
        {-# UNPACK #-} !XRay2
    | XRayPlaneSpaceIntersectionFront
    | XRayPlaneSpaceIntersectionBack
    deriving (Show, Eq)

intersectXRayWithPlaneSpace ::
    XRay3 ->
    PlaneSpace ->
    XRayPlaneSpaceIntersection
intersectXRayWithPlaneSpace xray ps
{-
    xv = 0

    x = xu u        + xo
    y = yu u + yv v + yo
    z = zu u + zv v + zo

    yu u + yv v = y - yo
    zu u + zv v = z - zo

    yu u + yv v = dy
    zu u + zv v = dz

    u =   (dy zv - dz yv) / (yu zv - yv zu)
    v = - (dy zu - dz yu) / (yu zv - yv zu)
    x = xu (dy zv - dz yv) / (yu zv - yv zu) + xo

    u =
        1 / (yu zv - yv zu) (
            + zv y - yv z - yo zv + zo yv
        )
    v =
        1 / (yu zv - yv zu) (
            - zu y + yu z + yo zu - zo yu
        )
    x = xu u + xo
-}
    | planeSpaceUDir ps /= IVec3 1 0 0 = do
        let innerCoords@(Vec2 u _) =
                dy *. planeSpaceIntersectionYDir ps +.
                dz *. planeSpaceIntersectionZDir ps
        case (planeSpaceHalfSpace ps, planeSpaceUDir ps, planeSpaceOrigin ps) of
            (HalfSpace (IVec3 nx _ _) _, IVec3 xu _ _, Vec3 xo _ _) ->
                XRayPlaneSpaceIntersectionPoint
                    (if nx >= 0 then XRay3Right else XRay3Left)
                    (fromInteger xu * u + xo)
                    innerCoords
    | normOffset > 0 =
        XRayPlaneSpaceIntersectionFront
    | normOffset < 0 =
        XRayPlaneSpaceIntersectionBack
    | otherwise =
        case planeSpaceVDir ps of
            IVec3 _ vy vz ->
                XRayPlaneSpaceIntersectionInside
                    (XRay2
                        ((fromInteger vy * dy + fromInteger vz * dz) /
                            fromInteger (planeSpaceVDirSqr ps))
                    )
  where
    XRay3 dy dz = xray -. xray3At (planeSpaceOrigin ps)
    normOffset =
        case planeSpaceHalfSpace ps of
            HalfSpace (IVec3 _ ny nz) _ ->
                numerator dy * denominator dz * ny +
                denominator dy * numerator dz * nz







-- data Projection23 = Projection23
    -- {-# UNPACK #-} !Rational
    -- {-# UNPACK #-} !Rational
    -- {-# UNPACK #-} !Rational
    -- {-# UNPACK #-} !Rational
    -- {-# UNPACK #-} !Rational
    -- {-# UNPACK #-} !Rational
    -- {-# UNPACK #-} !Rational
    -- {-# UNPACK #-} !Rational
  -- deriving (Show, Eq, Ord)

-- project23 :: Projection23 -> Vec3 -> Vec2
-- project23 (Projection23 ux uy uz uw vx vy vz vw) (Vec3 x y z) =
    -- Vec2
        -- (ux * x + uy * y + uz * z + uw)
        -- (vx * x + vy * y + vz * z + vw)

-- projectDisp23 :: Projection23 -> Vec3 -> Vec2
-- projectDisp23 (Projection23 ux uy uz _ vx vy vz _) (Vec3 x y z) =
    -- Vec2
        -- (ux * x + uy * y + uz * z)
        -- (vx * x + vy * y + vz * z)

-- projectLine23 :: Projection23 -> Line3 -> Line2
-- projectLine23 proj (Line3 pt dir) =
    -- Line2 (project23 proj pt) (projectDisp23 proj dir)

-- data Embedding32 = Embedding32
    -- !Integer !Integer {-# UNPACK #-} !Rational
    -- !Integer !Integer {-# UNPACK #-} !Rational
    -- !Integer !Integer {-# UNPACK #-} !Rational
    -- !Integer !Integer
  -- deriving (Eq, Ord)

-- instance Show Embedding32 where
    -- showsPrec d (Embedding32 xu xv xw yu yv yw zu zv zw _ _) =
        -- showParen (d > 10) $
        -- showString "embedding32 " .
        -- showsPrec 11 (IVec3 xu yu zu) .
        -- showString " " .
        -- showsPrec 11 (IVec3 xv yv zv) .
        -- showString " " .
        -- showsPrec 11 (Vec3 xw yw zw)

-- embed32 :: Embedding32 -> Vec2 -> Vec3
-- embed32 (Embedding32 xu xv xw yu yv yw zu zv zw _ _) (Vec2 u v) =
    -- Vec3
        -- (fromInteger xu * u + fromInteger xv * v + xw)
        -- (fromInteger yu * u + fromInteger yv * v + yw)
        -- (fromInteger zu * u + fromInteger zv * v + zw)

-- embedDisp32 :: Embedding32 -> Vec2 -> Vec3
-- embedDisp32 (Embedding32 xu xv _ yu yv _ zu zv _ _ _) (Vec2 u v) =
    -- Vec3
        -- (fromInteger xu * u + fromInteger xv * v)
        -- (fromInteger yu * u + fromInteger yv * v)
        -- (fromInteger zu * u + fromInteger zv * v)

-- invertEmbedding32 :: Embedding32 -> Maybe Projection23
-- invertEmbedding32 (Embedding32 xu xv xw yu yv yw zu zv zw usqr vsqr) = do
    -- let !() = assert (xu*xu + yu*yu + zu*zu == usqr) ()
    -- let !() = assert (xv*xv + yv*yv + zv*zv == vsqr) ()
    -- let !() = assert (xu*xv + yu*yv + zu*zv == 0) ()
    -- guard (usqr /= 0 && vsqr /= 0)
    -- let ux = xu % usqr
    -- let uy = yu % usqr
    -- let uz = zu % usqr
    -- let uw = - ux*xw - uy*yw - uz*zw
    -- let vx = xv % vsqr
    -- let vy = yv % vsqr
    -- let vz = zv % vsqr
    -- let vw = - vx*xw - vy*yw - vz*zw
    -- Just $ Projection23
        -- ux uy uz uw
        -- vx vy vz vw
        -- {- Moore-Penrose pseudoinverse -}
        -- let usqr = xu*xu + yu*yu + zu*zu
        -- let vsqr = xv*xv + yv*yv + zv*zv
        -- let uvdot = xu*xv + yu*yv + zu*zv
        -- let hdet = usqr * vsqr - uvdot * uvdot
        -- guard (hdet /= 0)
        -- let hu = usqr / hdet
        -- let hv = vsqr / hdet
        -- let hd = - uvdot / hdet
        -- let ux = hv*xu + hd*xv
        -- let uy = hv*yu + hd*yv
        -- let uz = hv*zu + hd*zv
        -- let uw = - ux*xw - uy*yw - uz*zw
        -- let vx = hd*xu + hu*xv
        -- let vy = hd*yu + hu*yv
        -- let vz = hd*zu + hu*zv
        -- let vw = - vx*xw - vy*yw - vz*zw
        -- Just $ Projection23
            -- ux uy uz uw
            -- vx vy vz vw

-- data PlaneSpace = PlaneSpace
    -- { planeSpaceProj :: !Projection23
    -- , planeSpaceEmbed :: !Embedding32
    -- , planeSpaceHalfSpace :: !HalfSpace
    -- }
    -- deriving (Eq, Ord)

-- instance Show PlaneSpace where
    -- showsPrec d ps
        -- | let hs = planeSpaceHalfSpace ps
        -- , ps == planeSpaceFromHalfSpace hs =
            -- showParen (d > 10) $
            -- showString "planeSpaceFromHalfSpace " .
            -- showsPrec 11 hs
        -- | otherwise =
            -- showParen (d > 10) $
            -- showString "planeSpace " .
            -- showsPrec 11 (planeSpaceEmbed ps)

-- toPlaneSpace :: PlaneSpace -> Vec3 -> Vec2
-- toPlaneSpace ps = project23 (planeSpaceProj ps)

-- fromPlaneSpace :: PlaneSpace -> Vec2 -> Vec3
-- fromPlaneSpace ps = embed32 (planeSpaceEmbed ps)

-- toPlaneSpaceDisp :: PlaneSpace -> Vec3 -> Vec2
-- toPlaneSpaceDisp ps = projectDisp23 (planeSpaceProj ps)

-- fromPlaneSpaceDisp :: PlaneSpace -> Vec2 -> Vec3
-- fromPlaneSpaceDisp ps = embedDisp32 (planeSpaceEmbed ps)

-- toPlaneSpaceLine :: PlaneSpace -> Line3 -> Line2
-- toPlaneSpaceLine ps = projectLine23 (planeSpaceProj ps)

-- squareXCosFromPlaneSpace :: PlaneSpace -> IVec2 -> Rational
-- squareXCosFromPlaneSpace ps (IVec2 u v) =
    -- case planeSpaceEmbed ps of
        -- Embedding32
            -- xu xv _
            -- _ _ _
            -- _ _ _
            -- usqr vsqr
          -- -> do
            -- let x = xu * u + xv * v
            -- let lensqr = usqr * u*u + vsqr * v*v
            -- x % lensqr
{-
    x = xu u + xv v
    y = yu u + yv v
    z = zu u + zv v

    x^2 / (x^2 + y^2 + z^2)

    x^2 = (xu u + xv v)^2
    y^2 = (yu u + yv v)^2
    z^2 = (zu u + zv v)^2

    x^2 = xu^2 u^2 + xv^2 v^2 + 2 xu xv u v
    y^2 = yu^2 u^2 + yv^2 v^2 + 2 yu yv u v
    z^2 = zu^2 u^2 + zv^2 v^2 + 2 zu zv u v

    x^2 + y^2 + z^2 =
        + (xu^2 + yu^2 + zu^2) u^2
        + (xv^2 + yv^2 + zv^2) v^2
        + 2 (xu xv + yu yv + zu zv) u v
-}

-- planeSpaceFromHalfSpace ::
    -- HalfSpace ->
    -- PlaneSpace
-- planeSpaceFromHalfSpace hs@(HalfSpace nivec@(IVec3 nx _ _) offset) = do
    -- let !() =
            -- if nivec == IVec3 0 0 0
                -- then error "zero normal"
                -- else ()
    -- {-  Expected invariant: for planes parallel to X axis,
        -- their U vector is always (Vec3 1 0 0)
        -- Used in Geometry.Face.intersectXRayWithFace -}
    -- let uivec =
            -- if nx == 0
                -- then IVec3 1 0 0
                -- else normalizeVec3i $ cross3i (IVec3 0 1 0) nivec
    -- let vivec = normalizeVec3i $ cross3i nivec uivec
    -- let Vec3 xw yw zw = vec3Rational (offset *. negateVec nivec) (dotSqr nivec)
    -- let u@(IVec3 xu yu zu) = uivec
    -- let v@(IVec3 xv yv zv) = vivec
    -- let emb =
            -- Embedding32
                -- xu xv xw
                -- yu yv yw
                -- zu zv zw
                -- (dotSqr u) (dotSqr v)
    -- case invertEmbedding32 emb of
        -- Just proj ->
            -- PlaneSpace
                -- { planeSpaceProj = proj
                -- , planeSpaceEmbed = emb
                -- , planeSpaceHalfSpace = hs
                -- }
        -- Nothing ->
            -- error "degenerate embedding"

{-
    plane space intersection
        PSA, PSB

    a pair of XFrorm2
        XLA, XLB

    a common line
        L

    common line:
        PSA.embed . PA = PSB.embed . PB

    half-space:
        Cross[embed.U, embed.V] . (P - embed.W) = 0
        N = Cross[embed.U, embed.V]
        w = - Cross[embed.U, embed.V] . embed.W
        N . P + w = 0

    half-space intersection:
        D = Cross[PSA.N, PSB.N]
        P = Cross[PSB.w PSA.N - PSA.w PSB.N, D] / D^2

    projecting back:
        DA.u = PSA.proj.U . D
        DA.v = PSA.proj.V . D
        PA.u = PSA.proj.U . P + PSA.proj.u0
        PA.v = PSA.proj.V . P + PSA.proj.v0

        D = Cross[PSA.N, PSB.N]
        D = Cross[Cross[PSA.embed.U, PSA.embed.V], Cross[PSB.embed.U, PSB.embed.V]]


    PSA.embed.xu * a.u + PSA.embed.xv * a.v + PSA.embed.x0 = PSB.embed.xu * b.u + PSB.embed.xv * b.v + PSB.embed.x0
    PSA.embed.yu * a.u + PSA.embed.yv * a.v + PSA.embed.y0 = PSB.embed.yu * b.u + PSB.embed.yv * b.v + PSB.embed.y0
    PSA.embed.zu * a.u + PSA.embed.zv * a.v + PSA.embed.z0 = PSB.embed.zu * b.u + PSB.embed.zv * b.v + PSB.embed.z0

    matrix:
        axu AU + axv AV - bxu BU - bxv BV = bx0 - ax0
        ayu AU + ayv AV - byu BU - byv BV = by0 - ay0
        azu AU + azv AV - bzu BU - bzv BV = bz0 - az0

    null space:
        D[AU] = + Det[{PSA.embed.V, PSB.embed.U, PSB.embed.V}]
        D[AV] = - Det[{PSA.embed.U, PSB.embed.U, PSB.embed.V}]
        D[BU] = - Det[{PSA.embed.U, PSA.embed.V, PSB.embed.V}]
        D[BV] = + Det[{PSA.embed.U, PSA.embed.V, PSB.embed.U}]
-}


-- halfSpaceRelation :: HalfSpace -> HalfSpace -> HalfSpaceRelation
-- halfSpaceRelation (HalfSpace ai aw) (HalfSpace bi bw)
    -- | (ai, aw) == (bi, bw) =
        -- HalfSpaceCoplanar True
    -- | (ai, aw) == (negateVec bi, -bw) =
        -- HalfSpaceCoplanar False
    -- | ni == IVec3 0 0 0 =
        -- HalfSpaceParallel
    -- | otherwise = do
        -- let dir = vecFromInteger ni
        -- let ndet = dotSqr ni
        -- let ui = cross3i (bw *. ai -. aw *. bi) ni
        -- let pt = vecRational ui ndet
        -- HalfSpaceIntersect (Line pt dir)
  -- where
    -- ni = cross3i ai bi
