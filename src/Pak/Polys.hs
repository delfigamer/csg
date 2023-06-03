module Pak.Polys where

import Data.Int
import Data.Sequence (Seq (..))
import Data.Word
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.Text as Text

data Vector = Vector
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float
  deriving (Show)

Aeson.deriveJSON Aeson.defaultOptions ''Vector

data Rotator = Rotator
    { rPitch :: {-# UNPACK #-} !Int32
    , rYaw :: {-# UNPACK #-} !Int32
    , rRoll :: {-# UNPACK #-} !Int32
    }
  deriving (Show)

Aeson.deriveJSON Aeson.defaultOptions ''Rotator

data ShearAxis
    = SANone
    | SAXY
    | SAXZ
    | SAYX
    | SAYZ
    | SAZX
    | SAZY
  deriving (Show, Enum)

Aeson.deriveJSON Aeson.defaultOptions ''ShearAxis

data Scale = Scale
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float
    !ShearAxis
  deriving (Show)

Aeson.deriveJSON Aeson.defaultOptions ''Scale

data PolysFace = PolysFace
    { fOrigin :: !Vector
    , fNormal :: !Vector
    , fTextureU :: !Vector
    , fTextureV :: !Vector
    , fVertices :: !(Seq Vector)
    , fFlags :: {-# UNPACK #-} !Word32
    , fTextureName :: !Text.Text
    , fUPan :: {-# UNPACK #-} !Int16
    , fVPan :: {-# UNPACK #-} !Int16
    }
  deriving (Show)

Aeson.deriveJSON Aeson.defaultOptions ''PolysFace

data InstanceOper
    = OperAddSolid
    | OperSubtractSolid
    | OperAddNonSolid
    | OperAddSemiSolid
  deriving (Show)

Aeson.deriveJSON Aeson.defaultOptions ''InstanceOper

data InstanceTransform = InstanceTransform
    { itPrePivot :: !Vector
    , itMainScale :: !Scale
    , itRotation :: !Rotator
    , itPostScale :: !Scale
    , itLocation :: !Vector
    }
  deriving (Show)

Aeson.deriveJSON Aeson.defaultOptions ''InstanceTransform

data Instance = Instance
    { iOper :: !InstanceOper
    , iTransform :: !InstanceTransform
    , iIsSemisolid :: !Bool
    , iFaces :: !(Seq PolysFace)
    , iObjectName :: Text.Text
    }
  deriving (Show)

Aeson.deriveJSON Aeson.defaultOptions ''Instance
