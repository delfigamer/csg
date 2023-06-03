module Common.Types where

import Geometry.Class
import Geometry.Mat3
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.Text as Text

type Material = MaterialT Text.Text

data MaterialT name = Material
    { materialTextureSpace :: Xform3
    , materialTextureName :: name
    , materialSemisolid :: Bool
    , materialMasked :: Bool
    , materialTranslucent :: Bool
    , materialModulated :: Bool
    , materialUnlit :: Bool
    , materialMirror :: Bool
    , materialInvisible :: Bool
    }
  deriving (Show, Eq, Ord)

instance ToWolfMod (MaterialT name) where
    toWolfMod _ = []

Aeson.deriveJSON Aeson.defaultOptions ''MaterialT
