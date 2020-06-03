-- Black magic...
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DotHacks where

import Data.Generics.Product.Fields qualified as GL
import GHC.Generics (Generic)
import GHC.Records.Extra
import Graphics.Svg.Types
import Lens.Micro

instance {-# OVERLAPPABLE #-} GL.HasField name s s a a => HasField name s a where
    hasField s = (\a -> s & l .~ a, s ^. l)
        where
            l :: Lens' s a
            l = GL.field @name

-- we need to derive Generic for every external type with which we wish to use dot syntax
--TODO is there a plugin, or some Template Haskell, that can automate this?
deriving instance Generic (Group a)

deriving instance Generic Document

deriving instance Generic DrawAttributes

deriving instance Generic Path

deriving instance Generic Tree
