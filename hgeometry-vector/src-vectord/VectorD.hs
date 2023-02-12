{-# OPTIONS_GHC -Wno-orphans #-}
module VectorD
  ( Vector(..)
  , Out.Vec(..)
  ) where

import           Control.DeepSeq (NFData)
import           Control.Lens
import           GHC.Generics (Generic)
import           HGeometry.Vector.Class
import           R
import           D
import qualified Out
import qualified In

--------------------------------------------------------------------------------

-- | vectors
newtype instance Vector D R = V_D Out.Vec
  deriving newtype (Eq,Ord,Generic,NFData)

_VD :: Iso' (Vector D R) Out.Vec
_VD = iso (\(V_D v) -> v) V_D

instance (IxValue In.Vec ~ R) => VectorLike_ (Vector D R) where
  components = components' _VD
  {-# INLINE components #-}
  unsafeComponent i = unsafeComponent' _VD i
  {-# INLINE unsafeComponent #-}

deriving newtype instance (IxValue In.Vec ~ R) => Additive_ (Vector D R)

--------------------------------------------------------------------------------
-- * Helpers

-- | implementation of component
components'    :: (VectorLike_ vecImpl, IxValue vecImpl ~ R)
               => Iso' vector vecImpl -> IndexedTraversal1' Int vector R
components' is = is.components
{-# INLINE components' #-}

-- | implementation of unsafeComponent
unsafeComponent'      :: (VectorLike_ vecImpl, IxValue vecImpl ~ R)
                      => Iso' vector vecImpl -> Int -> IndexedLens' Int vector R
unsafeComponent' is i = is.unsafeComponent i
{-# INLINE unsafeComponent' #-}
