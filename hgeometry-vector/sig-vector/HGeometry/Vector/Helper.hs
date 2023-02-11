module HGeometry.Vector.Helper
  ( xComponent
  , yComponent
  , zComponent
  , wComponent
  ) where

import Control.Lens
import D
import Data.Type.Ord
import R
import Vector

--------------------------------------------------------------------------------

-- | Access the x-coordinate
xComponent :: (0 < D) => IndexedLens' Int Vector R
xComponent = component @0
{-# INLINE xComponent #-}

-- | Access the y-coordinate
yComponent :: (1 < D) => IndexedLens' Int Vector R
yComponent = component @1
{-# INLINE yComponent #-}

-- | Access the z-coordinate
zComponent :: (2 < D) => IndexedLens' Int Vector R
zComponent = component @2
{-# INLINE zComponent #-}

-- | Access the w-coordinate
wComponent :: (3 < D) => IndexedLens' Int Vector R
wComponent = component @3
{-# INLINE wComponent #-}
