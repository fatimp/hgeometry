{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.HyperPlane.NonVertical
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Non-vertical hyperplanes in d-dimensional space.
--
--------------------------------------------------------------------------------
module HGeometry.HyperPlane.NonVertical
  ( NonVerticalHyperPlane(NonVerticalHyperPlane, Plane)
  ) where

import Data.Type.Ord
import GHC.TypeLits
import HGeometry.HyperPlane.Class
import HGeometry.HyperPlane.Internal (MkHyperPlaneConstraints)
import HGeometry.Properties
import HGeometry.Vector

--------------------------------------------------------------------------------

-- $setup
-- >>> let myHyperPlane = NonVerticalHyperPlane $ Vector2 1 2
--


-- | A non-vertical Hyperplane described by \( x_d = a_d + \sum_{i=1}^{d-1}
-- a_i * x_i \) where \(\langle a_1,..,a_d \rangle \) are the
-- coefficients of te hyperplane.
--
--
-- e.g. the 'myHyperPlane' defines the hyperplane described by
--
-- y = 2 + 1*x
--
newtype NonVerticalHyperPlane d r = NonVerticalHyperPlane (Vector d r)

type instance NumType   (NonVerticalHyperPlane d r) = r
type instance Dimension (NonVerticalHyperPlane d r) = d

deriving instance Eq  (Vector d r) => Eq (NonVerticalHyperPlane d r)
deriving instance Ord (Vector d r) => Ord (NonVerticalHyperPlane d r)

deriving instance Show (Vector d r) => Show (NonVerticalHyperPlane d r)

instance ( MkHyperPlaneConstraints d r
         , 2 <= d
         ) => HyperPlane_ (NonVerticalHyperPlane d r) d r where

  fromPointAndNormal _ n = NonVerticalHyperPlane n
  -- see https://en.wikipedia.org/wiki/Normal_(geometry)
  -- FIXME: this seems fishy; don't we need the point?


instance ( MkHyperPlaneConstraints d r
         , Fractional r
         , 2 <= d
         ) => ConstructableHyperPlane_ (NonVerticalHyperPlane d r) d r where
  -- | pre: the last component is not zero
  --
  --
  -- >>> hyperPlaneFromEquation $ Vector3 2 1 (-1)
  -- NonVerticalHyperPlane (Vector2 1 2)
  hyperPlaneFromEquation e = NonVerticalHyperPlane $ a ^/ (-ad)
    where
      (e' :: Vector d r, ad :: r) = unsnoc e
      (a0 :: r, as :: Vector (d-1) r) = uncons e'
      a       = snoc as a0 :: Vector d r
  {-# INLINE hyperPlaneFromEquation #-}

instance ( MkHyperPlaneConstraints d r
         , Num r
         , 1 + (d-1) ~ d
         , 2 <= d
         ) => NonVerticalHyperPlane_ (NonVerticalHyperPlane d r) d r where
  --
  -- >>> hyperPlaneCoefficients
  hyperPlaneCoefficients (NonVerticalHyperPlane v) = v

--------------------------------------------------------------------------------
-- * Specific 3D Functions

-- | Constructs a Plane in R^3 for the equation z = ax + by + c
pattern Plane       :: r -> r -> r -> NonVerticalHyperPlane 3 r
pattern Plane a b c = NonVerticalHyperPlane (Vector3 a b c)
