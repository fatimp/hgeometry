{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Box.Boxable
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Typeclass that expresses that we can compute an axis parallel bounding box of an
-- object.
--
--------------------------------------------------------------------------------
module HGeometry.Box.Boxable
  ( IsBoxable(..)
  ) where

import Control.Lens
import Data.Functor.Contravariant (phantom)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup.Foldable
import HGeometry.Box.Class
import HGeometry.Box.Internal
import HGeometry.Ext
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Vector

--------------------------------------------------------------------------------

-- foo'     :: ( IxValue vector ~ Bool
--             , Additive_ vector
--             , Eq (IxValue vector)
--             ) => vector -> vector -> Bool
-- foo' u v = andOf components $ liftU2 (==) u v

-- | Types for which we can compute an axis parallel boundingbox
class IsBoxable g where
  -- | Compute the axis-parallel boundingbox of the given geometry.
  boundingBox :: ( d ~ Dimension g, r ~ NumType g
                 , Ord r
                 ) => g -> Box (Point d r)
  default boundingBox  :: ( d ~ Dimension g, r ~ NumType g
                          , Ord r
                          , HasPoints' g point
                          , Point_ point d r
                          , Ord (Vector d r)
                          )
                       => g -> Box (Point d r)
  boundingBox = defaultBBox
  {-# INLINE boundingBox #-}

-- | default implementation of boundingBox
defaultBBox  :: forall g point d r. ( d ~ Dimension g, r ~ NumType g
                , HasPoints' g point
                , Point_ point d r
                , Ord (Vector d r)
                )
              => g -> Box (Point d r)
defaultBBox g = Box bl tr
    where
      thePoints :: Fold1 g (Point d r)
      thePoints = folding1 (fmap (view asPoint) . toNonEmptyOf allPoints)
      bl = minimum1Of thePoints g
      tr = maximum1Of thePoints g

-- | construct a Fold1 from a function that produces a Foldable1
folding1         :: Foldable1 f => (s -> f a) -> Fold1 s a
folding1 sfa agb = phantom . traverse1_ agb . sfa
{-# INLINE folding1 #-}

  -- $ \g -> g^..allPoints

--------------------------------------------------------------------------------
-- Instances

instance IsBoxable (Point d r) where
  boundingBox p = Box p p

instance ( Box_ (Box point) point
         , Point_ point d r
         ) => IsBoxable (Box point) where
  boundingBox = fmap (view asPoint)
  {-# INLINE boundingBox #-}

instance IsBoxable g => IsBoxable (g :+ extra) where
  boundingBox = boundingBox . view core
  {-# INLINE boundingBox #-}

----------------------------------------
newtype Union point = Union { unUnion :: Box point }

instance (Point_ point d r, Ord r) => Semigroup (Union point) where
  (Union (Box p q)) <> (Union (Box p' q')) = Union $ Box (f min p p') (f max q q')
    where
      f combine a b = b&vector %~ liftI2 combine (a^.vector)

instance ( IsBoxable g
         , Has_ Additive_ d r
         , d ~ Dimension g, r ~ NumType g
         ) => IsBoxable (NonEmpty g) where
  boundingBox = unUnion . foldMap1 (Union . boundingBox)



----------------------------------------
