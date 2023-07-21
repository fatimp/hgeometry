{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.HyperPlane.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Classes for representing hyperplanes in d-dimensional space.
--
--------------------------------------------------------------------------------
module HGeometry.HyperPlane.Class
  ( HyperPlane_(..)
  , ConstructableHyperPlane_(..)
  , NonVerticalHyperPlane_(..)
  , isParallelTo

  , HyperPlaneFromPoints(..)
  ) where

import Control.Lens hiding (snoc, cons, uncons, unsnoc)
import Data.Default.Class
import Data.Kind (Constraint)
import Data.Type.Ord
import GHC.TypeNats
import HGeometry.Ext
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Vector
import Prelude hiding (head,last)

--------------------------------------------------------------------------------

-- | myLine, myHyperPlane, and myNVHyperPlane all represent the same
-- 2d hyperplane; i.e. a line in R^2.

-- $setup
-- >>> import HGeometry.HyperPlane.NonVertical
-- >>> import HGeometry.HyperPlane
-- >>> import HGeometry.Line.LineEQ
--
-- >>> let myLine = LineEQ 1 (2 :: Double)
-- >>> let myHyperPlane2 = HyperPlane $ Vector3 2 1 (-1 :: Double)
-- >>> let myNVHyperPlane2 = NonVerticalHyperPlane $ Vector2 1 (2 :: Double)



-- | A class to represent hyperplanes in d-dimensional space.
class ( NumType hyperPlane ~ r
      , Dimension hyperPlane ~ d
      , Has_ Vector_ d r
      , Has_ Vector_ (1+d) r
      -- , NumType (EquationFor hyperPlane) ~ r
      ) => HyperPlane_ hyperPlane d r | hyperPlane -> d
                                      , hyperPlane -> r where
--  {-# MINIMAL hyperPlaneTrough #-}

  -- -- | Constructs the hyperplane through d points
  -- hyperPlaneTrough :: Num r => Point_ point d r => Vector d point -> hyperPlane

  -- | A hyperplane \(h) has coefficients \(a_i \in \mathbb{R}\) so that
  -- a \point \( (p_1,..,p_d) \) lies on \(h) iff:
  -- \( a_0  + \sum_i=1^d a_i*p_i = 0 \)
  --
  -- this fuction returns the vector of these coefficients \(\langle a_0,..,a_d \rangle\)
  --
  -- >>> hyperPlaneEquation myLine
  -- Vector3 2.0 1.0 (-1.0)
  -- >>> hyperPlaneEquation myHyperPlane2
  -- Vector3 2.0 1.0 (-1.0)
  -- >>> hyperPlaneEquation myNVHyperPlane2
  -- Vector3 2.0 1.0 (-1.0)
  hyperPlaneEquation :: ( Num r
                        ) => hyperPlane -> Vector (d+1) r
  default hyperPlaneEquation :: ( NonVerticalHyperPlane_ hyperPlane d r
                                , Num r
                                , 1 <= d
                                )
                             => hyperPlane -> Vector (d+1) r
  hyperPlaneEquation h = cons a0 a
    where
      a' = h^.hyperPlaneCoefficients
      a  = a'&last .~ -1
      a0 = a'^.last

  {-# INLINE hyperPlaneEquation #-}

  -- | Get the normal vector of the hyperplane.
  --
  --
  normalVector :: Num r => hyperPlane -> Vector d r
  default normalVector :: ( KnownNat d
                          , Num r
                          )
                       => HyperPlane_ hyperPlane d r => hyperPlane -> Vector d r
  normalVector = suffix . hyperPlaneEquation
  {-# INLINE normalVector #-}


  -- | Test if a point lies on a hyperplane.
  --
  -- >>> Point2 0 2 `onHyperPlane` myHyperPlane2
  -- True
  -- >>> Point2 1 3 `onHyperPlane` myHyperPlane2
  -- True
  -- >>> Point2 1 5 `onHyperPlane` myHyperPlane2
  -- False
  --
  -- >>> Point2 0 2 `onHyperPlane` myNVHyperPlane2
  -- True
  -- >>> Point2 1 3 `onHyperPlane` myNVHyperPlane2
  -- True
  -- >>> Point2 1 5 `onHyperPlane` myNVHyperPlane2
  -- False
  onHyperPlane     :: (Point_ point d r, Eq r, Num r) => point -> hyperPlane -> Bool
  default onHyperPlane :: ( Point_ point d r, Eq r, Num r
                          , Has_ Metric_ d r
                          , d < d+1
                          ) => point -> hyperPlane -> Bool
  q `onHyperPlane` h = a0 + (a `dot` (q^.vector)) == 0
    where
      (a0,a) = uncons $ hyperPlaneEquation h
  {-# INLINE onHyperPlane #-}

  -- | Test if a point lies on a hyperplane. For non-vertical
  -- hyperplanes, returns whether the point is *above* the hyperplane
  -- or not. For vertical hyperplanes, we return 'LT' when the point
  -- is on the left.
  --
  -- >>> Point2 0 2 `onSideTest` myHyperPlane2
  -- EQ
  -- >>> Point2 1 3 `onSideTest` myHyperPlane2
  -- EQ
  -- >>> Point2 1 5 `onSideTest` myHyperPlane2
  -- GT
  -- >>> Point2 4 5 `onSideTest` myHyperPlane2
  -- LT
  -- >>> Point2 0 0 `onSideTest` HyperPlane 1 (-1) 0
  -- LT
  --
  -- >>> Point2 0 2 `onSideTest` myNVHyperPlane2
  -- EQ
  -- >>> Point2 1 3 `onSideTest` myNVHyperPlane2
  -- EQ
  -- >>> Point2 1 5 `onSideTest` myNVHyperPlane2
  -- GT
  -- >>> Point2 4 5 `onSideTest` myNVHyperPlane2
  -- LT
  onSideTest     :: (Point_ point d r, Ord r, Num r) => point -> hyperPlane -> Ordering
  default onSideTest :: ( Point_ point d r, Ord r, Num r
                        , Has_ Metric_ d r
                        , d < d+1 -- silly constraints
                        ) => point -> hyperPlane -> Ordering
  q `onSideTest` h = 0 `compare` (a0 + (a `dot` (q^.vector)))
    where
      (a0,a) = uncons $ hyperPlaneEquation h
  {-# INLINE onSideTest #-}

-- | Class representing hyperplanes with methods to construct the
-- hyperplane.
class HyperPlane_ hyperPlane d r
     => ConstructableHyperPlane_ hyperPlane d r where

  -- | Additional constraints for constructing a hyperplane from its equation.
  type HyperPlaneFromEquationConstraint hyperPlane d r :: Constraint
  type HyperPlaneFromEquationConstraint hyperPlane d r = ()

  -- | Given the coefficients \(a_0,..,a_d\) of the equation, i.e.
  -- so that
  --
  -- \( a_0  + \sum_i=1^d a_i*p_i = 0 \)
  --
  -- construct the hyperplane form it.
  --
  -- >>> Vector3 "c" "a" "b" -- the line a*x + b*y + c = 0
  -- HyperPlane ["c","a","b"]
  -- >>> Vector3 10 2 1 -- the line 2*x + 1*y + 10 = 0
  -- HyperPlane [10,2,1]
  -- >>> Vector4 "d" "a" "b" "c" -- the plane a*x + b*y + c*z + d = 0
  -- HyperPlane ["d","a","b","c"]
  hyperPlaneFromEquation :: HyperPlaneFromEquationConstraint hyperPlane d r
                         => Vector (d+1) r -> hyperPlane


  -- | Construct a Hyperplane from a point and a normal.
  fromPointAndNormal     :: ( Point_ point d r, Num r)
                         => point -> Vector d r -> hyperPlane
  default fromPointAndNormal :: ( Point_ point d r, Num r
                                , ConstructableHyperPlane_ hyperPlane d r
                                , HyperPlaneFromEquationConstraint hyperPlane d r
                                , Has_ Metric_ d r
                                )
                             => point -> Vector d r -> hyperPlane
  fromPointAndNormal q n = hyperPlaneFromEquation $ cons a0 n
    where
      a0 = negate $ (q^.vector) `dot` n
  {-# INLINE fromPointAndNormal #-}


-- -- | returns a point that lies on the hyperpalne
-- pointOn :: hyperPlane -> Point d r


--------------------------------------------------------------------------------

-- | Non-vertical hyperplanes.
class HyperPlane_ hyperPlane d r => NonVerticalHyperPlane_ hyperPlane d r where
  {-# MINIMAL hyperPlaneCoefficients #-}

  -- | Get the coordinate in dimesnion $d$ of the hyperplane at the given position.
  --
  -- >>> evalAt (Point1 1) myNVHyperPlane2
  -- 3.0
  -- >>> evalAt (Point1 10) myNVHyperPlane2
  -- 12.0
  evalAt     :: ( Num r
                , 1 <= d
                , Point_ point (d-1) r
                ) => point -> hyperPlane -> r
  default evalAt :: ( Num r
                    , 1 <= d
                    , Point_ point (d-1) r
                    , 1 + (d-1) ~ d -- silly silly agian :(
                    , Has_ Metric_ d r
                    ) => point -> hyperPlane -> r
  evalAt p h = (h^.hyperPlaneCoefficients) `dot` snoc (p^.vector) 1
  {-# INLINE evalAt #-}


  -- | The coefficients \( \langle a_1,..,a_d \rangle \) such that
  -- a point \(p = (p_1,..,p_d) \) lies on the hyperplane the given coefficients iff
  --
  --
  --  \( a_d + \sum_i=1^{d-1} a_i*p_i = p_d \)
  --
  -- >>> view hyperPlaneCoefficients myNVHyperPlane2
  -- Vector2 1.0 2.0
  -- >>> view hyperPlaneCoefficients myLine
  -- Vector2 1.0 2.0
  -- >>> view hyperPlaneCoefficients $ Plane 1 2 3
  -- Vector3 1 2 3
  hyperPlaneCoefficients :: Lens' hyperPlane (Vector d r)


--------------------------------------------------------------------------------
-- * Functions on Hyperplanes

-- | Test if two hyperplanes are parallel.
isParallelTo       :: ( HyperPlane_ hyperPlane  d r
                      , HyperPlane_ hyperPlane' d r
                      , Has_ Metric_ d r
                      , Num r, Eq r
                      ) => hyperPlane -> hyperPlane' -> Bool
isParallelTo h1 h2 = isScalarMultipleOf (normalVector h1) (normalVector h2)

-- | Class that tells us we can construct a hyperplane from a vector
-- of points.
class HyperPlaneFromPoints hyperPlane where
  -- | Construct a hyperplane through the given d points.
  hyperPlaneThrough     :: ( Point_ point d r
                           , HyperPlane_ hyperPlane d r
                           , Num r
                           ) => Vector d point -> hyperPlane


instance (HyperPlane_ hyperPlane d r, Default extra)
         => HyperPlane_ (hyperPlane :+ extra) d r where
  hyperPlaneEquation = hyperPlaneEquation . view core
  {-# INLINE hyperPlaneEquation #-}
  normalVector = normalVector . view core
  {-# INLINE normalVector #-}
  onHyperPlane q = onHyperPlane q . view core
  {-# INLINE onHyperPlane #-}
  onSideTest q = onSideTest q . view core
  {-# INLINE onSideTest #-}

instance (ConstructableHyperPlane_ hyperPlane d r, Default extra)
          => ConstructableHyperPlane_ (hyperPlane :+ extra) d r where
  type HyperPlaneFromEquationConstraint (hyperPlane :+ extra) d r =
         HyperPlaneFromEquationConstraint hyperPlane d r
  hyperPlaneFromEquation v = hyperPlaneFromEquation v :+ def
  {-# INLINE hyperPlaneFromEquation #-}
  fromPointAndNormal p v = fromPointAndNormal p v :+ def
  {-# INLINE fromPointAndNormal #-}

instance (NonVerticalHyperPlane_  hyperPlane d r, Default extra)
         => NonVerticalHyperPlane_ (hyperPlane :+ extra) d r where
  evalAt p = evalAt p . view core
  {-# INLINE evalAt #-}
  hyperPlaneCoefficients = core.hyperPlaneCoefficients
  {-# INLINE hyperPlaneCoefficients #-}
