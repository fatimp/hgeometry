{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Point
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(d\)-dimensional points.
--
--------------------------------------------------------------------------------
module Data.Geometry.Point( Point(.., Point1, Point2, Point3)
                          , origin, vector
                          , pointFromList
                          , projectPoint

                          , xCoord, yCoord, zCoord

                          , PointFunctor(..)

                          , CCW, ccw, isCoLinear
                          , pattern CCW, pattern CW, pattern CoLinear

                          , ccwCmpAround, ccwCmpAround'
                          , cwCmpAround, cwCmpAround'
                          , ccwCmpAroundWith, ccwCmpAroundWith'
                          , cwCmpAroundWith, cwCmpAroundWith'
                          , sortAround, sortAround'
                          , insertIntoCyclicOrder

                          , Quadrant(..), quadrantWith, quadrant, partitionIntoQuadrants

                          , cmpByDistanceTo, cmpByDistanceTo', cmpInDirection

                          , squaredEuclideanDist, euclideanDist
                          , HasSquaredEuclideanDistance(..)

                          , coord, unsafeCoord
                          ) where

import Data.Geometry.Point.Class
import Data.Geometry.Point.Internal hiding (coord, unsafeCoord)
import Data.Geometry.Point.Orientation.Degenerate
import Data.Geometry.Point.Quadrants
import Data.Geometry.Line.Internal
import Data.Geometry.Vector

--------------------------------------------------------------------------------

-- | Compare the points with respect to the direction given by the
-- vector, i.e. by taking planes whose normal is the given vector.
--
-- >>> cmpInDirection (Vector2 1 0) (Point2 5 0) (Point2 10 0)
-- LT
-- >>> cmpInDirection (Vector2 1 1) (Point2 5 0) (Point2 10 0)
-- LT
-- >>> cmpInDirection (Vector2 1 1) (Point2 5 0) (Point2 10 10)
-- LT
-- >>> cmpInDirection (Vector2 1 1) (Point2 15 15) (Point2 10 10)
-- GT
-- >>> cmpInDirection (Vector2 1 0) (Point2 15 15) (Point2 15 10)
-- EQ
cmpInDirection       :: (Ord r, Num r) => Vector 2 r -> Point 2 r -> Point 2 r -> Ordering
cmpInDirection n p q = case p `onSide` perpendicularTo (Line q n) of
                         LeftSide  -> LT
                         OnLine    -> EQ
                         RightSide -> GT
  -- TODO: Generalize to arbitrary dimension
