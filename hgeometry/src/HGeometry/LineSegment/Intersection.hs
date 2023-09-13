--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.LineSegment.Intersection
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
--
--------------------------------------------------------------------------------
module HGeometry.LineSegment.Intersection
  ( BooleanSweep.hasIntersections
  , BO.intersections
  , BO.interiorIntersections
  , Intersections
  , Associated(..), startPointOf, endPointOf, interiorTo
  , IntersectionPoint(..), mkIntersectionPoint
  , intersectionPoint, associatedSegs
  -- , isInteriorIntersection
  , hasSelfIntersections

  , AroundStart(..), AroundEnd(..), AroundIntersection(..)
  ) where

import qualified Data.Map as Map
import           HGeometry.Ext (ext)
import           HGeometry.LineSegment
import qualified HGeometry.LineSegment.Intersection.BentleyOttmann as BO
import qualified HGeometry.LineSegment.Intersection.BooleanSweep as BooleanSweep
import           HGeometry.LineSegment.Intersection.Types
import           HGeometry.Polygon

-- | Test if the polygon has self intersections.
--
-- \(O(n \log n)\)
hasSelfIntersections :: (Ord r, Fractional r) => Polygon t p r -> Bool
hasSelfIntersections = not . Map.null . BO.interiorIntersections . map ext . listEdges
-- hasSelfIntersections :: (Ord r, Num r) => Polygon t p r -> Bool
-- hasSelfIntersections = BooleanSweep.hasIntersections . listEdges
-- FIXME: fix the open/closed bug, then switch to a boolean sweep based version
