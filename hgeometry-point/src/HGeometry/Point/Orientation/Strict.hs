--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Point.Orientation.Strict
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Strict orientation tests using SoS
--
--------------------------------------------------------------------------------
module HGeometry.Point.Orientation.Strict
  ( StrictCCW
  , pattern SCCW, pattern SCW
  , strictCcw
  ) where

import HGeometry.Indexed
import HGeometry.Point.Class
import HGeometry.Sign
import HGeometry.SoS.Orientation
import HGeometry.Vector

--------------------------------------------------------------------------------

-- $setup
-- >>> import HGeometry.Point
-- >>> :{
-- let with     :: Point 2 Int -> Int -> WithIndex (Point 2 Int)
--     with p i = WithIndex i p
-- :}


--------------------------------------------------------------------------------

newtype StrictCCW = MkSCCW Sign deriving Eq

pattern SCCW :: StrictCCW
pattern SCCW = MkSCCW Negative

pattern SCW  :: StrictCCW
pattern SCW  = MkSCCW Positive
{-# COMPLETE SCCW, SCW #-}

instance Show StrictCCW where
  show = \case
    SCCW -> "SCCW"
    SCW  -> "SCW"

-- | Given three points p q and r determine the orientation when going
-- from p to r via q. Uses SoS to disambiguate for colinear points.
--
-- >>> strictCcw (Point2 0 0 `with` 1) (Point2 2 2 `with` 3) (Point2 1 2 `with` 0)
-- SCCW
-- >>> strictCcw (Point2 0 0 `with` 1) (Point2 2 2 `with` 3) (Point2 1 (-2) `with` 0)
-- SCW
-- >>> strictCcw (Point2 0 0 `with` 1) (Point2 2 2 `with` 3) (Point2 1 1 `with` 0)
-- SCW
-- >>> strictCcw (Point2 0 0 `with` 1) (Point2 2 2 `with` 3) (Point2 1 1 `with` 10)
-- SCCW
-- >>> strictCcw (Point2 0 0 `with` 3) (Point2 2 2 `with` 1) (Point2 1 1 `with` 10)
-- SCCW
strictCcw      :: ( Num r, Ord r, Point_ point 2 r, HasIndex point)
                => point -> point -> point -> StrictCCW
strictCcw p q r = MkSCCW . flipSign $ sideTest r (Vector2 p q)
