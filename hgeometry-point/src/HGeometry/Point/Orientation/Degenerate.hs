--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Point.Orientation.Degenerate
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- orientation tests, allowing for degeneracies
--
--------------------------------------------------------------------------------
module HGeometry.Point.Orientation.Degenerate(
    CCW(CCW,CW,CoLinear)
  , ccw
  , isCoLinear

  , sortAround

  , ccwCmpAroundWith
  , cwCmpAroundWith
  , ccwCmpAround
  , cwCmpAround

  , insertIntoCyclicOrder
  ) where


import qualified Data.CircularList as C
import qualified HGeometry.CircularList.Util as CU
import           HGeometry.Point.Class
import           HGeometry.Point.EuclideanDistance
-- import qualified HGeometry.Point.Optimal as Optimal
import           HGeometry.Vector
import qualified Data.List as L
import           R

--------------------------------------------------------------------------------

-- $setup
-- >>> import HGeometry.Point

-- | Data type for expressing the orientation of three points, with
-- the option of allowing Colinearities.
newtype CCW = CCWWrap Ordering deriving Eq

-- | CounterClockwise orientation. Also called a left-turn.
pattern CCW      :: CCW
pattern CCW      = CCWWrap GT

-- | Clockwise orientation. Also called a right-turn.
pattern CW       :: CCW
pattern CW       = CCWWrap LT

-- | CoLinear orientation. Also called a straight line.
pattern CoLinear :: CCW
pattern CoLinear = CCWWrap EQ
{-# COMPLETE CCW, CW, CoLinear #-}

instance Show CCW where
  show = \case
    CCW      -> "CCW"
    CW       -> "CW"
    CoLinear -> "CoLinear"


-- | Given three points p q and r determine the orientation when going from p to r via q.
--
-- Be wary of numerical instability:
-- >>> ccw (Point2 0 0.3) (Point2 1 0.6) (Point2 2 (0.9::Double))
-- CCW
--
-- >>> ccw (Point2 0 0.3) (Point2 1 0.6) (Point2 2 (0.9::Rational))
-- CoLinear
--
ccw       :: Point_ point 2 R
          => point -> point -> point -> CCW
ccw p q r = CCWWrap $ (ux*vy) `compare` (uy*vx)
-- ccw p q r = CCWWrap $ z `compare` 0 -- Comparing against 0 is bad for numerical robustness.
                                       -- I've added a testcase that fails if comparing against 0.
            -- case z `compare` 0 of
            --   LT -> CW
            --   GT -> CCW
            --   EQ -> CoLinear
     where
       Vector2 ux uy = q .-. p
       Vector2 vx vy = r .-. p
      --  _z             = ux * vy - uy * vx
{-# INLINE ccw #-}
     -- ccw :: (Ord r, Num r, OptCVector_ 2 r, OptMetric_ 2 r)
     --     => Optimal.Point 2 r -> Optimal.Point 2 r -> Optimal.Point 2 r -> CCW #-}

-- | Given three points p q and r determine if the line from p to r via q is straight/colinear.
--
-- This is identical to `ccw p q r == CoLinear` but doesn't have the `Ord` constraint.
isCoLinear       :: Point_ point 2 R => point -> point -> point -> Bool
isCoLinear p q r = (ux * vy) == (uy * vx)
     where
       Vector2 ux uy = q .-. p
       Vector2 vx vy = r .-. p

-- -- Given three points p q and r determine the orientation when going from p to r via q.
-- ccw' :: (Ord r, Num r) => Point 2 r :+ a -> Point 2 r :+ b -> Point 2 r :+ c -> CCW
-- ccw' = ccw -- p q r = ccw (p^.core) (q^.core) (r^.core)

-- | \( O(n log n) \)
-- Sort the points arround the given point p in counter clockwise order with
-- respect to the rightward horizontal ray starting from p.  If two points q
-- and r are colinear with p, the closest one to p is reported first.
sortAround   :: Point_ point 2 R
             => point -> [point] -> [point]
sortAround c = L.sortBy (ccwCmpAround c <> cmpByDistanceTo c)

-- | Given a zero vector z, a center c, and two points p and q,
-- compute the ccw ordering of p and q around c with this vector as zero
-- direction.
--
-- pre: the points p,q /= c
ccwCmpAroundWith                              :: Point_ point 2 R
                                              => Vector 2 R
                                              -> point
                                              -> point -> point
                                              -> Ordering
ccwCmpAroundWith z@(Vector2 zx zy) c q r =
    case (ccw c a q, ccw c a r) of
      (CCW,CCW)      -> cmp
      (CCW,CW)       -> LT
      (CCW,CoLinear) | onZero r  -> GT
                     | otherwise -> LT

      (CW, CCW)      -> GT
      (CW, CW)       -> cmp
      (CW, CoLinear) -> GT

      (CoLinear, CCW) | onZero q  -> LT
                      | otherwise -> GT

      (CoLinear, CW)      -> LT
      (CoLinear,CoLinear) -> case (onZero q, onZero r) of
                               (True, True)   -> EQ
                               (False, False) -> EQ
                               (True, False)  -> LT
                               (False, True)  -> GT
  where
    a = c .+^ z
    b = c .+^ Vector2 (-zy) zx
    -- b is on a perpendicular vector to z

    -- test if the point lies on the ray defined by z, starting in c
    onZero d = case ccw c b d of
                 CCW      -> False
                 CW       -> True
                 CoLinear -> True -- this shouldh appen only when you ask for c itself

    cmp = case ccw c q r of
            CCW      -> LT
            CW       -> GT
            CoLinear -> EQ

-- | Given a zero vector z, a center c, and two points p and q,
-- compute the cw ordering of p and q around c with this vector as zero
-- direction.
--
-- pre: the points p,q /= c
cwCmpAroundWith     :: Point_ point 2 R
                    => Vector 2 R
                    -> point
                    -> point -> point
                    -> Ordering
cwCmpAroundWith z c = flip (ccwCmpAroundWith z c)

-- | Counter clockwise ordering of the points around c. Points are ordered with
-- respect to the positive x-axis.
ccwCmpAround :: Point_ point 2 R
             => point -> point -> point -> Ordering
ccwCmpAround = ccwCmpAroundWith (Vector2 1 0)

-- | Clockwise ordering of the points around c. Points are ordered with
-- respect to the positive x-axis.
cwCmpAround :: Point_ point 2 R
            => point -> point -> point -> Ordering
cwCmpAround = cwCmpAroundWith (Vector2 1 0)

-- | \( O(n) \)
-- Given a center c, a new point p, and a list of points ps, sorted in
-- counter clockwise order around c. Insert p into the cyclic order. The focus
-- of the returned cyclic list is the new point p.
insertIntoCyclicOrder   :: Point_ point 2 R
                        => point -> point
                        -> C.CList point -> C.CList point
insertIntoCyclicOrder c = CU.insertOrdBy (ccwCmpAround c <> cmpByDistanceTo c)
