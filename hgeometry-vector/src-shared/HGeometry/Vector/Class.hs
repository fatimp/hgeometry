module HGeometry.Vector.Class
  ( Vector_
  ) where


import           Data.Type.Ord
-- import HGeometry.Properties

-- | Declare that the type vector represents a d dimensional vector
-- whose coordinates are of type r.
--
-- This class does not have any methods, since all such methods are
-- provided using a backpack Signature instead.
class 0 < d => Vector_ vector d r | vector -> d
                                  , vector -> r
