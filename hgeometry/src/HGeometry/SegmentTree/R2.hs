{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.SegmentTree.R2
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Segment Tree specifically implemented for 2D segment stabbing/reporting/counting
-- queries.
--
--------------------------------------------------------------------------------
module HGeometry.SegmentTree.R2
  ( SegmentTree2

  ) where


import           Control.Lens
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Monoid (Sum(..))
-- import           Data.Set (Set)
-- import qualified Data.Set as Set
import qualified Data.Vector as Vector
import           Data.Vector.NonEmpty.Internal (NonEmptyVector(..))
import           HGeometry.Foldable.Sort
import           HGeometry.Intersection
import           HGeometry.Interval
import           HGeometry.LineSegment
import           HGeometry.Measured.Report
import           HGeometry.Point
import           HGeometry.Vector
import           HGeometry.Properties
import qualified HGeometry.RangeTree.Base as RangeTree
import qualified HGeometry.SegmentTree.Base as Base
import           HGeometry.Tree.Binary.Static
import           HGeometry.Vector.NonEmpty.Util ()
import           Data.Reflection
import           Data.Proxy
import           Data.Coerce
import           HGeometry.Properties
import           Data.Type.Ord

--------------------------------------------------------------------------------


newtype OnY s r segment = OnY segment
                        deriving (Show)

type instance NumType (OnY s r segment) = OnY s r segment
type instance Dimension (OnY s r segment) = 1

-- instance Reifies s r => Point_ (OnY s r segment) 1 r where

instance HasVector (OnY s r segment) (OnY s r segment) where
  vector = iso Vector1 (\(Vector1 v) -> v)

instance HasCoordinates (OnY s r segment) (OnY s r segment)
instance Affine_ (OnY s r segment) 1 (OnY s r segment) where
instance Point_ (OnY s r segment) 1 (OnY s r segment) where
  fromVector = error "OnY.fromVector not implemented"

-- instance Reifies s r => Point_ (OnY s r segment) 1 r where


instance (Ord r
         , LineSegment_ lineSegment point, Point_ point 2 r
         , Reifies s r) => Eq (OnY s r segment) where
  (OnY s) == (OnY s') = s `compare` s' == EQ

instance ( Ord r, Reifies s r
         , LineSegment_ lineSegment point, Point_ point 2 r
         ) => Ord (OnY s r segment) where
  (OnY s) `compare` (OnY s') = ordAtX x s s'
    where
      x = reflect (Proxy @r)


data VerticallyOrdered s r f segment =
    VEmpty
  | VerticallyOrdered (RangeTree.RangeTree f segment)

instance Reifies s r => Semigroup (VerticallyOrdered s r f sement) where
  VEmpty                <> r                     = r
  l                     <> VEmpty                = l
  (VerticallyOrdered l) <> (VerticallyOrdered r) = VerticallyOrdered undefined

instance Reifies s r => Monoid (VerticallyOrdered s r f sement) where
  mempty = VEmpty


buildAssoc                :: forall f segment r s.
                             Report (XInterval r segment) -> VerticallyOrdered s r f segment
buildAssoc (MkReport mxs) = case mxs of
    Nothing   -> VEmpty
    Just ints -> VerticallyOrdered . RangeTree.buildRangeTree $ coerce ints


  -- deriving (Show,Eq)


-- rangeTreeAtX :: VerticallyOrdered f segment r -> RangeTree.RangeTree f (OnY s segment)



queryAssoc                           :: forall f segment yInterval r s.
                                        (Interval_ yInterval r, Ord r)
                                     => yInterval -> VerticallyOrdered s r f segment
                                     -> [f segment]
queryAssoc q (VerticallyOrdered t) = undefined -- (coerce . runQuery)
  where
    runQuery    :: Proxy s -> [f (OnY s r segment)]
    runQuery px = undefined -- RangeTree.rangeQuery q t'
      where
        t' = undefined -- coerce t


newtype SegmentTree2 f segment =
  SegmentTree2 (Base.SegmentTree (VerticallyOrdered () (NumType segment) f)
                                 (XInterval (NumType segment) segment)
               )

newtype XInterval r segment = XInterval segment
  deriving (Show,Eq)

_XIntervalSegment :: Iso (XInterval r segment) (XInterval r segment') segment segment'
_XIntervalSegment = coerced

type instance NumType (XInterval r segment) = r


type instance StartPointOf (XInterval r segment) = EndPoint Closed r
type instance EndPointOf   (XInterval r segment) = EndPoint Closed r

instance ( LineSegment_ segment point, Point_ point d r, 1 <= d
         ) => HasStart (XInterval r segment) r where
  start = _XIntervalSegment.start.xCoord
instance ( ClosedLineSegment_ segment point, Point_ point d r, 1 <= d
         ) => HasStartPoint (XInterval r segment) (EndPoint Closed r) where
  startPoint = _XIntervalSegment.startPoint.project

project :: (Point_ point d r, 1 <= d) => Lens' (EndPoint epType point) (EndPoint epType r)
project = lens (\p   -> p&_endPoint %~ view xCoord)
               (\p x -> p&_endPoint.xCoord .~ (x^._endPoint))

instance ( LineSegment_ segment point, Point_ point d r, 1 <= d
         ) => HasEnd (XInterval r segment) r where
  end = _XIntervalSegment.end.xCoord

instance ( ClosedLineSegment_ segment point, Point_ point d r, 1 <= d
         ) => HasEndPoint (XInterval r segment) (EndPoint Closed r) where
  endPoint = _XIntervalSegment.endPoint.project

instance ( ClosedLineSegment_ segment point, Point_ point d r, 1 <= d
         ) => IntervalLike_ (XInterval r segment) r where
  mkInterval = error "we cannot create XIntervals out of thin air"

instance ( ClosedLineSegment_ segment point, Point_ point d r, 1 <= d
         ) => Interval_ (XInterval r segment) r
instance ( ClosedLineSegment_ segment point, Point_ point d r, 1 <= d
         ) => ClosedInterval_ (XInterval r segment) r






buildSegmentTree :: forall f segment point r g. ( Foldable1 g
                    , ClosedLineSegment_ segment point
                    , Point_ point 2 r, Ord r
                    ) => g segment -> SegmentTree2 f segment
buildSegmentTree = undefined

  -- SegmentTree2 . Base.buildSegmentTree
  --                . coerce


buildSementTree' :: ( Foldable1 g, Functor g
                    , ClosedLineSegment_ segment point
                    , Point_ point 2 r, Ord r
                    ) => g segment -> Base.SegmentTree Report (XInterval r segment)
buildSementTree' = Base.buildSegmentTree . fmap coerce
