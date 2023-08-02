--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.RangeTree.Base
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Range Tree implementation
--
--------------------------------------------------------------------------------
module HGeometry.RangeTree.Base
  ( RangeTree
  , buildRangeTree
  , fromAscList
  , fromGroupedAscList

  , rangeQuery
  , query
  ) where


import           Control.Lens
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Monoid (Sum(..))
import           Data.Type.Ord
import qualified Data.Vector as Vector
import           Data.Vector.NonEmpty.Internal (NonEmptyVector(..))
import           HGeometry.Foldable.Sort
import           HGeometry.Intersection
import           HGeometry.Interval
import           HGeometry.Measured
import           HGeometry.Measured.Size
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.SegmentTree.CanonicalSubSet
import           HGeometry.Tree.Binary.Static
import           HGeometry.Vector.NonEmpty.Util ()

--------------------------------------------------------------------------------

-- | Type Representing a generic (1D) range-tree.
newtype RangeTree f point = RangeTree (SubTree f point)
  deriving (Show,Eq,Functor,Foldable,Traversable)

-- | The actual tree type
type SubTree f point = BinLeafTree (NodeData f point (NumType point))
                                   (LeafData f point (NumType point) )


-- | Data stored at a leaf
data LeafData f point r = LeafData { _thePoint      :: !r
                                   , _leafCanonical :: !(f point)
                                   } deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

-- | Access the point stored in this leaf.
thePoint :: Lens (LeafData f point r) (LeafData f point s) r s
thePoint = lens _thePoint (\ld p -> ld { _thePoint = p})

instance HasCanonicalSubset (LeafData f point r) (LeafData g point r) point f g where
  canonicalSubset = lens _leafCanonical (\(LeafData p _) cs -> LeafData p cs)

data NodeData f point r = NodeData { _split           :: !r
                                   , _canonicalSubSet :: !(f point)
                                   } deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

instance HasCanonicalSubset (NodeData f point r) (NodeData g point r) point f g where
  canonicalSubset = lens _canonicalSubSet (\(NodeData x _) cs -> NodeData x cs)


buildRangeTree :: ( Foldable1 g, Point_ point d r, Ord r, 1 <= d
                  , Semigroup (f a), Measured f a
                  ) => g point -> RangeTree f point
buildRangeTree = fromAscList . NonEmptyVector . sortOnCheap (^.asPoint)

-- | Given the points in sorted order, builds the range tree.
--
-- The running time and space depends on the monoid used. In particlar, we use (<>) at
-- every internal node.
fromAscList :: ( Foldable1 g, Functor g, Point_ point 1 r
               , Semigroup (f a), Measured f a
               ) => g point -> RangeTree f point
fromAscList = fromGroupedAscList . fmap (:| [])

-- | Given the points in groups per x-coordinate, in increasing order of x-coordinate
fromGroupedAscList :: ( Foldable1 g, Foldable1 h, Point_ point 1 r
                      , Semigroup (f a), Measured f a
                      ) => g (h point) -> RangeTree f point
fromGroupedAscList = RangeTree . fst . foldUp node' leaf' . asBalancedBinLeafTree
  where
    leaf' pts            = let p = first1Of folded pts
                               x = p^.xCoord
                               m = foldMap measure pts
                           in (LeafData x m, x)
    node' (l,m) _ (r,m') = ( Node l (NodeData m $ l^.canonicalSubset <> r^.canonicalSubset) r
                           , m'
                           )

-- | Report the canonical subsets of the nodes that together represent the query interval.
--
-- \(O(\log n)\)
rangeQuery                  :: (Interval_ interval r, Ord r)
                            => interval -> RangeTree f point -> [f point]
rangeQuery q (RangeTree t0) = findSplitNode t0
  where
    --
    findSplitNode t = case t of
      Leaf ld     -> goReport ld
      Node l nd r -> case (nd^.split) `compareInterval` q of
                       LT -> findSplitNode l
                       EQ -> goReportR l <> goReportL r
                       GT -> findSplitNode r

    -- | Report a leaf (if needed)
    goReport ld
        | Point1 (ld^.thePoint) `intersects` q = [ld^.canonicalSubset]
        | otherwise                            = []

    -- | walk the left-path reporting the right subtrees
    goReportR = \case
      Leaf ld     -> goReport ld
      Node l nd r -> case (nd^.split) `compareInterval` q of
                       LT -> reportSubTree r : goReportR l
                       EQ -> reportSubTree r : goReportR l
                       GT -> goReportR r

    -- | walk the right-path reporting left subtrees
    goReportL = \case
      Leaf ld     -> goReport ld
      Node l nd r -> case (nd^.split) `compareInterval` q of
                       LT -> goReportR l
                       EQ -> reportSubTree l : goReportL r
                       GT -> reportSubTree l : goReportL r

    -- | report the canonical subset of the node
    reportSubTree = \case
      Leaf ld     -> ld^.canonicalSubSet
      Node _ nd _ -> nd^.canonicalSubSet


-- | Query the range tree
--
-- \(O(\log n + k)\), where \(k\) is somehow depends on the output size (and the monoid
-- used).
query   :: (Interval_ interval r, Ord r, Monoid (f interval))
        => interval -> RangeTree f point -> f point
query q = mconcat . queryNodes q
