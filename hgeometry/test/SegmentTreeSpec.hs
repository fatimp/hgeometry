module SegmentTreeSpec where

import qualified Data.Foldable as F
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import           HGeometry.Instances ()
import           HGeometry.Interval
import           HGeometry.SegmentTree
import           Test.Hspec
import           Test.Hspec.QuickCheck

--------------------------------------------------------------------------------

type R = Int

spec :: Spec
spec = describe "segmentTree tests" $ do
         it "manual report" $
           query 5 testTree `shouldBe` (Report [ClosedInterval 0 15])
         it "manual count" $
           query 15 testTree `shouldBe` (Count 2)
         prop "same as naive" $
           \(qs :: [Int]) (ints :: NonEmpty (ClosedInterval Int)) ->
             let t = buildSegmentTree ints
             in all (\q -> let Report res = query q t
                           in Set.fromList res == naiveQuery q ints
                    ) qs

         -- prop: at most an int in at most 2 canonical subsets per level


naiveQuery        :: (Ord r, ClosedInterval_ interval r, Ord interval, Foldable f)
                  => r -> f interval -> Set.Set interval
naiveQuery q ints = Set.fromList $ filter (q `stabsInterval`) $ F.toList ints


testTree :: ( Monoid (f (ClosedInterval Int)), MeasureF f (ClosedInterval Int)
            ) => SegmentTree f (ClosedInterval Int)
testTree = buildSegmentTree myIntervals

myIntervals = NonEmpty.fromList
              [ ClosedInterval 10 20
              , ClosedInterval 23 30
              , ClosedInterval 0  15
              ]
