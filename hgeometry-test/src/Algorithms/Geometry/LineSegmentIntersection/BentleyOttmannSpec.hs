{-# LANGUAGE OverloadedStrings          #-}
module Algorithms.Geometry.LineSegmentIntersection.BentleyOttmannSpec (spec) where

import           Algorithms.Geometry.LineSegmentIntersection
import qualified Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann as Sweep
import qualified Algorithms.Geometry.LineSegmentIntersection.Naive as Naive
import           Control.Lens
import           Data.Ext
import           Ipe
import           Data.Geometry.LineSegment
import           Data.Geometry.Polygon
import qualified Data.Map as Map
import           Data.Proxy
import           Paths_hgeometry_test
import           Test.Hspec

spec :: Spec
spec = do
  describe "Testing Bentley Ottmann LineSegment Intersection" $ do
    -- toSpec (TestCase "myPoints" myPoints)
    -- toSpec (TestCase "myPoints'" myPoints')
    ipeSpec

testPath = "src/Algorithms/Geometry/LineSegmentIntersection/"

ipeSpec :: Spec
ipeSpec = do testCases (testPath <> "manual.ipe")
             testCases (testPath <> "open.ipe")

testCases    :: FilePath -> Spec
testCases fp = (runIO $ readInput =<< getDataFileName fp) >>= \case
    Left e    -> it "reading LineSegment Intersection file" $
                   expectationFailure $ "Failed to read ipe file " ++ show e
    Right tcs -> mapM_ toSpec tcs


-- | Point sets per color, Crosses form the solution
readInput    :: FilePath -> IO (Either ConversionError [TestCase Rational])
readInput fp = fmap f <$> readSinglePageFile fp
  where
    f page = [TestCase segs]
      where
        segs = page^..content.traverse._IpePath.core._asLineSegment



data TestCase r = TestCase { _segments :: [LineSegment 2 () r]
                           } deriving (Show,Eq)


toSpec                 :: (Fractional r, Ord r, Show r) => TestCase r -> Spec
toSpec (TestCase segs) = describe ("testing segments ") $ do
                            samePointsAsNaive segs
                            sameAsNaive segs

-- | Test if we have the same intersection points
samePointsAsNaive segs = it "Same points as Naive" $ do
  (Map.keys $ Sweep.intersections segs) `shouldBe` (Map.keys $ Naive.intersections segs)

-- | Test if they every intersection point has the right segments
sameAsNaive      :: (Fractional r, Ord r, Eq p
                    , Show p, Show r
                    ) => [LineSegment 2 p r] -> Spec
sameAsNaive segs = it "Same as Naive " $ do
    (Sweep.intersections segs) `shouldBe` (Naive.intersections segs)
