module HGeometry.HalfSpaceSpec
  (spec) where

import HGeometry.HalfSpace
import HGeometry.HyperPlane
import HGeometry.Line
import HGeometry.Point
import HGeometry.Intersection
import Test.Hspec

--------------------------------------------------------------------------------

type R = Double

myHalfspace :: HalfSpaceF (LineEQ R)
myHalfspace = HalfSpace Positive (LineEQ 1 2)

myPoints :: [(Point 2 R, Bool)]
myPoints = [ (Point2 10 10, False)
           , (Point2 10 1000, True)
           , (Point2 0 20, True)
           , (Point2 0 2, True)
           ]

spec :: Spec
spec = describe "halfspace Tests" $ do
         it "in halfspace" $ do
           mapM_ (\(q,ans) -> (q `intersects` myHalfspace) `shouldBe` ans) myPoints


         -- it "intersect tests" $ do
         --   let h = HalfSpace Positive $ horizontalLine (4 % 1 :: Rational)
         --       l = LinePV origin (Vector2 (1 % 1) (1 % 1 :: Rational))
         --   ((horizontalLine @Rational $ 5 % 1) `intersects` h) `shouldBe` True
         --   (l `intersects` h) `shouldBe` True
