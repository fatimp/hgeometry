module Vector2Spec where

import           Control.Lens
import           HGeometry.Vector.Generic
import           HGeometry.Vector.V2
import qualified HGeometry.Vector.V3 as V3
import qualified HGeometry.Vector.V1 as V1
import           R
import           Test.Hspec

--------------------------------------------------------------------------------

myVec :: Vector
myVec = Vector2_ 5 11

spec :: Spec
spec = describe "vector2 tests" $ do
         it "components" $
           myVec^..components `shouldBe` [5,11]
         it "component" $ do
           myVec^.(component @0) `shouldBe` 5
           myVec^.(component @1) `shouldBe` 11
         it "xComponent" $
           myVec^.xComponent `shouldBe` 5
         it "yComponent" $
           myVec^.yComponent `shouldBe` 11
         it "add" $
           myVec ^+^ myVec `shouldBe` Vector2_ 10 22
         it "dot" $
           myVec `dot` Vector2_ 100 2 `shouldBe` 522
         it "vectorFromList" $ do
           V1.vectorFromList [10]   `shouldBe` Just (V1.Vector1_ 10)
          -- vectorFromList @V3.Vector [10,2,3]   `shouldBe` Just (V3.Vector3_ 10 2 3)
--           vectorFromList @V3.Vector [10,2,3,5] `shouldBe` Nothing
--           vectorFromList @V3.Vector [10,2]     `shouldBe` Nothing
