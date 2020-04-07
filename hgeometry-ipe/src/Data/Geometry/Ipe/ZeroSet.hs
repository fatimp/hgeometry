module Data.Geometry.Ipe.ZeroSet where

import           Control.Lens
import           Data.Ext
import           Data.Foldable
import           Data.Geometry.Box
-- import qualified Data.Geometry.CatmulRomSpline as CatmulRom
import           Data.Geometry.Ipe.Attributes
import           Data.Geometry.Ipe.Color
import           Data.Geometry.Ipe.IpeOut
import           Data.Geometry.Ipe.Types
import           Data.Geometry.Point
import           Data.Geometry.PolyLine (PolyLine)
import qualified Data.Geometry.PolyLine as PolyLine
import           Data.Geometry.QuadTree
import           Data.Geometry.QuadTree.Cell
import           Data.Geometry.QuadTree.Quadrants
import           Data.Geometry.QuadTree.Split
import           Data.Geometry.QuadTree.Tree (Tree(..))
import qualified Data.Geometry.QuadTree.Tree as Tree
import           Data.Geometry.Vector
import           Data.Intersection
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.RealNumber.Rational

import           Data.Geometry.Ipe.Writer

import           Debug.Trace

--------------------------------------------------------------------------------

type R = RealNumber 10


-- drawSpline :: Fractional r => IpeOut (CatmulRom.Spline 2 r) Path r
-- drawSpline = \s -> toCubicBezier s


drawCell :: Fractional r => IpeOut (Cell r) Path r
drawCell = \c -> ipeRectangle (toBox c)

-- drawQuadTree' ::

drawQuadTree :: (Fractional r, Ord r) => IpeOut (QuadTree v p) Group r
drawQuadTree = drawQuadTreeWith (\(_ :+ c) -> drawCell c)

drawQuadTreeWith           :: (ToObject i, Fractional r, Ord r)
                           => IpeOut (p :+ Cell r) i r -> IpeOut (QuadTree v p) Group r
drawQuadTreeWith drawCell' = ipeGroup . fmap (iO . drawCell') . leaves . withCells

drawZeroCell :: Fractional r => IpeOut (Either v Sign :+ Cell r) Path r
drawZeroCell = \(p :+ c) -> case p of
                              Left _     -> drawCell c ! attr SFill blue
                              Right Zero -> drawCell c ! attr SFill green
                              Right _    -> drawCell c


test' = writeIpeFile "/tmp/test.ipe" . singlePageFromContent $
        [ iO $ drawQuadTreeWith drawZeroCell testT
        ,
          iO $ defIO pl
        ]
  where
    Just pl = traceZeroFrom Zero myStartP testT


testT :: QuadTree (Quadrants Sign) (Either (Quadrants Sign) Sign)
testT = fromZeros (Cell 8 origin) (\q -> (r^2) - squaredEuclideanDist origin (realToFrac <$> q))
  where
    r = 90.5 :: R -- draw circle of radius r


centerPoints :: (Functor f, Fractional r) => f (p :+ Cell r) -> f (Point 2 r :+ p)
centerPoints = fmap (\(p :+ c) -> midPoint c :+ p)

--
traceZeroFrom            :: forall zero r b v. (Eq zero, Fractional r, Ord r)
                         => zero -- ^ the zero value
                         -> Point 2 r -> QuadTree v (Either b zero)
                         -> Maybe (PolyLine 2 (Either b zero) r)
traceZeroFrom zero' p qt = do startCell <- findLeaf p qt
                              let zCells = NonEmpty.filter (\(p :+ _) -> isZeroCell zero' p)
                                         . leaves . withCells $ qt
                                  path   = explorePathWith (const True) startCell zCells
                              PolyLine.fromPoints . NonEmpty.toList . centerPoints $ path
                              -- CatmulRom.fromListOfPoints . NonEmpty.toList . centerPoints $ path





-- startCell :: Maybe (Either (Quadrants Sign) Sign :+ Cell)
-- startCell = findLeaf myStartP testT

trace startP qt = case findLeaf startP qt of
                    Nothing        -> []
                    Just startCell -> NonEmpty.toList $ explorePathWith (const True) startCell zCells
  where
    zCells = NonEmpty.filter (\(p :+ _) -> isZeroCell Zero p)
           . leaves $ qt'
    qt' = withCells qt
    -- start =

    -- [start] = filter (\(_ :+ c) -> startP `intersects` c) zCells



-- testTrace = trace myStartP testT

myStartP = (Point2 0 (90.5 :: Rational))

-- [startX] = filter (\(_ :+ c) -> myStartP `intersects` c) zCells

-- findS :: [(p :+ Cell) :+ e] -> [(p :+ Cell) :+ e]
-- findS = filter (\((_ :+ c) :+ _) -> myStartP `intersects` c)


-- zCells = NonEmpty.filter (\(p :+ _) -> isZeroCell p)
--        . leaves . withCells $ testT




-- testTrace =