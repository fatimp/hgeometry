{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import           Control.Lens
import           Data.Data
import qualified Data.Foldable as F
import           Data.Maybe (mapMaybe)
import           Data.Semigroup
import           HGeometry.Ext
import           HGeometry.LineSegment
import           HGeometry.LineSegment.Intersection.BentleyOttmann (interiorIntersections)
import           HGeometry.Number.Real.Rational
import           HGeometry.PlaneGraph
import           HGeometry.Point
import           HGeometry.Polygon.Simple
import           HGeometry.Polygon.Triangulation (triangulate)
import           HGeometry.Polygon.Triangulation.MakeMonotone (makeMonotone)
import           Ipe
import           Options.Applicative
import           System.OsPath

--------------------------------------------------------------------------------

type R = RealNumber 5

data Options = Options { _inPath    :: OsPath
                       , _outFile   :: OsPath
                       }
               -- deriving Data

options :: ParserInfo Options
options = info (helper <*> parser)
               (  progDesc "Triangulate all polygons in the input file."
               <> header   "trianguldateWorld"
               )
  where
    parser = Options
          <$> strOption (help "Input file (in ipe7 xml format)"
                         <> short 'i'
                        )
          <*> strOption (help "Output File (in ipe7 xml format)"
                         <> short 'o'
                        )

data PX = PX

main :: IO ()
main = execParser options >>= mainWith

mainWith                          :: Options -> IO ()
mainWith (Options inFile outFile) = do
    ePage <- readSinglePageFile inFile
    case ePage of
      Left err                  -> print err
      Right (page :: IpePage R) -> runPage page
  where
    runPage page = do
      let polies  :: [SimplePolygon (Point 2 R) :+ IpeAttributes Path R]
          polies  = readAll page
          polies' = filter (hasNoSelfIntersections . (^.core)) polies
          intersections' = foldMap (intersectionPoints . interiorIntersections
                                      . view (core.edgeSegments)) polies

          subdivs :: [PlaneGraph PX (Point 2 R) _ _]
          subdivs = map (\(pg :+ _) -> triangulate pg) polies'

          triangles' :: [SimplePolygon (Point 2 R :+ _)]
          triangles' = subdivs^..traverse.interiorFacePolygons
            -- mapMaybe (^?_2.core._Left)
            --          . concatMap (F.toList. internalFacePolygons) $ subdivs

          segs :: [ClosedLineSegment (Point 2 R)]
          segs = subdivs^..traverse.edgeSegments

          out     = mconcat [ [ iO' pg | pg <- polies ]
                            , [ iO' s  | s  <- segs ]
                            , [ iO' pg | pg <- triangles' ]
                            ]
      putStrLn $ "#polygons found: " <> show (length polies)

      putStrLn $ "first <=100 self-intersections: "
      mapM_ print $ take 100 intersections'
      putStrLn $ "number of non-self intersecting polygons: " <> show (length polies')

      mapM_ (print . numVertices) polies'

      putStrLn "# triangles: "
      print (length $ triangles')
      -- writeIpeFile outFile . singlePageFromContent $ out
