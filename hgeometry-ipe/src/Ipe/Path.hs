{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Ipe.Path
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Defines an Ipe Path.
--
--------------------------------------------------------------------------------
module Ipe.Path(
    Path(Path), pathSegments
  , PathSegment(..)

  , _PolyLineSegment
  , _PolygonPath
  , _CubicBezierSegment
  , _QuadraticBezierSegment
  , _EllipseSegment
  , _ArcSegment
  , _SplineSegment
  , _ClosedSplineSegment

  , Operation(..)
  , _MoveTo
  , _LineTo
  , _CurveTo
  , _QCurveTo
  , _Ellipse
  , _ArcTo
  , _Spline
  , _ClosedSpline
  , _ClosePath
  ) where

import Control.Lens hiding (rmap)
import Data.Bitraversable
import Data.Traversable
import HGeometry.BezierSpline
import HGeometry.Ellipse (Ellipse)
import HGeometry.Matrix
import HGeometry.Point
import HGeometry.PolyLine
import HGeometry.Polygon.Simple
import HGeometry.Properties
import HGeometry.Transformation

--------------------------------------------------------------------------------
-- | Paths

-- | Paths consist of Path Segments. PathSegments come in the following forms:
data PathSegment r = PolyLineSegment        (PolyLine (Point 2 r))
                   | PolygonPath            (SimplePolygon (Point 2 r))
                   | CubicBezierSegment     (CubicBezier (Point 2 r))
                   | QuadraticBezierSegment (QuadraticBezier (Point 2 r))
                   | EllipseSegment         (Ellipse r)
                     -- TODO
                   | ArcSegment
                   | SplineSegment          -- (Spline 2 r)
                   | ClosedSplineSegment    -- (ClosedSpline 2 r)
                   deriving (Show,Eq)
makePrisms ''PathSegment

type instance NumType   (PathSegment r) = r
type instance Dimension (PathSegment r) = 2

instance Functor PathSegment where
  fmap = fmapDefault
instance Foldable PathSegment where
  foldMap = foldMapDefault
instance Traversable PathSegment where
  traverse f = \case
    PolyLineSegment p        -> PolyLineSegment <$> bitraverse pure f p
    PolygonPath p            -> PolygonPath <$> bitraverse pure f p
    CubicBezierSegment b     -> CubicBezierSegment <$> traverse f b
    QuadraticBezierSegment b -> QuadraticBezierSegment <$> traverse f b
    EllipseSegment e         -> EllipseSegment <$> traverse f e
    ArcSegment               -> pure ArcSegment
    SplineSegment            -> pure SplineSegment
    ClosedSplineSegment      -> pure ClosedSplineSegment

instance Fractional r => IsTransformable (PathSegment r) where
  transformBy t = \case
    PolyLineSegment p        -> PolyLineSegment $ transformBy t p
    PolygonPath p            -> PolygonPath $ transformBy t p
    CubicBezierSegment b     -> CubicBezierSegment $ transformBy t b
    QuadraticBezierSegment b -> QuadraticBezierSegment $ transformBy t b
    EllipseSegment e         -> EllipseSegment $ transformBy t e
    -- TODO:
    ArcSegment               -> ArcSegment
    SplineSegment            -> SplineSegment
    ClosedSplineSegment      -> ClosedSplineSegment


-- | A path is a non-empty sequence of PathSegments.
newtype Path r = Path { _pathSegments :: LSeq.LSeq 1 (PathSegment r) }
                 deriving (Show,Eq,Functor,Foldable,Traversable)
makeLenses ''Path

type instance NumType   (Path r) = r
type instance Dimension (Path r) = 2

instance Fractional r => IsTransformable (Path r) where
  transformBy t (Path s) = Path $ fmap (transformBy t) s


--------------------------------------------------------------------------------

-- | type that represents a path in ipe.
data Operation r = MoveTo (Point 2 r)
                 | LineTo (Point 2 r)
                 | Ellipse (Matrix 3 3 r)
                 | ArcTo (Matrix 3 3 r) (Point 2 r)
                 | Spline [Point 2 r]
                 | ClosedSpline [Point 2 r]
                 | ClosePath
                 -- these should be deprecated
                 | CurveTo (Point 2 r) (Point 2 r) (Point 2 r)
                 | QCurveTo (Point 2 r) (Point 2 r)
                 deriving (Eq, Show,Functor,Foldable,Traversable)
makePrisms ''Operation

