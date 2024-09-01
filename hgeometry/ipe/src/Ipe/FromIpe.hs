{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Ipe.FromIpe
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Functions that help reading geometric values from ipe images.
--
--------------------------------------------------------------------------------
module Ipe.FromIpe(
  -- * Individual readers
    _asPoint
  , _asLineSegment
  , _asClosedLineSegment
  , _asRectangle
  , _asTriangle

  , _asPolyLine
  , _asSimplePolygon
  , _asConvexPolygon
  -- , _asSomePolygon, _asSimplePolygon, _asMultiPolygon

  -- * Dealing with Attributes
  , _withAttrs

  -- * Default readers
  , HasDefaultFromIpe(..)

  -- * Reading all elements of a particular type
  , readAll, readAllFrom
  ) where

import           Control.Lens hiding (Simple)
import           Data.Kind (Type)
import qualified Data.Sequence as Seq
import           HGeometry.Ball
import           HGeometry.Box
import qualified HGeometry.Box as Box
import           HGeometry.Ellipse (Ellipse, _EllipseCircle)
import           HGeometry.Ext
import           HGeometry.LineSegment
import           HGeometry.Number.Radical
import           HGeometry.Point
import qualified HGeometry.PolyLine as PolyLine
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Simple
import           HGeometry.Properties
import           HGeometry.Triangle
import           Ipe.Path
import           Ipe.Reader
import           Ipe.Types
import           System.OsPath


--------------------------------------------------------------------------------

-- import qualified Data.List.NonEmpty as NonEmpty
-- import Ipe.Attributes
-- import Ipe.Color(IpeColor(..))
--------------------------------------------------------------------------------
-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Ipe.Attributes
-- >>> import Ipe.Color(IpeColor(..))
-- >>> import qualified Data.List.NonEmpty as NonEmpty
-- >>> :{
-- let testPath :: Path Int
--     testPath = Path . fromSingleton  . PolyLineSegment
--              . PolyLine.polyLineFromPoints . NonEmpty.fromList
--              $ [ origin, Point2 10 10, Point2 200 100 ]
--     testPathAttrs :: IpeAttributes Path Int
--     testPathAttrs = attr SStroke (IpeColor "red")
--     testObject :: IpeObject Int
--     testObject = IpePath (testPath :+ testPathAttrs)
-- :}

-- testPath :: Path Int
-- testPath = Path . fromSingleton  . PolyLineSegment
--              . PolyLine.polyLineFromPoints . NonEmpty.fromList
--              $ [ origin, Point2 10 10, Point2 200 100 ]
-- testPathAttrs :: IpeAttributes Path Int
-- testPathAttrs = attr SStroke (IpeColor "red")
-- testObject :: IpeObject Int
-- testObject = IpePath (testPath :+ testPathAttrs)



-- | Extracts the point from a Symbol. When creating a symbol this
-- creates a disk that supports a stroke color.
_asPoint :: Prism' (IpeSymbol r) (Point 2 r)
_asPoint = prism' (flip Symbol "mark/disk(sx)") (Just . view symbolPoint)

-- | Try to convert a path into a line segment, fails if the path is not a line
-- segment or a polyline with more than two points.
--
--
_asLineSegment :: Prism' (Path r) (LineSegment AnEndPoint (Point 2 r))
_asLineSegment = _asPolyLine.PolyLine._PolyLineLineSegment

-- | Try to convert a path into a line segment, fails if the path is not a line
-- segment or a polyline with more than two points.
--
--
_asClosedLineSegment :: Prism' (Path r) (ClosedLineSegment (Point 2 r))
_asClosedLineSegment = _asPolyLine.PolyLine._PolyLineLineSegment

-- | Convert to a polyline. Ignores all non-polyline parts
--
-- >>> testPath ^? _asPolyLine
-- Just (PolyLine [Point2 0 0,Point2 10 10,Point2 200 100])
_asPolyLine :: Prism' (Path r) (PolyLine.PolyLine (Point 2 r))
_asPolyLine = prism' poly2path path2poly
  where
    poly2path = Path . fromSingleton  . PolyLineSegment
    path2poly = preview (pathSegments.traverse._PolyLineSegment)
    -- TODO: Check that the path actually is a polyline, rather
    -- than ignoring everything that does not fit

-- | Convert to a simple polygon
_asSimplePolygon :: Prism' (Path r) (SimplePolygon (Point 2 r))
_asSimplePolygon = prism' polygonToPath pathToPolygon

-- | Convert to a convex polygon
_asConvexPolygon :: (Num r, Ord r) => Prism' (Path r) (ConvexPolygon (Point 2 r))
_asConvexPolygon = _asSimplePolygon._ConvexPolygon

-- | Tries to convert a path into a rectangle.
_asRectangle :: forall r. (Num r, Ord r) => Prism' (Path r) (Rectangle (Point 2 r))
_asRectangle = prism' rectToPath pathToRect
  where
    rectToPath (Box.corners -> Corners a b c d) =
      review _asSimplePolygon . uncheckedFromCCWPoints $ [a,b,c,d]
    pathToRect p = p^?_asSimplePolygon >>= asRect

    asRect    :: SimplePolygon (Point 2  r) -> Maybe (Rectangle (Point 2 r))
    asRect pg = case pg^..outerBoundary of
        [a,b,c,d]
          | isH a b && isV b c && isH c d && isV d a ->
              if a^.xCoord < b^.xCoord then Just (Rectangle a c)
                                       else Just (Rectangle c a)
          | isV a b && isH b c && isV c d && isH d a ->
              if a^.yCoord < b^.yCoord then Just (Rectangle d b)
                                       else Just (Rectangle b d)
        _                                            -> Nothing

    isH p q = p^.yCoord == q^.yCoord -- edge pq is horizontal
    isV p q = p^.xCoord == q^.xCoord -- edge pq is vertical




-- | Convert to a triangle
_asTriangle :: Prism' (Path r) (Triangle (Point 2 r))
_asTriangle = prism' triToPath path2tri
  where
    triToPath (Triangle p q r) = polygonToPath $ uncheckedFromCCWPoints [p,q,r]
    path2tri p = case p^..pathSegments.traverse._PolygonPath of
                    []   -> Nothing
                    [pg] -> case pg^..vertices of
                              [a,b,c] -> Just $ Triangle a b c
                              _       -> Nothing
                    _    -> Nothing


  -- an ellipse is an affine transformation of the unit disk


-- (Disk origin 1) (Vector2 1 1)

_asEllipse :: Prism' (Path r) (Ellipse r)
_asEllipse = prism' toPath toEllipse
  where
    toPath      = Path . fromSingleton  . EllipseSegment
    toEllipse p = case p^..pathSegments.traverse._EllipseSegment of
                    [e] -> Just e
                    _   -> Nothing

_asCircle :: (Radical r, Eq r) => Prism' (Path r) (Circle (Point 2 r))
_asCircle = _asEllipse._EllipseCircle
-- FIXME: For reading we should not need the Radical constraint!

_asDisk :: (Radical r, Eq r) => Prism' (Path r) (Disk (Point 2 r))
_asDisk = _asCircle.from _DiskCircle


-- -- | Convert to a multipolygon
-- _asMultiPolygon :: Prism' (Path r) (MultiPolygon () r)
-- _asMultiPolygon = _asSomePolygon._Right

-- _asPolygon :: Prism' (Path r) (forall t. Polygon t () r)
-- _asPolygon = prism' polygonToPath (fmap (either id id) . pathToPolygon)

-- _asSomePolygon :: Prism' (Path r) (SomePolygon () r)
-- _asSomePolygon = prism' embed pathToPolygon
--   where
--     embed     = either polygonToPath polygonToPath


polygonToPath :: SimplePolygon (Point 2 r) -> Path r
polygonToPath = Path . fromSingleton . PolygonPath


-- polygonToPath (MultiPolygon vs hs) = Path . LSeq.fromNonEmpty . fmap PolygonPath
--                                    $ vs :| hs

pathToPolygon   :: Path r -> Maybe (SimplePolygon (Point 2 r))
pathToPolygon p = case p^..pathSegments.traverse._PolygonPath of
                    [pg]  -> Just pg
                    _     -> Nothing
                    -- vs:hs -> Just . Right $ MultiPolygon vs hs





--------------------------------------------------------------------------------


-- | Use the first prism to select the ipe object to depicle with, and the second
-- how to select the geometry object from there on. Then we can select the geometry
-- object, directly with its attributes here.
--
-- >>> testObject ^? _withAttrs _IpePath _asPolyLine
-- Just (PolyLine [Point2 0 0,Point2 10 10,Point2 200 100] :+ Attrs {NoAttr, NoAttr, NoAttr, NoAttr, Attr IpeColor (Named "red"), NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr})
_withAttrs       :: Prism' (IpeObject r) (i r :+ IpeAttributes i r) -> Prism' (i r) g
                 -> Prism' (IpeObject r) (g :+ IpeAttributes i r)
_withAttrs po pg = prism' g2o o2g
  where
    g2o    = review po . over core (review pg)
    o2g o  = preview po o >>= \(i :+ ats) -> (:+ ats) <$> preview pg i





-- instance HasDefaultIpeObject Path where
--   defaultIpeObject' = _IpePath




class HasDefaultFromIpe g where
  type DefaultFromIpe g :: Type -> Type
  defaultFromIpe :: (r ~ NumType g)
                 => Prism' (IpeObject r) (g :+ IpeAttributes (DefaultFromIpe g) r)

instance HasDefaultFromIpe (Point 2 r) where
  type DefaultFromIpe (Point 2 r) = IpeSymbol
  defaultFromIpe = _withAttrs _IpeUse _asPoint

instance HasDefaultFromIpe (ClosedLineSegment (Point 2 r)) where
  type DefaultFromIpe (ClosedLineSegment (Point 2 r)) = Path
  defaultFromIpe = _withAttrs _IpePath _asClosedLineSegment

instance HasDefaultFromIpe (LineSegment AnEndPoint (Point 2 r)) where
  type DefaultFromIpe (LineSegment AnEndPoint (Point 2 r)) = Path
  defaultFromIpe = _withAttrs _IpePath _asLineSegment

instance HasDefaultFromIpe (Ellipse r) where
  type DefaultFromIpe (Ellipse r) = Path
  defaultFromIpe = _withAttrs _IpePath _asEllipse

instance (Radical r, Eq r) => HasDefaultFromIpe (Circle (Point 2 r)) where
  type DefaultFromIpe (Circle (Point 2 r)) = Path
  defaultFromIpe = _withAttrs _IpePath _asCircle

instance (Radical r, Eq r) => HasDefaultFromIpe (Disk (Point 2 r)) where
  type DefaultFromIpe (Disk (Point 2 r)) = Path
  defaultFromIpe = _withAttrs _IpePath _asDisk

instance HasDefaultFromIpe (PolyLine.PolyLine (Point 2 r)) where
  type DefaultFromIpe (PolyLine.PolyLine (Point 2 r)) = Path
  defaultFromIpe = _withAttrs _IpePath _asPolyLine


instance HasDefaultFromIpe (SimplePolygon (Point 2 r)) where
  type DefaultFromIpe (SimplePolygon (Point 2 r)) = Path
  defaultFromIpe = _withAttrs _IpePath _asSimplePolygon

-- instance HasDefaultFromIpe (MultiPolygon () r) where
--   type DefaultFromIpe (MultiPolygon () r) = Path
--   defaultFromIpe = _withAttrs _IpePath _asMultiPolygon

instance (Num r, Ord r) => HasDefaultFromIpe (Rectangle (Point 2 r)) where
  type DefaultFromIpe (Rectangle (Point 2 r)) = Path
  defaultFromIpe = _withAttrs _IpePath _asRectangle


-- | Read all g's from some ipe page(s).
readAll   :: forall g r. (HasDefaultFromIpe g, r ~ NumType g)
          => IpePage r -> [g :+ IpeAttributes (DefaultFromIpe g) r]
readAll p = p^..content.traverse.defaultFromIpe

-- | Convenience function from reading all g's from an ipe file. If there
-- is an error reading or parsing the file the error is "thrown away".
readAllFrom    :: forall g r. (HasDefaultFromIpe g, r ~ NumType g, Coordinate r, Eq r)
               => OsPath -> IO [g :+ IpeAttributes (DefaultFromIpe g) r]
readAllFrom fp = foldMap readAll <$> readSinglePageFile fp

fromSingleton :: a -> Seq.Seq a
fromSingleton = Seq.singleton