{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Ipe.IpeOut
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Functions that help drawing geometric values in ipe. An "IpeOut" is
-- essenitally a function that converts a geometric type g into an IpeObject.
--
-- We also proivde a "HasDefaultIpeOut" typeclass that defines a default
-- conversion function from a geometry type g to an ipe type.
--
--------------------------------------------------------------------------------
module Ipe.IpeOut where

import           Control.Lens hiding (Simple)
import           Data.Bifunctor
import           Data.Ext
import           Data.Foldable (toList)
import           Data.Kind
import qualified Data.LSeq as LSeq
import           Data.List.NonEmpty (NonEmpty(..))
import           HGeometry.Number.Radical
import           Geometry.Ball
import           Geometry.BezierSpline
import           Geometry.Boundary
import           Geometry.Box
import           Geometry.Ellipse (Ellipse, circleToEllipse)
import           Geometry.HalfLine
import           Geometry.Line
import           Geometry.LineSegment
import           Geometry.Point
import           Geometry.PolyLine (PolyLine,fromLineSegment)
import           Geometry.Polygon
import           Geometry.Polygon.Convex
import           Geometry.Properties
import           Ipe.Attributes
import           Ipe.Color (IpeColor(..))
import           Ipe.FromIpe
import           Ipe.Types

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Vinyl (Rec(..))
import           Data.Vinyl.CoRec
import           Ipe.Content (IpeAttributes)


--------------------------------------------------------------------------------

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :{
-- let myPolygon = fromPoints . map ext $ [origin, Point2 10 10, Point2 100 200]
-- :}

--------------------------------------------------------------------------------
-- * The IpeOut type and the default combinator to use it

type IpeOut g i r = g -> IpeObject' i r

-- | Give the option to draw zero, one or more things, i.e. by
-- choosing f ~ Maybe or f ~ []
type IpeOut' f g i r = g -> f (IpeObject' i r)


-- | Add attributes to an IpeObject'
(!)       :: IpeObject' i r -> IpeAttributes i r -> IpeObject' i r
(!) i ats = i&extra %~ (<> ats)

-- | Render an ipe object
--
--
-- >>> :{
--   iO $ defIO myPolygon ! attr SFill (IpeColor "blue")
--                        ! attr SLayer "alpha"
--                        ! attr SLayer "beta"
-- :}
-- IpePath (Path {_pathSegments = LSeq (fromList [PolygonPath SimplePolygon CSeq [Point2 [0,0] :+ (),Point2 [10,10] :+ (),Point2 [100,200] :+ ()]])} :+ Attrs {Attr LayerName {_layerName = "beta"}, NoAttr, NoAttr, NoAttr, NoAttr, Attr IpeColor (Named "blue"), NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr})
--
-- >>> :{
--   iO $ ipeGroup [ iO $ ipePolygon myPolygon ! attr SFill (IpeColor "red")
--                 ] ! attr SLayer "alpha"
-- :}
-- IpeGroup (Group [IpePath (Path {_pathSegments = LSeq (fromList [PolygonPath SimplePolygon CSeq [Point2 [0,0] :+ (),Point2 [10,10] :+ (),Point2 [100,200] :+ ()]])} :+ Attrs {NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, Attr IpeColor (Named "red"), NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr})] :+ Attrs {Attr LayerName {_layerName = "alpha"}, NoAttr, NoAttr, NoAttr, NoAttr})
--
iO :: ToObject i => IpeObject' i r -> IpeObject r
iO = mkIpeObject

-- | Render to an ipe object using the defIO IpeOut
--
--
-- >>> :{
--   iO'' myPolygon $  attr SFill (IpeColor "red")
--                  <> attr SLayer "alpha"
--                  <> attr SLayer "beta"
-- :}
-- IpePath (Path {_pathSegments = LSeq (fromList [PolygonPath SimplePolygon CSeq [Point2 [0,0] :+ (),Point2 [10,10] :+ (),Point2 [100,200] :+ ()]])} :+ Attrs {Attr LayerName {_layerName = "beta"}, NoAttr, NoAttr, NoAttr, NoAttr, Attr IpeColor (Named "red"), NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr})
--
-- >>> iO'' [ myPolygon , myPolygon ] $ attr SLayer "alpha"
-- IpeGroup (Group [IpePath (Path {_pathSegments = LSeq (fromList [PolygonPath SimplePolygon CSeq [Point2 [0,0] :+ (),Point2 [10,10] :+ (),Point2 [100,200] :+ ()]])} :+ Attrs {NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr}),IpePath (Path {_pathSegments = LSeq (fromList [PolygonPath SimplePolygon CSeq [Point2 [0,0] :+ (),Point2 [10,10] :+ (),Point2 [100,200] :+ ()]])} :+ Attrs {NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr})] :+ Attrs {Attr LayerName {_layerName = "alpha"}, NoAttr, NoAttr, NoAttr, NoAttr})
iO''       :: ( HasDefaultIpeOut g, NumType g ~ r
             , DefaultIpeOut g ~ i, ToObject i
             ) => g -> IpeAttributes i r
           -> IpeObject r
iO'' g ats = iO $ defIO g ! ats

-- | generate an ipe object without any specific attributes
iO' :: HasDefaultIpeOut g => g -> IpeObject (NumType g)
iO' = iO . defIO

--------------------------------------------------------------------------------
-- * Default Conversions

-- | Class that specifies a default conversion from a geometry type g into an
-- ipe object.
class ToObject (DefaultIpeOut g) => HasDefaultIpeOut g where
  type DefaultIpeOut g :: Type -> Type

  defIO :: IpeOut g (DefaultIpeOut g) (NumType g)

instance (HasDefaultIpeOut g, a ~ IpeAttributes (DefaultIpeOut g) (NumType g))
        => HasDefaultIpeOut (g :+ a) where
  type DefaultIpeOut (g :+ a) = DefaultIpeOut g
  defIO (g :+ ats) = defIO g ! ats

instance HasDefaultIpeOut a => HasDefaultIpeOut [a] where
  type DefaultIpeOut [a] = Group
  defIO = ipeGroup . map (iO .  defIO)

instance HasDefaultIpeOut (Point 2 r) where
  type DefaultIpeOut (Point 2 r) = IpeSymbol
  defIO = ipeDiskMark

instance HasDefaultIpeOut (LineSegment 2 p r) where
  type DefaultIpeOut (LineSegment 2 p r) = Path
  defIO = ipeLineSegment

instance HasDefaultIpeOut (PolyLine 2 p r) where
  type DefaultIpeOut (PolyLine 2 p r) = Path
  defIO = ipePolyLine

instance (Fractional r, Ord r) => HasDefaultIpeOut (Line 2 r) where
  type DefaultIpeOut (Line 2 r) = Path
  defIO = ipeLine

instance (Fractional r, Ord r) => HasDefaultIpeOut (HalfLine 2 r) where
  type DefaultIpeOut (HalfLine 2 r) = Path
  defIO = ipeHalfLine

instance HasDefaultIpeOut (Polygon t p r) where
  type DefaultIpeOut (Polygon t p r) = Path
  defIO = ipePolygon

instance HasDefaultIpeOut (SomePolygon p r) where
  type DefaultIpeOut (SomePolygon p r) = Path
  defIO = either defIO defIO

instance HasDefaultIpeOut (ConvexPolygon p r) where
  type DefaultIpeOut (ConvexPolygon p r) = Path
  defIO = defIO . view simplePolygon

instance HasDefaultIpeOut (Ellipse r) where
  type DefaultIpeOut (Ellipse r) = Path
  defIO = ipeEllipse

instance Radical r => HasDefaultIpeOut (Disk p r) where
  type DefaultIpeOut (Disk p r) = Path
  defIO = ipeDisk

instance Radical r => HasDefaultIpeOut (Circle p r) where
  type DefaultIpeOut (Circle p r) = Path
  defIO = ipeCircle

instance Num r => HasDefaultIpeOut (Rectangle p r) where
  type DefaultIpeOut (Rectangle p r) = Path
  defIO = ipeRectangle

--------------------------------------------------------------------------------
-- * Point Converters

ipeMark     :: Text -> IpeOut (Point 2 r) IpeSymbol r
ipeMark n p = Symbol p n :+ mempty

ipeDiskMark :: IpeOut (Point 2 r) IpeSymbol r
ipeDiskMark = ipeMark "mark/disk(sx)"

--------------------------------------------------------------------------------
-- * Path Converters

-- | Size of the default bounding box used to clip lines and
-- half-lines in the default IpeOuts.
defaultBox :: Num r => Rectangle () r
defaultBox = let z  = 1000
                 z' = negate z
             in box (ext $ Point2 z' z') (ext $ Point2 z z)

-- | Renders a line as a Path. The line is clipped to the 'defaultBox'
ipeLine :: (Ord r, Fractional r) => IpeOut (Line 2 r) Path r
ipeLine = ipeLineIn defaultBox

-- | Renders the line in the given box.
--
-- pre: the intersection of the box with the line is non-empty
ipeLineIn        :: forall p r. (Ord r, Fractional r)
                 => Rectangle p r -> IpeOut (Line 2 r) Path r
ipeLineIn bBox l = match (l `intersect` bBox) $
     H (\NoIntersection    -> error "ipeLineIn: precondition failed, no intersection")
  :& H (\(_p :: Point 2 r) -> error "ipeLineIn: precondition failed, single point")
  :& H ipeLineSegment
  :& RNil

-- | Renders an Halfine.
--
--
-- pre: the intersection of the box with the line is non-empty
ipeHalfLine :: (Ord r, Fractional r) => IpeOut (HalfLine 2 r) Path r
ipeHalfLine = ipeHalfLineIn defaultBox

-- | Renders a ray, i.e. a half line drawing an arrow in the direction
-- of the ray.
--
-- pre: the intersection of the box with the line is non-empty
ipeRay :: (Ord r, Fractional r) => IpeOut (HalfLine 2 r) Path r
ipeRay = \hl -> ipeHalfLine hl ! attr SArrow normalArrow


-- | Renders the HalfLine in the given box.
--
-- pre: the intersection of the box with the line is non-empty
ipeHalfLineIn        :: forall p r. (Ord r, Fractional r)
                     => Rectangle p r -> IpeOut (HalfLine 2 r) Path r
ipeHalfLineIn bBox l = match (l `intersect` bBox) $
     H (\NoIntersection    -> error "ipeHalfLineIn: precondition failed, no intersection")
  :& H (\(_p :: Point 2 r) -> error "ipeHalfLineIn: precondition failed, single point")
  :& H ipeLineSegment
  :& RNil

ipeLineSegment   :: IpeOut (LineSegment 2 p r) Path r
ipeLineSegment s = (path . pathSegment $ s) :+ mempty

ipePolyLine   :: IpeOut (PolyLine 2 p r) Path r
ipePolyLine p = (path . PolyLineSegment . first (const ()) $ p) :+ mempty

ipeEllipse :: IpeOut (Ellipse r) Path r
ipeEllipse = \e -> path (EllipseSegment e) :+ mempty

ipeCircle :: Radical r => IpeOut (Circle p r) Path r
ipeCircle = ipeEllipse . circleToEllipse

ipeDisk   :: Radical r => IpeOut (Disk p r) Path r
ipeDisk d = ipeCircle (Boundary d) ! attr SFill (IpeColor "0.722 0.145 0.137")

ipeBezier :: IpeOut (BezierSpline 3 2 r) Path r
ipeBezier b = (path $ CubicBezierSegment b) :+ mempty

-- | Helper to construct a path from a singleton item
path :: PathSegment r -> Path r
path = Path . LSeq.fromNonEmpty . (:| [])

pathSegment :: LineSegment 2 p r -> PathSegment r
pathSegment = PolyLineSegment . fromLineSegment . first (const ())

-- | Draw a polygon
ipePolygon                          :: IpeOut (Polygon t p r) Path r
ipePolygon (first (const ()) -> pg) = case pg of
               SimplePolygon{} -> pg^.re _asSimplePolygon :+ mempty
               MultiPolygon{}  -> pg^.re _asMultiPolygon  :+ mempty


-- | Draw a Rectangle
ipeRectangle   :: Num r => IpeOut (Rectangle p r) Path r
ipeRectangle r = ipePolygon $ unsafeFromPoints [tl,tr,br,bl]
  where
    Corners tl tr br bl = corners r

--------------------------------------------------------------------------------
-- * Group Converters

ipeGroup    :: Foldable f => IpeOut (f (IpeObject r)) Group r
ipeGroup xs = Group (toList xs) :+ mempty


--------------------------------------------------------------------------------
-- * Text Converters

-- | Creates an text label
ipeLabel            :: IpeOut (Text :+ Point 2 r) TextLabel r
ipeLabel (txt :+ p) = Label txt p :+ mempty


-- | Annotate an IpeOut with a label
labelled :: (Show lbl, NumType g ~ r, ToObject i)
         => (g -> Point 2 r) -- ^ where to place the label
         -> IpeOut g i r     -- ^ how to draw the geometric object
         -> IpeOut (g :+ lbl) Group r
labelled = labelledWith mempty

-- | Annotate an IpeOut with a label
labelledWith                      :: (Show lbl, NumType g ~ r, ToObject i)
                                  => IpeAttributes TextLabel r -- ^ attributes for the label
                                  -> (g -> Point 2 r) -- ^ where to place the label
                                  -> IpeOut g i r     -- ^ how to draw the geometric object
                                  -> IpeOut (g :+ lbl) Group r
labelledWith ats pos f (g :+ lbl) = ipeGroup [ iO $ f g
                                     , iO $ ipeLabel (Text.pack (show lbl) :+ pos g) ! ats
                                     ]
