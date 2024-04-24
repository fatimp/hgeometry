--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.PlnarSubdivision.Basic
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
-- Description :  Basic data types to represent a PlanarSubdivision
--
--------------------------------------------------------------------------------
module HGeometry.PlanarSubdivision.Basic
  ( -- $setup
    PlanarSubdivision(PlanarSubdivision)
  , Component, ComponentId

    -- * Constructing Planar Subdivisions
  -- , fromSimplePolygon
  -- , fromConnectedSegments
  -- , fromPlaneGraph, fromPlaneGraph'

  -- * Quering the Planar Subdivision
  -- , numComponents
  -- , numVertices
  -- , numEdges, numFaces, numDarts
  -- , dual


  -- , components, component


  -- , vertices', vertices
  -- , edges', edges
  -- , faces', internalFaces', faces, internalFaces
  -- , darts'

  -- * Incidences and Adjacencies
  -- , headOf, tailOf, twin, endPoints

  -- , incidentEdges, incomingEdges, outgoingEdges
  -- , nextIncidentEdge, prevIncidentEdge
  -- , nextIncidentEdgeFrom, prevIncidentEdgeFrom
  -- , neighboursOf

  -- , leftFace, rightFace
  -- , outerBoundaryDarts, boundaryVertices, holesOf
  -- , outerFaceId
  -- , boundary'

  -- , Incident (incidences)
  -- , common, commonVertices, commonDarts, commonFaces

  -- * Data
  -- , locationOf
  -- , HasDataOf(..)

  -- , endPointsOf, endPointData

  -- , faceDataOf

  -- , traverseVertices, traverseDarts, traverseFaces
  -- , mapVertices, mapDarts, mapEdges, mapFaces

  -- * Obtaining a Geometric Representation
  -- , edgeSegment, edgeSegments
  -- , faceBoundary
  -- , internalFacePolygon, internalFacePolygons
  -- , outerFacePolygon, outerFacePolygon'
  -- , facePolygons

  -- * IO

  -- -- * ReExports
  -- , VertexId', FaceId'
  -- , VertexId(..), FaceId(..), Dart, World(..)
  -- , VertexData(VertexData), PG.vData, PG.location
  -- , PlanarGraph
  -- , PlaneGraph

  -- -- * Helper; dealing with the Raw types
  -- , PolygonFaceData(..)
  -- , FaceData(FaceData), holes, fData
  -- , Wrap
  -- , rawVertexData, rawDartData, rawFaceData
  -- , vertexData, dartData, faceData
  -- , dataVal
  -- , dartMapping, Raw(..)
  -- , asLocalD, asLocalV, asLocalF
  ) where

import           Control.Lens hiding (holes, holesOf, (.=))
import           Data.Bifunctor (first, second)
import           Data.Coerce
import qualified Data.Foldable as F
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           GHC.Generics (Generic)
import           HGeometry.Box
import           HGeometry.Ext
import           HGeometry.LineSegment hiding (endPoints)
import           HGeometry.PlanarSubdivision.Component
import           HGeometry.PlanarSubdivision.Raw
import           HGeometry.PlaneGraph
import           HGeometry.Point
import           HGeometry.Polygon.Class
import           HGeometry.Properties
import           Hiraffe.PlanarGraph (FaceId, VertexId)
import qualified Hiraffe.PlanarGraph.Dart as Dart
-- import           Hiraffe.PlanarGraph (allDarts,isPositive)

--------------------------------------------------------------------------------

-- | A planarsubdivision is essentially a bunch of plane-graphs; one for every
-- connected component. These graphs store the global ID's (darts, vertexId's, faceId's)
-- in their data values. This essentially gives us a mapping between the two.
--
-- note that a face may actually occur in multiple graphs, hence when we store
-- the edges to the the holes, we store the global edgeId's rather than the
-- 'local' edgeId (dart)'s.
--
-- invariant: the outerface has faceId 0
data PlanarSubdivision s v e f =
  PlanarSubdivision { _components    :: V.Vector (Component s)
                    , _rawVertexData :: V.Vector (Raw s (VertexId  (Wrap s)) v)
                    , _rawDartData   :: V.Vector (Raw s (Dart.Dart (Wrap s)) e)
                    , _rawFaceData   :: V.Vector (RawFace s f)
                    } deriving (Show,Eq,Functor,Generic)

-- makeLenses ''PlanarSubdivision

-- | Lens to access the connected components of a planar subdivision.
components :: Lens' (PlanarSubdivision s v e f) (V.Vector (Component s))
components = lens _components (\ps cs -> ps { _components = cs })

-- | Lens to access the raw vertex data
rawVertexData :: Lens (PlanarSubdivision s v e f) (PlanarSubdivision s v' e f)
                      (V.Vector (Raw s (VertexId  (Wrap s)) v))
                      (V.Vector (Raw s (VertexId  (Wrap s)) v'))
rawVertexData = lens _rawVertexData (\ps vxd -> ps { _rawVertexData = vxd })

-- | Lens to access the raw dart daat a
rawDartData :: Lens (PlanarSubdivision s v e f) (PlanarSubdivision s v e' f)
                    (V.Vector (Raw s (Dart.Dart  (Wrap s)) e))
                    (V.Vector (Raw s (Dart.Dart  (Wrap s)) e'))
rawDartData = lens _rawDartData (\ps vxd -> ps { _rawDartData = vxd })

-- | Access the raw face data
rawFaceData :: Lens (PlanarSubdivision s v e f) (PlanarSubdivision s v e f')
                    (V.Vector (RawFace s f))    (V.Vector (RawFace s f'))
rawFaceData = lens _rawFaceData (\ps vxd -> ps { _rawFaceData = vxd })




type instance NumType   (PlanarSubdivision s v e f) = NumType v
type instance Dimension (PlanarSubdivision s v e f) = 2

-- instance IsBoxable v => IsBoxable (PlanarSubdivision s v e f) where
--   boundingBox = boundingBox . toNonEmptyOf vertices

  -- (allPoints.asPoint)

  --   boundingBoxList' . V.toList . _components


-- | Lens to access a particular component of the planar subdivision.
component    :: ComponentId s -> Lens' (PlanarSubdivision s v e f) (Component s)
component ci = components.singular (ix $ unCI ci)




-- instance (ToJSON v, ToJSON v, ToJSON e, ToJSON f, ToJSON r)
--          => ToJSON (PlanarSubdivision s v e f) where
--   toEncoding = genericToEncoding defaultOptions



--------------------------------------------------------------------------------

instance HasVertices' (PlanarSubdivision s v e f) where
  type Vertex   (PlanarSubdivision s v e f) = v
  type VertexIx (PlanarSubdivision s v e f) = VertexId s
  vertexAt = undefined

-- instance HasVertices (PlanarSubdivision s v e f) (PlanarSubdivision s v' e f)  where

-- instance HasEdges' (PlanarSubdivision s v e f) where
--   type Edge   (PlanarSubdivision s v e f) = e
--   type EdgeIx (PlanarSubdivision s v e f) = Dart.Darts

-- instance HasEdges (PlanarSubdivision s v e f) (PlanarSubdivision s v e' f)  where

-- instance HasFaces' (PlanarSubdivision s v e f) where
--   type Face   (PlanarSubdivision s v e f) = f
--   type FaceIx (PlanarSubdivision s v e f) = FaceId s

--   -- faceAt =

-- instance HasFaces (PlanarSubdivision s v e f) (PlanarSubdivision s v e f')  where
--   -- faces =

-- instance DiGraph_ (PlanarSubdivision s v e f) where

-- instance BidirGraph_ (PlanarSubdivision s v e f) where

-- instance Graph_ (PlanarSubdivision s v e f) where

-- instance PlanarGraph_ (PlanarSubdivision s v e f) v where
--   -- dualGraph, (incidentFaceOf | leftFaceOf), rightFaceOf, prevDartOf, nextDartOf, boundaryDartOf, boundaryDartOf, boundaryDarts

-- instance PlaneGraph_ (PlanarSubdivision s v e f) v where
--   -- TODO: fromEmbedding

-- instance PlanarSubdivision_ (PlanarSubdivision s v e f) v where

--------------------------------------------------------------------------------
{-

-- | Constructs a planarsubdivision from a PlaneGraph
--
-- runningTime: \(O(n)\)
fromPlaneGraph   :: forall s v e f. (Ord r, Num r)
                      => PlaneGraph s v e f -> PlanarSubdivision s v e f
fromPlaneGraph g = fromPlaneGraph' g (PG.outerFaceDart g)

{- HLINT ignore fromPlaneGraph' -}
-- | Given a (connected) PlaneGraph and a dart that has the outerface on its left
-- | Constructs a planarsubdivision
--
-- runningTime: \(O(n)\)
fromPlaneGraph'        :: forall s v e f. PlaneGraph s v e f -> Dart s
                       -> PlanarSubdivision s v e f
fromPlaneGraph' g ofD = PlanarSubdivision (V.singleton . coerce $ g') vd ed fd
  where
    c = ComponentId 0
    vd = V.imap    (\i v   -> Raw c (VertexId i) v)                   $ g^.PG.vertexData
    ed = V.zipWith (\d dd  -> Raw c d dd) allDarts''                  $ g^.PG.rawDartData
    fd = V.imap (\i f      -> RawFace (mkFaceIdx i) (mkFaceData i f)) $ g^.PG.faceData

    g' :: PlaneGraph s (VertexId' s) (Dart s) (FaceId' s) r
    g' = g&PG.faceData    %~ V.imap (\i _ -> mkFaceId $ flipID i)
          &PG.vertexData  %~ V.imap (\i _ -> VertexId i)
          &PG.rawDartData .~ allDarts''

    allDarts'' :: forall s'. V.Vector (Dart s')
    allDarts'' = allDarts' (PG.numDarts g)

    -- make sure the outerFaceId is 0
    oF@(FaceId (VertexId of')) = PG.leftFace ofD g

    mkFaceIdx i | i == 0    = Nothing
                | otherwise = Just (c,mkFaceId . flipID $ i)

    -- at index i we are storing the outerface
    mkFaceData                 :: Int -> f -> FaceData (Dart s) f
    mkFaceData i f | i == 0    = FaceData (Seq.singleton ofD) (g^.dataOf oF)
                   | i == of'  = FaceData mempty              (g^.dataOf (mkFaceId @s 0))
                   | otherwise = FaceData mempty              f

    mkFaceId :: forall s'. Int -> FaceId' s'
    mkFaceId = FaceId . VertexId

    flipID i | i == 0    = of'
             | i == of'  = 0
             | otherwise = i

-- | Construct a plane graph from a simple polygon. It is assumed that the
-- polygon is given in counterclockwise order.
--
-- the interior of the polygon will have faceId 0
--
-- pre: the input polygon is given in counterclockwise order
-- running time: \(O(n)\).
fromSimplePolygon            :: forall s p f. (Ord r, Num r)
                             => SimplePolygon p r
                             -> f -- ^ data inside
                             -> f -- ^ data outside the polygon
                             -> PlanarSubdivision s p () f
fromSimplePolygon pg iD oD =
  fromPlaneGraph (PG.fromSimplePolygon pg iD oD)

-- | Constructs a connected planar subdivision.
--
-- pre: the segments form a single connected component
-- running time: \(O(n\log n)\)
fromConnectedSegments :: forall s p e r f. (Foldable f, Ord r, Num r)
                      => f (LineSegment 2 p r :+ e)
                      -> PlanarSubdivision s (NonEmpty p) e () r
fromConnectedSegments = fromPlaneGraph . PG.fromConnectedSegments

-- g1 = PG.fromConnectedSegments (Identity Test1) testSegs
-- ps1 = fromConnectedSegments (Identity Test1) testSegs

-- data Test1 = Test1

-- draw = V.filter isEmpty . rawFacePolygons
--   where
--     isEmpty (_,Left  p :+ _) = (< 3) . length . polygonVertices $ p
--     isEmpty (_,Right p :+ _) = (< 3) . length . polygonVertices $ p

-- testSegs = map (\(p,q) -> ClosedLineSegment (ext p) (ext q) :+ ())
--                    [ (origin, Point2 10 10)
--                    , (origin, Point2 12 10)
--                    , (origin, Point2 20 5)
--                    , (origin, Point2 13 20)
--                    , (Point2 10 10, Point2 12 10)
--                    , (Point2 10 10, Point2 13 20)
--                    , (Point2 12 10, Point2 20 5)
--                    ]


--------------------------------------------------------------------------------

-- | Data type that expresses whether or not we are inside or outside the
-- polygon.
data PolygonFaceData = Inside | Outside deriving (Show,Read,Eq)


--------------------------------------------------------------------------------
-- * Basic Graph information

-- | Get the number of vertices
--
-- >>> numVertices myGraph
-- 1
numComponents :: PlanarSubdivision s v e f  -> Int
numComponents = V.length . _components

-- | Get the number of vertices
--
-- >>> numVertices myGraph
-- 4
numVertices :: PlanarSubdivision s v e f  -> Int
numVertices = V.length . _rawVertexData

-- | Get the number of Darts
--
-- >>> numDarts myGraph
-- 12
numDarts :: PlanarSubdivision s v e f  -> Int
numDarts = V.length . _rawDartData

-- | Get the number of Edges
--
-- >>> numEdges myGraph
-- 6
numEdges :: PlanarSubdivision s v e f  -> Int
numEdges = (`div` 2) . V.length . _rawDartData

-- | \( O(1) \). Get the number of faces
--
-- >>> numFaces myGraph
-- 4
numFaces :: PlanarSubdivision s v e f  -> Int
numFaces = V.length . _rawFaceData

-- | Enumerate all vertices
--
-- >>> vertices' myGraph
-- [VertexId 0,VertexId 1,VertexId 2,VertexId 3]
vertices'   :: PlanarSubdivision s v e f  -> V.Vector (VertexId' s)
vertices' ps = let n = numVertices ps
               in V.fromList $ map VertexId [0..n-1]

-- | Enumerate all vertices, together with their vertex data

-- >>> vertices myGraph
-- [(VertexId 0,()),(VertexId 1,()),(VertexId 2,()),(VertexId 3,())]
vertices    :: PlanarSubdivision s v e f  -> V.Vector (VertexId' s, VertexData r v)
vertices ps = (\vi -> (vi,ps^.vertexDataOf vi)) <$> vertices' ps

-- | Enumerate all darts
darts' :: PlanarSubdivision s v e f  -> V.Vector (Dart s)
darts' = allDarts' . numDarts

allDarts'   :: forall s'. Int -> V.Vector (Dart s')
allDarts' n = V.fromList $ take n allDarts


-- | Enumerate all edges. We report only the Positive darts
edges' :: PlanarSubdivision s v e f  -> V.Vector (Dart s)
edges' = V.filter isPositive . darts'

-- | Enumerate all edges with their edge data. We report only the Positive
-- darts.
--
-- >>> mapM_ print $ edges myGraph
-- (Dart (Arc 2) +1,"c+")
-- (Dart (Arc 1) +1,"b+")
-- (Dart (Arc 0) +1,"a+")
-- (Dart (Arc 5) +1,"g+")
-- (Dart (Arc 4) +1,"e+")
-- (Dart (Arc 3) +1,"d+")
edges    :: PlanarSubdivision s v e f  -> V.Vector (Dart s, e)
edges ps = (\e -> (e,ps^.dataOf e)) <$> edges' ps

-- | \( O(n) \). Vector of all primal faces.
faces'    :: PlanarSubdivision s v e f -> V.Vector (FaceId' s)
faces' ps = let n = numFaces ps
            in V.fromList $ map (FaceId . VertexId) [0..n-1]

-- | \( O(n) \). Vector of all primal faces.
internalFaces'    :: PlanarSubdivision s v e f -> V.Vector (FaceId' s)
internalFaces' = V.tail . faces'

-- | \( O(n) \). Vector of all primal faces with associated data.
faces    :: PlanarSubdivision s v e f -> V.Vector (FaceId' s, FaceData (Dart s) f)
faces ps = (\fi -> (fi,ps^.faceDataOf fi)) <$> faces' ps




-- | Enumerates all faces with their face data exlcluding  the outer face
internalFaces    :: PlanarSubdivision s v e f
                 -> V.Vector (FaceId' s, FaceData (Dart s) f)
internalFaces ps = V.tail $ faces ps
  -- this uses that the outerfaceId is 0, and thus it is the first face in the vector.

-- | lens to access the Dart Data
dartData :: Lens (PlanarSubdivision s v e f) (PlanarSubdivision s v e' f)
                 (V.Vector (Dart s, e))        (V.Vector (Dart s, e'))
dartData = lens getF setF
  where
    getF     = V.imap (\i x -> (toEnum i, x^.dataVal)) . _rawDartData
    setF ps ds' = ps&rawDartData %~ mkDS' ds'

    -- create a new dartData vector to assign the values to
    mkDS' ds' ds = V.create $ do
                     v <- MV.new (V.length ds)
                     mapM_ (assignDart ds v) ds'
                     pure v

    assignDart ds v (d,x) = let i = fromEnum d
                                y = ds V.! i
                            in MV.write v i (y&dataVal .~ x)


-- | Lens to the facedata of the faces themselves. The indices correspond to the faceIds
faceData :: Lens (PlanarSubdivision s v e f) (PlanarSubdivision s v e f' r)
                 (V.Vector f)                  (V.Vector f')
faceData = lens getF setF
  where
    getF = fmap (^.faceDataVal.fData) . _rawFaceData
    setF ps v' = ps&rawFaceData %~ V.zipWith (\x' x -> x&faceDataVal.fData .~ x') v'

-- | Lens to the facedata of the vertexdata themselves. The indices correspond to the vertexId's
vertexData :: Lens (PlanarSubdivision s v e f) (PlanarSubdivision s v' e f)
                   (V.Vector v)                  (V.Vector v')
vertexData = lens getF setF
  where
    getF = fmap (^.dataVal) . _rawVertexData
    setF ps v' = ps&rawVertexData %~ V.zipWith (\x' x -> x&dataVal .~ x') v'


-- | The tail of a dart, i.e. the vertex this dart is leaving from
--
-- running time: \(O(1)\)
tailOf      :: Dart s -> PlanarSubdivision s v e f  -> VertexId' s
tailOf d ps = let (_,d',g) = asLocalD d ps
              in g^.dataOf (PG.tailOf d' g)


-- | The vertex this dart is heading in to
--
-- running time: \(O(1)\)
headOf       :: Dart s -> PlanarSubdivision s v e f  -> VertexId' s
headOf d ps = let (_,d',g) = asLocalD d ps
              in g^.dataOf (PG.headOf d' g)


-- | endPoints d g = (tailOf d g, headOf d g)
--
-- running time: \(O(1)\)
endPoints      :: Dart s -> PlanarSubdivision s v e f
               -> (VertexId' s, VertexId' s)
endPoints d ps = (tailOf d ps, headOf d ps)


-- | All edges incident to vertex v, in counterclockwise order around v.
--
-- running time: \(O(k)\), where \(k\) is the number of edges reported.
incidentEdges                 :: VertexId' s -> PlanarSubdivision s v e f
                              -> V.Vector (Dart s)
incidentEdges v ps=  let (_,v',g) = asLocalV v ps
                         ds       = PG.incidentEdges v' g
                     in (\d -> g^.dataOf d) <$> ds


-- | Given a dart d that points into some vertex v, report the next dart in the
-- cyclic (counterclockwise) order around v.
--
-- running time: \(O(1)\)
nextIncidentEdge      :: Dart s -> PlanarSubdivision s v e f -> Dart s
nextIncidentEdge d ps = let (_,d',g) = asLocalD d ps
                            d''      = PG.nextIncidentEdge d' g
                        in g^.dataOf d''

-- | Given a dart d that points into some vertex v, report the
-- previous dart in the cyclic (counterclockwise) order around v.
--
-- running time: \(O(1)\)
--
-- >>> prevIncidentEdge (dart 1 "+1") smallG
-- Dart (Arc 3) +1
prevIncidentEdge      :: Dart s -> PlanarSubdivision s v e f -> Dart s
prevIncidentEdge d ps = let (_,d',g) = asLocalD d ps
                            d''      = PG.prevIncidentEdge d' g
                        in g^.dataOf d''

-- | Given a dart d that points away from some vertex v, report the
-- next dart in the cyclic (counterclockwise) order around v.
--
--
-- running time: \(O(1)\)
--
nextIncidentEdgeFrom      :: Dart s -> PlanarSubdivision s v e f -> Dart s
nextIncidentEdgeFrom d ps = let (_,d',g) = asLocalD d ps
                                d''      = PG.nextIncidentEdgeFrom d' g
                            in g^.dataOf d''

-- | Given a dart d that points into away from vertex v, report the previous dart in the
-- cyclic (counterclockwise) order around v.
--
-- running time: \(O(1)\)
--
prevIncidentEdgeFrom      :: Dart s -> PlanarSubdivision s v e f -> Dart s
prevIncidentEdgeFrom d ps = let (_,d',g) = asLocalD d ps
                                d''      = PG.prevIncidentEdgeFrom d' g
                            in g^.dataOf d''


-- | All edges incident to vertex v in incoming direction
-- (i.e. pointing into v) in counterclockwise order around v.
--
-- running time: \(O(k)\), where \(k) is the total number of incident edges of v
incomingEdges      :: VertexId' s -> PlanarSubdivision s v e f -> V.Vector (Dart s)
incomingEdges v ps = orient <$> incidentEdges v ps
  where
    orient d = if headOf d ps == v then d else twin d

-- | All edges incident to vertex v in outgoing direction
-- (i.e. pointing away from v) in counterclockwise order around v.
--
-- running time: \(O(k)\), where \(k) is the total number of incident edges of v
outgoingEdges      :: VertexId' s -> PlanarSubdivision s v e f  -> V.Vector (Dart s)
outgoingEdges v ps = orient <$> incidentEdges v ps
  where
    orient d = if tailOf d ps == v then d else twin d


-- | Gets the neighbours of a particular vertex, in counterclockwise order
-- around the vertex.
--
-- running time: \(O(k)\), where \(k\) is the output size
neighboursOf      :: VertexId' s -> PlanarSubdivision s v e f -> V.Vector (VertexId' s)
neighboursOf v ps = flip tailOf ps <$> incomingEdges v ps

-- | The face to the left of the dart
--
-- running time: \(O(1)\).
leftFace      :: Dart s -> PlanarSubdivision s v e f  -> FaceId' s
leftFace d ps = let (_,d',g) = asLocalD d ps
                    fi       = PG.leftFace d' g
                in g^.dataOf fi

-- | The face to the right of the dart
--
-- running time: \(O(1)\).
rightFace      :: Dart s -> PlanarSubdivision s v e f  -> FaceId' s
rightFace d ps = let (_,d',g) = asLocalD d ps
                     fi       = PG.rightFace d' g
                in g^.dataOf fi

-- | The darts on the outer boundary of this face. The darts are
-- reported in order along the face. This means that for internal
-- faces the darts are reported in *clockwise* order along the
-- boundary, whereas for the outer face the darts are reported in
-- counter clockwise order.
--
-- running time: \(O(k)\), where \(k\) is the output size.
outerBoundaryDarts      :: FaceId' s -> PlanarSubdivision s v e f  -> V.Vector (Dart s)
outerBoundaryDarts f ps = V.concatMap single . V.fromList . NonEmpty.toList $ asLocalF f ps
  where
    single (_,f',g) = (\d -> g^.dataOf d) <$> PG.boundary f' g


-- | Get the local face and component from a given face.
asLocalF                          :: FaceId' s -> PlanarSubdivision s v e f
                                  -> NonEmpty (ComponentId s, FaceId' (Wrap s), Component s r)
asLocalF (FaceId (VertexId f)) ps = case ps^?!rawFaceData.ix f of
      RawFace (Just (ci,f')) _        -> (ci,f',ps^.component ci) :| []
      RawFace Nothing (FaceData hs _) -> toLocalF <$> NonEmpty.fromList (F.toList hs)
  where
    toLocalF d = let (ci,d',c) = asLocalD d ps in (ci,PG.leftFace d' c,c)

-- | The vertices of the outer boundary of the face, for internal
-- faces in clockwise order, for the outer face in counter clockwise
-- order.
--
--
-- running time: \(O(k)\), where \(k\) is the output size.
boundaryVertices      :: FaceId' s -> PlanarSubdivision s v e f
                      -> V.Vector (VertexId' s)
boundaryVertices f ps = (`headOf` ps) <$> outerBoundaryDarts f ps


-- | Lists the holes in this face, given as a list of darts to arbitrary darts
-- on those faces. The returned darts are on the outside of the hole, i.e. they are
-- incident to the given input face:
--
-- prop> all (\d -> leftFace d ps == fi) $ holesOf fi ps
--
-- running time: \(O(k)\), where \(k\) is the number of darts returned.
holesOf       :: FaceId' s -> PlanarSubdivision s v e f -> Seq.Seq (Dart s)
holesOf fi ps = ps^.faceDataOf fi.holes


--------------------------------------------------------------------------------
-- * Access data



asLocalD      :: Dart s -> PlanarSubdivision s v e f
              -> (ComponentId s, Dart (Wrap s), Component s r)
asLocalD d ps = let (Raw ci d' _) = ps^?!rawDartData.ix (fromEnum d)
                in (ci,d',ps^.component ci)




asLocalV                 :: VertexId' s -> PlanarSubdivision s v e f
                         -> (ComponentId s, VertexId' (Wrap s), Component s r)
asLocalV (VertexId v) ps = let (Raw ci v' _) = ps^?!rawVertexData.ix v
                           in (ci,v',ps^.component ci)

-- | Lens to access the vertex data
--
-- Note that using the setting part of this lens may be very
-- expensive!!  (O(n))
vertexDataOf               :: VertexId' s
                           -> Lens' (PlanarSubdivision s v e f ) (VertexData r v)
vertexDataOf (VertexId vi) = lens get' set''
  where
    get' ps = let (Raw ci wvdi x) = ps^?!rawVertexData.ix vi
                  vd              = ps^.component ci.PG.vertexDataOf wvdi
              in vd&vData .~ x
    set'' ps x = let (Raw ci wvdi _)  = ps^?!rawVertexData.ix vi
                 in ps&rawVertexData.ix vi.dataVal                .~ (x^.vData)
                      &component ci.PG.vertexDataOf wvdi.location .~ (x^.location)


-- | Get the location of a vertex in the planar subdivision.
--
-- Note that the setting part of this lens may be very expensive!
-- Moreover, use with care (as this may destroy planarity etc.)
locationOf   :: VertexId' s -> Lens' (PlanarSubdivision s v e f ) (Point 2 r)
locationOf v = vertexDataOf v.location


-- | Lens to get the face data of a particular face. Note that the
-- setting part of this lens may be very expensive! (O(n))
faceDataOf    :: FaceId' s -> Lens' (PlanarSubdivision s v e f)
                                    (FaceData (Dart s) f)
faceDataOf fi = lens getF setF
  where
    (FaceId (VertexId i)) = fi
    getF ps = ps^?!rawFaceData.ix i.faceDataVal
    setF ps fd = ps&rawFaceData.ix i.faceDataVal .~ fd

instance HasDataOf (PlanarSubdivision s v e f) (VertexId' s) where
  type DataOf (PlanarSubdivision s v e f) (VertexId' s) = v
  dataOf v = vertexDataOf v.vData

instance HasDataOf (PlanarSubdivision s v e f) (Dart s) where
  type DataOf (PlanarSubdivision s v e f) (Dart s) = e
  dataOf d = rawDartData.singular (ix (fromEnum d)).dataVal

instance HasDataOf (PlanarSubdivision s v e f) (FaceId' s) where
  type DataOf (PlanarSubdivision s v e f) (FaceId' s) = f
  dataOf f = faceDataOf f.fData

-- | Traverse the vertices of the planar subdivision
traverseVertices :: Applicative g
                 => (VertexId' s -> v -> g v')
                 -> PlanarSubdivision s v e f -> g (PlanarSubdivision s v' e f)
traverseVertices h = traverseOfawVertexData (traverseWith VertexId h)

-- | Traverse the darts of the Planar subdivision
traverseDarts   :: Applicative g
                => (Dart s -> e -> g e')
                -> PlanarSubdivision s v e f -> g (PlanarSubdivision s v e' f)
traverseDarts h = traverseOfawDartData (traverseWith toEnum h)


-- | Traverse the faces of the planar subdivision.
traverseFaces   :: Applicative g
                => (FaceId' s -> f -> g f')
                -> PlanarSubdivision s v e f -> g (PlanarSubdivision s v e f' r)
traverseFaces h = traverseOfawFaceData (traverseFaces' h)
  where
    traverseFaces' h' = itraverse (\i -> traverse (h' (FaceId . VertexId $ i)))

-- | Helper function to implement traver(vertertices|darts|faces)
traverseWith         :: Applicative g
                     => (Int -> w s)
                     -> (w s -> v -> g v')
                     -> V.Vector (Raw ci i v)
                     -> g (V.Vector (Raw ci i v'))
traverseWith mkIdx h = itraverse (\i -> traverse (h $ mkIdx i))

--------------------------------------------------------------------------------

-- | Map with index over all faces
mapFaces   :: (FaceId' s -> f -> f')
           -> PlanarSubdivision s v e f -> PlanarSubdivision s v e f' r
mapFaces h = runIdentity . traverseFaces (\i x -> Identity $ h i x)

-- | Map with index over all vertices
mapVertices   :: (VertexId' s -> v -> v')
              -> PlanarSubdivision s v e f -> PlanarSubdivision s v' e f
mapVertices h = runIdentity . traverseVertices (\i x -> Identity $ h i x)

-- | Map with index over all darts
mapDarts   :: (Dart s -> e -> e')
           -> PlanarSubdivision s v e f -> PlanarSubdivision s v e' f
mapDarts h = runIdentity . traverseDarts (\i x -> Identity $ h i x)

-- | Map a function over all positive darts. The negative darts are
-- simply set to look up the value of their corresponding positive dart
mapEdges     :: (Dart s -> e -> e')
             -> PlanarSubdivision s v e  f
             -> PlanarSubdivision s v e' f
mapEdges f ps = mapDarts (\d -> \case
                             Left  _ -> fromRight $ ps'^.dataOf (twin d)
                             Right e -> e
                         ) ps'
  where
    ps'       = mapDarts (\d e -> if isPositive d then Right (f d e) else Left e) ps
    fromRight = either (error "mapEdges: absurd") id

--------------------------------------------------------------------------------

-- | Getter for the data at the endpoints of a dart
--
-- running time: \(O(1)\)
endPointsOf   :: Dart s -> Getter (PlanarSubdivision s v e f )
                                  (VertexData r v, VertexData r v)
endPointsOf d = to (endPointData d)

-- | data corresponding to the endpoints of the dart
--
-- running time: \(O(1)\)
endPointData      :: Dart s -> PlanarSubdivision s v e f
                  ->  (VertexData r v, VertexData r v)
endPointData d ps = let (u,v) = endPoints d ps
                    in (ps^.vertexDataOf u, ps^.vertexDataOf v)


--------------------------------------------------------------------------------

-- | gets the id of the outer face
--
-- running time: \(O(1)\)
outerFaceId :: PlanarSubdivision s v e f -> FaceId' s
outerFaceId = const . FaceId . VertexId $ 0
  -- our invariant tells us the outerface is always at faceId 0

--------------------------------------------------------------------------------

-- | Reports all edges as line segments
edgeSegments    :: PlanarSubdivision s v e f -> V.Vector (Dart s, LineSegment 2 v r :+ e)
edgeSegments ps = (\d -> (d,edgeSegment d ps)) <$> edges' ps


-- | Given a dart and the subdivision constructs the line segment
-- representing it. The segment \(\overline{uv})\) is has \(u\) as its
-- tail and \(v\) as its head.
--
-- \(O(1)\)
edgeSegment      :: Dart s -> PlanarSubdivision s v e f -> LineSegment 2 v r :+ e
edgeSegment d ps = let (p,q) = bimap PG.vtxDataToExt PG.vtxDataToExt $ ps^.endPointsOf d
                   in ClosedLineSegment p q :+ ps^.dataOf d


-- | Given a dart d, generates the darts on (the current component of)
-- the boundary of the the face that is to the right of the given
-- dart. The darts are reported in order along the face. This means
-- that for
--
-- - (the outer boundary of an) internal faces the darts are reported
--   in *clockwise* order along the boundary,
-- - the "inner" boundary of a face, i.e. the boundary of ahole, the
--   darts are reported in *counter clockwise* order.
--
-- Note that this latter case means that in the darts of a a component
-- of the outer face are reported in counter clockwise order.
--
-- \(O(k)\), where \(k\) is the number of darts reported
boundary'     :: Dart s -> PlanarSubdivision s v e f -> V.Vector (Dart s)
boundary' d ps = let (_,d',g) = asLocalD d ps
                 in (\d'' -> g^.dataOf d'') <$> PG.boundary' d' g

-- | The outerboundary of the face as a simple polygon. For internal
-- faces the polygon that is reported has its vertices stored in CCW
-- order (as expected).
--
-- pre: FaceId refers to an internal face.
--
-- \(O(k)\), where \(k\) is the complexity of the outer boundary of
-- the face
faceBoundary      :: FaceId' s -> PlanarSubdivision s v e f -> SimplePolygon v r :+ f
faceBoundary i ps = unsafeFromPoints (reverse pts) :+ (ps^.dataOf i)
  where
    d   = V.head $ outerBoundaryDarts i ps
    pts = (\d' -> PG.vtxDataToExt $ ps^.vertexDataOf (headOf d' ps))
       <$> V.toList (boundary' d ps)
    -- for internal faces boundary' produces the boundary darts in
    -- clockwise order. Hence, we reverse the sequence of points we
    -- obtain to get the points/vertices in CCW order, so that we can
    -- construct a simplepolygon out of them.

-- | Constructs the boundary of the given face.
--
-- \(O(k)\), where \(k\) is the complexity of the face
internalFacePolygon      :: FaceId' s -> PlanarSubdivision s v e f
                         -> SomePolygon v r :+ f
internalFacePolygon i ps = case F.toList $ holesOf i ps of
                        [] -> Left  res                               :+ x
                        hs -> Right (MultiPolygon res $ map toHole hs) :+ x
  where
    res :+ x = faceBoundary i ps
    toHole d = faceBoundary (leftFace d ps) ps ^. core
-- TODO: Verify that holes are in the right orientation.


-- | Returns a sufficiently large, rectangular, polygon that contains
-- the entire planar subdivision. Each component corresponds to a hole
-- in this polygon.
outerFacePolygon    :: (Num r, Ord r)
                    => PlanarSubdivision s v e f -> MultiPolygon (Maybe v) r :+ f
outerFacePolygon ps = outerFacePolygon' outer ps & core %~ first (either (const Nothing) Just)
  where
    outer = rectToPolygon . grow 1 . boundingBox $ ps
    rectToPolygon = unsafeFromPoints . reverse . F.toList . corners

-- | Given a sufficiently large outer boundary, draw the outerface as
-- a polygon with a hole.
outerFacePolygon'          :: SimplePolygon v' r
                           -> PlanarSubdivision s v e f -> MultiPolygon (Either v' v) r :+ f
outerFacePolygon' outer ps = MultiPolygon (first Left outer) holePgs :+ ps^.dataOf i
  where
    i       = outerFaceId ps
    holePgs = map getBoundary . F.toList $ holesOf i ps
    -- get the bondary of a hole. Note that for holes, the function
    -- 'boundary' promisses to report the darts, and therefore the
    -- vertices in CCW order. Hence, we can directly construct a SimplePolygon out of it.
    getBoundary d = unsafeFromPoints . fmap (second Right) $ faceBoundary' (twin d)
    faceBoundary' d = (\d' -> PG.vtxDataToExt $ ps^.vertexDataOf (headOf d' ps))
                      <$> V.toList (boundary' d ps)

-- | Procuces a polygon for each *internal* face of the planar
-- subdivision.
internalFacePolygons    :: PlanarSubdivision s v e f
                        -> V.Vector (FaceId' s, SomePolygon v r :+ f)
internalFacePolygons ps = fmap (\(i,_) -> (i,internalFacePolygon i ps)) . internalFaces $ ps


-- | Procuces a polygon for each face of the planar subdivision.
facePolygons    :: (Num r, Ord r)
                => PlanarSubdivision s v e f
                -> V.Vector (FaceId' s, SomePolygon (Maybe v) r :+ f)
facePolygons ps = V.cons (outerFaceId ps, first Right $ outerFacePolygon ps) ifs
  where
    ifs = wrapJust <$> internalFacePolygons ps
    g :: Bifunctor g => g a b -> g (Maybe a) b
    g = first Just

    wrapJust                 :: (FaceId' s, SomePolygon v r :+ f)
                             -> (FaceId' s, SomePolygon (Maybe v) r :+ f)
    wrapJust (i,(spg :+ f)) = (i,bimap g g spg :+ f)



-- | Mapping between the internal and extenral darts
dartMapping    :: PlanarSubdivision s v e f -> V.Vector (Dart (Wrap s), Dart s)
dartMapping ps = ps^.component (ComponentId 0).PG.dartData



--------------------------------------------------------------------------------

-- data Id a = Id a
-- data Test = Test

-- triangle :: PlanarSubdivision Test () () PolygonFaceData Rational
-- triangle = (\pg -> fromSimplePolygon (Id Test) pg Inside Outside)
--          $ trianglePG

-- trianglePG = fromPoints . map ext $ [origin, Point2 10 0, Point2 10 10]














--------------------------------------------------------------------------------


-- | A class for describing which features (vertex, edge, face) of a planar subdivision
--   can be incident to each other.
class Incident s a b where
  incidences :: PlanarSubdivision s v e f -> a -> [b]

instance Incident s (VertexId' s) (Dart s) where
  incidences psd i = V.toList (incidentEdges i psd) ++ map twin (V.toList $ incidentEdges i psd)

instance Incident s (VertexId' s) (FaceId' s) where
  incidences psd i = map ((flip leftFace) psd) $ V.toList $ incidentEdges i psd

instance Incident s (Dart s) (VertexId' s) where
  incidences psd i = [headOf i psd, tailOf i psd]

instance Incident s (Dart s) (FaceId' s) where
  incidences psd i = [leftFace i psd, rightFace i psd]

instance Incident s (FaceId' s) (VertexId' s) where
  incidences psd i = V.toList $ boundaryVertices i psd

instance Incident s (FaceId' s) (Dart s) where
  incidences psd i = V.toList (outerBoundaryDarts i psd) ++ map twin (V.toList $ outerBoundaryDarts i psd)

-- | Given two features (vertex, edge, or face) of a subdivision,
--   report all features of a given type that are incident to both.
common :: (Incident s a c, Incident s b c, Ord c) => PlanarSubdivision s v e f -> a -> b -> [c]
common psd a b = Set.toList $ Set.intersection (Set.fromList $ incidences psd a) (Set.fromList $ incidences psd b)

-- | Given two features (edge or face) of a subdivision, report all
-- vertices that are incident to both.
commonVertices :: (Incident s a (VertexId' s), Incident s b (VertexId' s)) => PlanarSubdivision s v e f -> a -> b -> [VertexId' s]
commonVertices = common

-- | Given two features (vertex or face) of a subdivision, report all
--   edges that are incident to both.  Returns both darts of each
--   qualifying edge.
commonDarts :: (Incident s a (Dart s), Incident s b (Dart s)) => PlanarSubdivision s v e f -> a -> b -> [Dart s]
commonDarts = common

-- | Given two features (vertex or edge) of a subdivision, report all
-- faces that are incident to both.
commonFaces :: (Incident s a (FaceId' s), Incident s b (FaceId' s)) => PlanarSubdivision s v e f -> a -> b -> [FaceId' s]
commonFaces = common


-}
