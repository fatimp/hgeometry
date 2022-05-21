{-# LANGUAGE TemplateHaskell #-}
module Algorithms.Geometry.SPM.MMP
  ( ShortestPathMap
  , edgeShortestPathMap

  , WithDistance(WithDistance), distanceToSource, predecessorToSource
  ) where

import           Control.Lens
import           Data.EnumMap.Strict (EnumMap)
import qualified Data.EnumMap.Strict as EnumMap
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Maybe (maybeToList, fromMaybe)
import qualified Data.PQueue.Prio.Min as PQueue
import qualified Data.Set as Set
import           Data.UnBounded
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Geometry.LineSegment hiding (endPoints)
import           Geometry.PlanarSubdivision
import           Geometry.Point
import           Witherable

--------------------------------------------------------------------------------


-- ideally we want one world per face,
data MyWorld


-- | Distance and predecessor towards the source
data WithDistance s r = WithDistance { _distanceToSource    :: !r
                                       -- ^ distance to the source
                                     , _predecessorToSource :: !(Maybe (VertexId' s))
                                       -- ^ Nothing if this is the source itself.
                                     }
                      deriving (Show)

makeLenses ''WithDistance

-- | Compare by distance
instance Eq r => Eq (WithDistance s r) where
  (WithDistance a _) == (WithDistance b _) = a == b
instance Ord r => Ord (WithDistance s r) where
  (WithDistance a _) `compare` (WithDistance b _) = a `compare` b


--------------------------------------------------------------------------------

-- | every vertex is labeled with its distance every fragment contains
-- a planar subdivision htat has the SPM in that fragment.
type ShortestPathMap s v e f r =
  PlanarSubdivision s (v, Top (WithDistance s r)) e f r

-- | Triangulated planar subdivision.
type Triangles s v e f r =
  PlanarSubdivision s v e f r



-- | Compute the shortest path map of a given source point
shortestPathMap :: (Ord r)
                 => Point 2 r -- ^ the source point
                -> PlanarSubdivision s v e f r
                -> ShortestPathMap s v e f r
shortestPathMap = undefined



-- |
shortestPathMap' :: (Ord r)
                 => VertexId' s -- ^ the source point
                 -> Triangles s v e f r --
                 -> ShortestPathMap s v e f r
shortestPathMap' = undefined

--------------------------------------------------------------------------------
-- * Computing the Edge subdivision

-- | Shortest path map decomposition of the edges
edgeShortestPathMap :: (Ord r, Num r)
                     => VertexId' s -- ^ the source
                    -> Triangles s v e f r
                    -> PlanarSubdivision s (v, Top (WithDistance s r))
                                           (e, EdgeSubdivision s r)
                                           f r
edgeShortestPathMap s subdiv = mergeAllIntervals
                             . mapDarts    (\d e  -> (e, edgeSubdivOf d edgeSPM))
                             . mapVertices (\vi v -> (v, distanceOf vi edgeSPM))
                             $ subdiv
  where
    edgeSPM = edgeShortestPathMapPortalEdge s subdiv

mergeAllIntervals subdiv = mapEdges (\d (e,es) -> let es' = subdiv^.dataOf (twin d)._2
                                                  in (e, mergeIntervals es es')
                                    )


-- | merge the two edge subdivisions so that we have the actual shortest paths
mergeIntervals :: EdgeSubdivision s r -> EdgeSubdivision s r -> EdgeSubdivision s r
mergeIntervals = undefined


-- | for every portal edge the subidision up to f-free paths
edgeShortestPathMapPortalEdge :: (Ord r, Num r)
                              => VertexId' s
                              -> Triangles s v e f r
                              -> EdgeSPM s r
edgeShortestPathMapPortalEdge s subdiv = computeIntervals s subdiv queue
                                       $ EdgeSPM initialDistances initialSubdivs
  where
    -- initialize all vertex distances on +Infty and s on 0
    initialDistances = EnumMap.insert s (ValT $ WithDistance 0 Nothing)
                     . EnumMap.fromAscList . map (\v -> (v,Top)) . F.toList
                     $ vertices' subdiv

    -- all subdivisions are empty, except for the edges opposite to s
    initialSubdivs  = insertOppositeEdges
                    . EnumMap.fromAscList . map (\e -> (e,EdgeSubdivision Set.empty)) . F.toList
                    $ darts' subdiv

    -- for each edge opposite to s, insert an interval with s as generator
    insertOppositeEdges eSubdivs0 = foldr insertEdge eSubdivs0 oppositeEdges
    insertEdge = undefined

    -- the edges opposite to s
    oppositeEdges = edgesOppositeTo s subdiv

    -- initialize the eventQueue
    queue = foldr (uncurry PQueue.insert) mempty initialEvents

    -- for each edge oppisite to s we add the appropriate events
    -- (endpoints and closest interior point)
    initialEvents = flip concatMap oppositeEdges $ \e ->
      let (u,v) = endPoints e subdiv
          f w   = ( WithDistance (distance (subdiv^.locationOf w) locS) (Just s)
                  , Event $ VertexEvent w
                  )
          mEvt = (\(c,d) -> (WithDistance d (Just s),Event $ InteriorEvent e gen0 c))
                 <$> closestInteriorPointOn (edgeSegment e subdiv) locS
      in maybeToList mEvt <> [ f u, f v ]

    locS = subdiv^.locationOf s
    gen0 = Generator s (WithDistance 0 Nothing) locS

----------------------------------------


-- | A generator gives rise to a candidate interval on an edge.
data Generator s r = Generator { _root         :: !(VertexId' s)
                               , _initialDist  :: !(WithDistance s r)
                               , _unfoldedRoot :: !(Point 2 r)
                               -- ^ point corresponding to the root in the same coordinate
                               -- system as the face opposite to the edge this generator
                               -- corresponds to.
                               --
                               -- note that in the plane the unfolded root is just the same
                               -- as the original location of the root.
                               } deriving (Show)

type SPMInterval s r = Generator s r

newtype EdgeSubdivision s r =
    EdgeSubdivision (Set.Set (Generator s r))
  deriving (Show)


insertInterval' :: Dart s -> SPMInterval s r -> EdgeSubdivision s r -> EdgeSubdivision s r
insertInterval' = undefined


----------------------------------------


data EdgeSPM s r =
  EdgeSPM { vertexDistances  :: EnumMap (VertexId' s) (Top (WithDistance s r))
          , edgeSubdivisions :: EnumMap (Dart s)      (EdgeSubdivision s r)
          }
                 deriving (Show)


-- | Get the current distance estimate and predecessor to v
distanceOf                  :: VertexId' s -> EdgeSPM s r -> Top (WithDistance s r)
distanceOf v (EdgeSPM vd _) = fromMaybe Top $ EnumMap.lookup v vd

-- | get the edge subdivision of a given dart
edgeSubdivOf                  :: Dart s -> EdgeSPM s r -> EdgeSubdivision s r
edgeSubdivOf d (EdgeSPM _ es) = fromMaybe err $ EnumMap.lookup d es
  where
    err = error $ "edgeSubdivOf: absurd, dart " <> show d <> " not found!?"


-- | Permanently assign a distance label to a vertex
permanentlyLabel                              :: VertexId' s
                                              -> Top (WithDistance s r)
                                              -> EdgeSPM s r -> EdgeSPM s r
permanentlyLabel v dist (EdgeSPM vds subdivs) = EdgeSPM (EnumMap.insert v dist vds) subdivs


insertSPMInterval                     :: Dart s -> SPMInterval s r -> EdgeSPM s r -> EdgeSPM s r
insertSPMInterval e i (EdgeSPM vs es) = EdgeSPM vs (EnumMap.adjust (insertInterval' e i) e es)



--------------------------------------------------------------------------------


closestInteriorPointOn            :: LineSegment 2 p r :+ e -> Point 2 r
                                  -> Maybe (Point 2 r, r)
closestInteriorPointOn (s :+ _) q =
  undefined --   sqrt' <$> pointClosestToWithDistance q s
  -- todo get rid of the point if it is and endpoint; otherwise we add that event twice.

sqrt' = undefined

distance :: Point 2 r -> Point 2 r -> r
distance = undefined


-- | Get all edges opposite to s
--
-- TODO: Test if this should indeed be prevIncidentEdge
edgesOppositeTo          :: VertexId' s -> PlanarSubdivision s v e f r -> Vector (Dart s)
edgesOppositeTo s subdiv = flip prevIncidentEdge subdiv <$> outgoingEdges s subdiv



type EventQueue s r = PQueue.MinPQueue (WithDistance s r) (Event s r)


data Event s r = Event { eventKind :: EventKind s r }
  deriving (Show)



-- (Point 2 r) EventKind

data EventKind s r = VertexEvent (VertexId' s)
                   | InteriorEvent (Dart s) (Generator s r) (Point 2 r)
                   deriving (Show)


computeIntervals          :: Ord r
                          => VertexId' s
                          -> Triangles s v e f r
                          -> EventQueue s r
                          -> EdgeSPM s r
                          -> EdgeSPM s r
computeIntervals s subdiv = go
  where
    go queue state = case PQueue.minViewWithKey queue of
      Nothing           -> state -- done
      Just (evt,queue') -> let (newEvts, state') = propagate subdiv state evt
                           in go (insertAll newEvts queue') state'

    insertAll = undefined -- foldr PQueue.insert


-- | Propagate an interval
propagate      :: Triangles s v e f r
               -> EdgeSPM s r -> ( WithDistance s r
                                 , Event s r
                                 )
               -> ( [Event s r] -- new events
                  , EdgeSPM s r
                  )
propagate subdiv state (dist,evt) = case eventKind evt of
    InteriorEvent e gen c -> propagateInterior subdiv state e gen c
    VertexEvent v         -> propagateVertex   subdiv state dist v


propagateInterior subdiv state e gen c = ( undefined
                                         , undefined
                                         )
  where
    -- the face on the other side of the dart e; i.e. the face we are propatating onto
    theFace = rightFace e
    e1 = prevIncidentEdge (twin e) subdiv
    e2 = nextIncidentEdge (twin e) subdiv


project :: SPMInterval s r
project = undefined


propagateVertex                     :: ()
                                    => Triangles s v e f r
                                    -> EdgeSPM s r
                                    -> WithDistance s r
                                    -> VertexId' s
                                    -> ( [Event s r] -- new events
                                       , EdgeSPM s r
                                       )
propagateVertex subdiv state dist v =
    ( undefined
    , permanentlyLabel v (ValT dist) $ state
         -- todo insert the intervals es onto the appropriate edges
    )
  where
    p  =  undefined -- p should be the predecessor, i.e. where we came from
    es = mapMaybe (createCandidateInterval
                    (Generator v dist (subdiv^.locationOf v))
                    undefined -- the access point
                    -- TODO: figure out if this should just be
                  )
         $ edgesOppositeTo v subdiv




-- | creates an interval on e with generator gen if applicable

-- TODO this still misses the access point
createCandidateInterval       :: Generator s r
                              -> Point 2 r -- access point
                              -> Dart s -> Maybe (Generator s r)
createCandidateInterval gen beta e =  undefined


--------------------------------------------------------------------------------
-- * Filling in the faces goes here
