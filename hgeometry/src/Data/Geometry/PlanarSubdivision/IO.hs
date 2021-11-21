--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.PlanarSubdivision.IO
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
--
-- Converting from/to Adjacency Representation of the Planar subdivision
--
--------------------------------------------------------------------------------
module Data.Geometry.PlanarSubdivision.IO
  (
    toTreeRep

  -- * Converting to and from Adjacency list representions
  , toAdjRep
  , fromAdjRep, fromAdjRep'

  ) where

import           Control.Lens hiding (holesOf)
import qualified Data.Foldable as F
import           Data.Geometry.PlanarSubdivision.Basic
import           Data.Geometry.PlanarSubdivision.TreeRep
import qualified Data.PlaneGraph as PG
import           Data.PlaneGraph.AdjRep (Gr(..))
import qualified Data.PlaneGraph.AdjRep as PG
import qualified Data.PlaneGraph.IO as PGIO

--------------------------------------------------------------------------------

-- | Convert to a Tree based representation.
toTreeRep :: PlanarSubdivision s v e f r -> PlanarSD v e f r
toTreeRep psd = let f0 = outerFaceId psd
                in PlanarSD (psd^.dataOf f0) (toInners psd f0)

-- | Creeat the innersd for each hole of the given face
toInners        :: PlanarSubdivision s v e f r -> FaceId' s -> [InnerSD v e f r]
toInners psd f0 = map (toInner psd) . F.toList $ holesOf f0 psd

-- Compute the InnerSD corresponding to the component indicated by the
-- given dart. The given dart lies on the outside of the hole.
toInner        :: PlanarSubdivision s v e f r -> Dart s -> InnerSD v e f r
toInner psd d = InnerSD (mkAdj psd c <$> as) (mkFace psd <$> fs)
  where
    (_,_,c)  = asLocalD d psd
    (Gr as fs) = PGIO.toAdjRep c

-- | Convert the 'Vtx' we created by locally converting the component
-- into a global one; i.e. collect all global information.
mkAdj                       :: forall s v e f r. PlanarSubdivision s v e f r
                            -> Component s r
                            -> Vtx (VertexId' s) (Dart s) r
                            -> Vtx v e r
mkAdj psd c (Vtx _ p ns vi) = Vtx (fromEnum vi) p (map makeGlobal ns) (psd^.dataOf vi)
  where
    makeGlobal (j,d) = let vj = c^.PG.dataOf (toEnum j :: VertexId' (Wrap s))
                       in (fromEnum vj, psd^.dataOf d)

mkFace                       :: forall s v e f r. PlanarSubdivision s v e f r
                             -> PG.Face (FaceId' s)
                             -> (f, [InnerSD v e f r])
mkFace psd (PG.Face _ fi) = (psd^.dataOf fi, toInners psd fi)





-- -- | Transforms the PlanarSubdivision into adjacency lists. For every
-- -- vertex, the adjacent vertices are given in counter clockwise order.
-- --
-- -- See 'toAdjacencyLists' for notes on how we handle self-loops.
-- --
-- -- running time: \(O(n)\)
-- toAdjRep :: PlanarSubdivision s v e f r -> Gr (Vtx v e r) (Face f)
-- toAdjRep = first (\(PGA.Vtx v aj (VertexData p x)) -> Vtx v p aj x) . PGIO.toAdjRep
--          .  view graph

toAdjRep = undefined

fromAdjRep = undefined

fromAdjRep'= undefined
