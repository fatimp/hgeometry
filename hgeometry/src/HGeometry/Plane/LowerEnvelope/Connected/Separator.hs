{-# LANGUAGE  UndecidableInstances  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.LowerEnvelope.Separator
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Computes a separator for a planar graph
--
--------------------------------------------------------------------------------
module HGeometry.Plane.LowerEnvelope.Connected.Separator
  ( Separator
  , planarSeparator

  , bff


  , treeEdges
  ) where

import qualified Data.Foldable as F
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Ord (Down(..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Tree
import           HGeometry.Plane.LowerEnvelope.Connected.Graph
import           HGeometry.Plane.LowerEnvelope.Connected.Separator.Util
import           HGeometry.Plane.LowerEnvelope.Connected.Split
import           HGeometry.Vector

import           Debug.Trace
--------------------------------------------------------------------------------
-- * BFS

-- | Computes a breath first forest
-- (O(n \log n))
bff    :: Ord k => PlaneGraph k v e -> [Tree k]
bff gr = go (Map.keysSet gr)
  where
    go remaining = case Set.minView remaining of
      Nothing              -> []
      Just (v, remaining') -> let (remaining'', t) = bfs gr v remaining'
                              in t : go remaining''

-- | Turn the map into a tree.
toTree   :: Ord k => Map k [k] -> k -> Tree k
toTree m = go
  where
    go s = Node s $ map go (fromMaybe [] $ Map.lookup s m)

-- | BFS from the given starting vertex, and the set of still-to-visit vertices.
-- returns the remaining still-to-visit vertices and the tree.
bfs      :: Ord k => PlaneGraph k v e -> k -> Set k -> (Set k, Tree k)
bfs gr s = fmap (flip toTree s) . bfs' [s] Map.empty
  where
    bfs' lvl m remaining = case foldr visit ([], remaining, m) lvl of
                             (lvl', remaining', m') -> case lvl' of
                               [] -> (remaining',m')
                               _  -> bfs' lvl' m' remaining'

    visit v (lvl,remaining, m) = let chs = filter (flip Set.member remaining) $ neighs v
                                 in ( chs <> lvl
                                    , foldr Set.delete remaining chs
                                    , Map.insert v chs m
                                    )
    neighs v = maybe [] (Map.elems . fst) $ Map.lookup v gr

--------------------------------------------------------------------------------




--------------------------------------------------------------------------------

data LevelInfo v = Level { levelIndex  :: {-# UNPACK #-}!Int
                         , levelSize   :: {-# UNPACK #-}!Int
                         -- ^ size of this level
                         , accumSize :: {-# UNPACK #-}!Int
                         -- ^ size of the prefix up to this level
                         , levelVertices :: [v]
                         }
                 deriving (Show,Eq,Foldable)

sqrt' :: Int -> Int
sqrt' = floor . sqrt . fromIntegral

-- planarSeparator    :: PlanarGraph_ planarGraph
--                    => planarGraph
--                    -> ([VertexIx planarGraph], Vector 2 [VertexIx planarGraph])

type Separator k = ([k],Vector 2 [k])

-- | Returns a pair (separator, Vector2 verticesSubGraphA verticesSubGraphB)
-- so that
--
-- 1) there are no edges connecting subGraph A and subgraph B,
-- 2) the size of the separator is at most sqrt(n).
-- 3) the vertex sets of A and B have weight at most 2/3 the total weight
planarSeparator    :: ( Ord k
                      , Show k
                      ) => PlaneGraph k v e -> Separator k
planarSeparator gr = case trees of
    []                 -> ([],Vector2 [] [])
    ((tr,m):rest)
      | m <= twoThirds -> traceShow (tr,m,n,twoThirds) $ groupComponents
      | otherwise      -> planarSeparator' (traceShowWith ("tree",) tr) m -- we should also add the remaining vertices
  where
    trees = List.sortOn (Down . snd) . map (\t -> (t, length t)) $ bff gr
    n     = sum $ map snd trees
    half = n `div` 2
    twoThirds = 2 * (n `div` 3)

    groupComponents = undefined

    planarSeparator' tr _ = case List.break (\lvl -> accumSize lvl < half) lvls of
        (_,    [])          -> ([], Vector2 (F.toList tr) [])
                                 -- somehow we have too little weight;
        (pref, (l1 : suff)) -> planarSeparatorTree twoThirds gr tr'
          where
            k      = accumSize l1
            p  lvl = levelSize lvl + 2*(levelIndex l1  - levelIndex lvl)    <= 2 * sqrt' k
            p' lvl = levelSize lvl + 2*(levelIndex lvl - levelIndex l1 - 1) <= 2 * sqrt' (n-k)

            l0 = findR     p  (pref <> [l1])
            l2 = List.find p' suff
            tr' = trim l0 l2 tr


            -- sep       = undefined
            -- verticesA = undefined
            -- verticesB = undefined
      where
            -- compute the levels, their sizes, and the sum of their sizes
        (_, lvls) = List.mapAccumL (\(Vector2 i acc) lvl ->
                                       let m    = length lvl
                                           acc' = acc + m
                                       in ( Vector2 (i+1) acc', Level i m acc' lvl)
                                   ) (Vector2 0 0) $ levels tr


-- | contracts the plane graph so that we get a spanning tree of diameter at most sqrt(n).
contract :: PlaneGraph k v e -> Tree k -> (PlaneGraph k v e, Tree k)
contract = undefined

trim _ _ tr = tr
-- TODO:

-- -- | Given a spanning tree of the graph that has diameter r, compute
-- -- a separator of size at most 2r+1
-- planarSeparatorTree       :: (Ord k
--                              , Show k
--                              ) => PlaneGraph k v e -> Tree k -> Separator k
-- planarSeparatorTree gr tr = (sep, foldMap F.toList <$> trees)
--   -- FIXME: continue searching
--   where
--     e = Set.findMin $ graphEdges gr `Set.difference` treeEdges tr
--     (sep, trees) = traceShowWith ("separator: ",)
--                  $ fromSplitTree . splitLeaf gr e
--                  $ traceShowWith ("splitTree",e,)
--                    $ splitTree e $ traceShowWith ("TR",) tr

--------------------------------------------------------------------------------
-- * Spliting the tree

-- data SplitTree l a = RootSplit l [Tree a] (Path l a) [Tree a]
--                    | Prefix (Path (a, Split l a) a)
--                    deriving (Show,Eq)
-- -- still not quite right, since now we can't represent rotosplits lower than the root .

-- newtype SplitTree a l = SplitTree (Path a (Split a l))
--   deriving (Show,Eq,Functor)

-- -- | A path in the tree that ends at a "leaf" in which we store something of type l
-- newtype Path a l = MkPath (NonEmpty (PathNode a l))
--   deriving (Show,Eq,Functor)

-- data PathNode a l = PathLeaf l
--                   | PathNode a [Tree a] [Tree a]
--                   deriving (Show,Eq,Functor)

-- pattern Leaf   :: l -> Path a l
-- pattern Leaf l = MkPath (PathLeaf l :| [])

-- (<|) :: PathNode a l -> Path a l -> Path a l
-- n <| (MkPath path) = MkPath $ n NonEmpty.<| path

-- pattern Path                     :: a -> [Tree a] -> Path a l -> [Tree a] -> Path a l
-- pattern Path u before path after <- (unconsPath -> Just (u, before, after, path))
--   where
--     Path u before path after = PathNode u before after <| path


-- unconsPath :: Path a l -> Maybe (a, [Tree a], [Tree a], Path a l)
-- unconsPath = \case
--   MkPath (PathNode u before after :| path') -> (u,before,after,) . MkPath
--                                            <$> NonEmpty.nonEmpty path'
--   _                                         -> Nothing
-- {-# COMPLETE Leaf, Path #-}


-- -- | The split node where the two paths diverge
-- data Split a l =
--     RootSplit l -- ^ apparently root is the split we are looking for.
--               [Tree a] (Path a l) [Tree a]
--   | NodeSplit a -- ^ label of the node we are splitting
--               [Tree a] -- ^ children before the left path
--               (Path a l)
--               -- ^ the value stored at the left node (i.e. the leaf) we argoing to,
--               -- and the pato that goes there.
--               [Tree a] -- ^ middle nodes
--               (Path a l)
--               -- ^ the value stored at the right node we argoing to, and the pato that
--               -- goes there.
--               [Tree a]
--   deriving (Show,Eq,Functor)

-- -- | Given an non-tree edge (v,w), split the tree usign the root to v,w paths
-- splitTree     :: Eq a => (a,a) -> Tree a -> SplitTree a (Tree a)
-- splitTree e t = case splitTree' e t of
--   Both split -> split
--   _          -> error "splitTree: absurd, didn't find both endpoints"

-- data ResultF a b = NotFound
--                  | Single a
--                  | Both b
--                  deriving (Show,Eq,Functor)

-- type Result a = ResultF (VW, Path a (Tree a) ) (SplitTree a (Tree a))

-- data VW = V | W

-- other     :: p -> p -> VW -> p
-- other v w = \case
--   V -> w
--   W -> v

-- data Loc a b = Here a | There b deriving (Show,Eq)

-- -- | Implementation of splitTree; i.e. tries to find both endpoints of the given edge.
-- splitTree'       :: Eq a => (a,a) -> Tree a -> Result a
-- splitTree' (v,w) = fmap SplitTree . go
--   where
--     -- Handle the cases that we find one of the elemtns (identified by 'found') here.
--     here found tr chs = case findNodes w chs of
--       Nothing                    -> Single (found, Leaf tr)
--       Just (before, after, path) -> Both . Leaf $ RootSplit tr before path after

--     go tr@(Node u chs)
--       | u == v    = here V tr chs
--       | u == w    = here W tr chs
--       | otherwise = case foldr process (NotFound, []) chs of
--           (NotFound, _)                     -> NotFound
--           (Single (middle, (x,path)),after) -> Single (x, PathNode u middle after <| path)
--           (Both (before, both'), after)     -> Both $ case both' of
--                 Here  (lp,middle,rp)  -> Leaf $ (NodeSplit u before lp middle rp after)
--                 There path            -> PathNode u before after <| path

--     process ch@(Node u chs) = \case
--       (NotFound, after)            -> case go ch of
--         NotFound          -> (NotFound,              ch:after)
--         Single rightPath  -> (Single ([], rightPath),   after)
--         Both split        -> (Both   ([], There split), after)

--       (Single (middle, path@(x, rightPath)), after)
--         | other v w x == u -> (Both ([], Here (Leaf ch, middle, rightPath)), after)
--         | otherwise        -> case pathNode u <$> findNodes (other v w x) chs of
--             Nothing       -> (Single (ch:middle,path),                       after)
--             Just leftPath -> (Both ([], Here (leftPath, middle, rightPath)), after)

--       (Both   (before, split),     after) -> (Both (ch:before, split), after)

-- -- | Search for a given element in a bunch of trees. Returns the path towards
-- -- the node if we find it.
-- findNodes   :: Eq a => a  -> [Tree a]  -> Maybe ([Tree a], [Tree a], Path a (Tree a))
-- findNodes v = go
--   where
--     go chs = case foldr process (Nothing, []) chs of
--                (Nothing, _)                 -> Nothing
--                (Just (before, path), after) -> Just (before, after, path)

--     process ch = \case
--       (Nothing,             after) -> case findNode' ch of
--         Nothing   -> (Nothing,        ch:after)
--         Just path -> (Just ([],path),    after)
--       (Just (before, path), after) -> (Just (ch:before, path), after)

--     findNode' t@(Node u chs)
--       | u == v    = Just (Leaf t)
--       | otherwise = pathNode u <$> go chs

-- -- | Smart constructor for producign a pathNode
-- pathNode                        :: a -> ([Tree a], [Tree a], Path a l) -> Path a l
-- pathNode u (before, after, path) = PathNode u before after <| path

----------------------------------------

-- type EndPoint a = (a, [Tree a], [Tree a])

-- -- | Split the leaf of the path
-- splitLeaf            :: Ord k
--                      => PlaneGraph k v e
--                      -> (k,k) -> SplitTree k (Tree k) -> SplitTree k (EndPoint k)
-- splitLeaf gr (v',w') = fmap $ \(Node u chs) -> split u chs (if u == v' then w' else v')
--   where
--     split v chs w = case List.break ((== w) . snd) adjacencies of
--                       (before, _:after) -> (v, mapMaybe fst before, mapMaybe fst after)
--                       _                 -> error "splitLeaf: absurd. edge not found!?"
--       where
--         adjacencies = annotateSubSet root chs $ maybe [] (Map.elems . fst) (Map.lookup v gr)

-- -- | Given a tagging function, a subset, and the full set, tag the elements in the full set
-- -- with whether or not they are present in the subset. Both sets should be sorted.
-- annotateSubSet   :: Eq b => (a -> b) -> [a] -> [b] -> [(Maybe a,b)]
-- annotateSubSet f = go
--   where
--     go []            fullSet = map (Nothing,) fullSet
--     go subSet@(x:xs) (y:ys)
--       | f x == y                         = (Just x,  y) : go xs     ys
--       | otherwise                        = (Nothing, y) : go subSet ys
--     go _             []      = [] -- this case should not really happen if the first is a subset


-- -- | Turn the split tree into a separator, and the trees inside the cycle, and outside the
-- -- separator.
-- fromSplitTree               :: Eq a => SplitTree a (EndPoint a) -> ([a],Vector 2 [Tree a])
-- fromSplitTree (SplitTree t) = go t
--   where
--     go = \case
--       Leaf split               -> fromSplit split
--       Path u before path after -> let (sep,Vector2 inside outside) = go path
--                                   in (u : sep,Vector2 inside (before <> outside <> after))

-- -- | Handling a split node
-- fromSplit :: Eq a => Split a (EndPoint a) -> ([a],Vector 2 [Tree a])
-- fromSplit = \case
--   RootSplit (v,beforeV,afterV) _ path _ -> case path of
--     Leaf (_,_,_)     -> error "w is a child of v, that shouldn't really happen"
--     Path u _ path' _ -> case List.break ((== u) . root) beforeV of
--         -- edge vw lies after the path from v via u to w
--         (before, _:insideV)  -> (v : u : sep, Vector2 inside outside)
--           where
--             (sep, Vector2 insideU beforeU) = fromPath After path'
--             inside  = insideU <> insideV
--             outside = before <> beforeU <> afterV
--         -- ede vw lies before the path from v via u to w
--         _                     -> case List.break ((== u) . root) afterV of
--           (middle, _:afterU) -> (v : u : sep, Vector2 inside outside)
--             where
--               (sep, Vector2 insideU afterW) = fromPath Before path'
--               inside  = middle <> insideU
--               outside = beforeV <> afterW <> afterU
--           _                   -> error "fromSplit. Rootsplit (v,w) not found"
--   NodeSplit u before lp middle rp after -> (u : lSep <> rSep, Vector2 inside outside)
--     where
--     (lSep, Vector2 lInside lOutside) = fromPath After  lp
--     (rSep, Vector2 rInside rOutside) = fromPath Before rp

--     inside  = lInside  <> middle <> rInside
--     outside = before <> lOutside <> rOutside <> after

--------------------------------------------------------------------------------

-- | Find the last element matching some predicate
findR   :: (a -> Bool) -> [a] -> Maybe a
findR p = List.find p . reverse



-- if the input graph is connected, are subgraph A and SubGraphB then connected?
-- I don't think so; in particular; the "outer layers", so Graph B may be disconnected I guess.


-- data Separators planarGraph =


-- planarSeparators :: PlanarGraph_ planarGraph
--                  => planarGraph -> Tree