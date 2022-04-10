module Algorithms.Geometry.ConvexHull.Minimalist.Hull where

-- import           Algorithms.BinarySearch
import           Algorithms.Geometry.ConvexHull.Minimalist.Point
import           Control.Applicative ((<|>))
import           Data.Ext
import           Data.Geometry.Point (ccw, pattern CCW)
import qualified Data.Geometry.PolyLine as PolyLine
import           Data.Geometry.Properties
import qualified Data.List as List
-- import           Data.Geometry.Triangle
-- import qualified Data.List as List
-- import           Data.List.NonEmpty (NonEmpty(..))
-- import qualified Data.List.NonEmpty as NonEmpty
-- import qualified Data.Sequence as Seq
-- import           Data.Sequence (Seq(..), ViewL(..), ViewR(..))
-- import qualified Data.OrdSeq as OrdSeq
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import           Data.Set (Set)
import           Data.Maybe
import           Data.Kind

--------------------------------------------------------------------------------

minInftyT :: Num r => r
minInftyT = -1000000000 -- FIXME

--------------------------------------------------------------------------------

class Zipper zipper elem where
  -- | Creates a singleton zipper
  singleton :: elem -> zipper elem
  -- | Gets the element under focus
  focus :: zipper elem -> elem
  -- | moves the focus left
  goLeft  :: zipper elem -> Maybe (zipper elem)
  -- | moves the focus right
  goRight :: zipper elem -> Maybe (zipper elem)
  -- | Moves focus to rightmost elem
  goRightMost   :: zipper elem -> zipper elem
  goRightMost h = maybe h goRightMost $ goRight h
  -- | Moves focus to leftmost elem
  goLeftMost   :: zipper elem -> zipper elem
  goLeftMost h = maybe h goLeftMost $ goLeft h
  {-# MINIMAL singleton, focus, goLeft, goRight #-}

class (Zipper hull point, Point point) => SearchableZipper (hull :: Type -> Type) point where
  predOf :: Point point => point -> hull point -> Maybe point
  succOf :: Point point => point -> hull point -> Maybe point

  -- | delete the point from the hull. If we delete the focus, try to
  -- take from the right first, if that does not work, take the new
  -- focus from the left instead.
  --
  -- pre : the hull remains non-empty
  delete :: Point point => point -> hull point -> hull point
  insert :: Point point => point -> hull point -> hull point

  -- | Get the predecessor of the focus
  predOfF :: Point point => hull point -> Maybe point
  predOfF h = predOf (focus h) h
  -- | Get the successor of the focus
  succOfF :: Point point => hull point -> Maybe point
  succOfF h = succOf (focus h) h
  {-# MINIMAL predOf, succOf, delete, insert #-}

class (SearchableZipper hull point, Point point) => Hull (hull :: Type -> Type) point where
  fromBridge :: Point point => Bridge hull point -> hull point


-- | turn a hull into a list of points
toList :: (Hull hull point, Point point) => hull point -> [point]
toList = List.unfoldr (fmap (\h -> (focus h, goRight h))) . Just . goLeftMost

-- | renders the hull at a particular time.
hullAt   :: (Hull hull point, Point point) => Time point -> hull point
         -> Maybe (PolyLine.PolyLine 2 () (NumType point))
hullAt t = PolyLine.fromPoints . fmap (ext . toPt2 t) . toList

--------------------------------------------------------------------------------

-- | Bridge ; so that (focus l, focus r) represents the actual bridge.
data Bridge hull point = Bridge (hull point) (hull point)
                       deriving (Show,Eq)

type instance NumType (Bridge hull point) = NumType point

--------------------------------------------------------------------------------

newtype X point = X { unX :: point } deriving newtype Show

instance Point point => Eq (X point) where
  p == q = p `compare` q == EQ
instance Point point => Ord (X point) where
  (X p) `compare` (X q) = compareX p q

-- | hull zipper

data HullZ point = HullZ (Set (X point)) point (Set (X point))

type instance NumType (HullZ point) = NumType point


instance Show point => Show (HullZ point) where
  showsPrec d (HullZ ll p rr) = showParen (d > app_prec) $ showString "HullZ "
      .  showList (Set.toAscList ll)
      .  showString " " .  showsPrec (app_prec+1) p .  showString " "
      .  showList (Set.toAscList rr)
    where app_prec = 10

instance Point point => Semigroup (HullZ point) where
  l <> r = fromBridge $ bridgeOf l r

instance Point point => Zipper HullZ point where
  singleton p = HullZ Set.empty p Set.empty
  focus (HullZ _ p _) = p

  goLeft (HullZ ll p rr) =
    (\(X p',ll') -> HullZ ll' p' (Set.insert (X p) rr)) <$> Set.maxView ll

  goRight (HullZ ll p rr) =
    (\(X p',rr') -> HullZ (Set.insert (X p) ll) p' rr') <$> Set.minView rr

  goRightMost h@(HullZ ll p rr) = case Set.maxView rr of
    Nothing        -> h
    Just (X r,rr') -> HullZ (ll <> Set.insert (X p) rr') r Set.empty

  goLeftMost h@(HullZ ll p rr) = case Set.minView ll of
    Nothing        -> h
    Just (X l,ll') -> HullZ Set.empty l (ll' <> Set.insert (X p) rr)

instance Point point => SearchableZipper HullZ point where
  succOf q (HullZ ll p rr) = case q `compareX` p of
                               LT -> (unX <$> Set.lookupGT (X q) ll) <|> Just p
                               _  -> unX <$> Set.lookupGT (X q) rr

  predOf q (HullZ ll p rr) = case q `compareX` p of
                               GT -> (unX <$> Set.lookupLT (X q) rr) <|> Just p
                               _  -> unX <$> Set.lookupLT (X q) ll

  delete q (HullZ ll p rr) = case q `compareX` p of
      LT -> HullZ (Set.delete (X q) ll) p rr
      EQ -> case Set.minView rr of
              Nothing         -> case Set.maxView ll of
                Nothing         -> error "HullZ: delete hull is now empty?"
                Just (X p',ll') -> HullZ ll' p' rr
              Just (X p',rr') -> HullZ ll p' rr'
      GT -> HullZ ll p (Set.delete (X q) rr)

  insert q (HullZ ll p rr) = case q `compareX` p of
                               LT -> HullZ (Set.insert (X q) ll) p rr
                               EQ -> error "HullZ: trying to insert existing point"
                               GT -> HullZ ll p (Set.insert (X q) rr)


  succOfF (HullZ _ _ rr) = (\(X p) -> p) <$> Set.lookupMin rr
  predOfF (HullZ ll _ _) = (\(X p) -> p) <$> Set.lookupMax ll

instance Point point => Hull HullZ point where
  fromBridge (Bridge (HullZ ll l _) (HullZ _ r rr)) = HullZ ll l (Set.insert (X r) rr)


-- | Computes the bridge of the two given hulls
bridgeOf       :: (Hull hull point, Point point)
               => hull point -> hull point -> Bridge hull point
bridgeOf l0 r0 = go (goRightMost l0) (goLeftMost r0)
    where
      go l r | isRight' (succOfF r) l r = go l          (goRight' r)
             | isRight' (predOfF l) l r = go (goLeft' l) r
             | otherwise                = Bridge l r

      isRight' Nothing  _ _ = False
      isRight' (Just x) l r = ccw (toPt l) (toPt r) (toPt2 t x) /= CCW

      goLeft'  = fromMaybe (error "goLeft': no left")   . goLeft
      goRight' = fromMaybe (error "goRight': no right") . goRight

      toPt h = toPt2 t (focus h)
      t = minInftyT

--------------------------------------------------------------------------------

  -- goLeft (HullZ ll p rr) = case Seq.viewr ll of
  --                            EmptyR   -> error "cannot go left"
  --                            ll' :> l -> HullZ ll' l (p :<| rr)
  -- goRight (HullZ ll p rr) = case Seq.viewl rr of
  --                             EmptyL    -> error "cannot go right"
  --                             r :< rr' -> HullZ (ll :|> p) r rr'


--------------------------------------------------------------------------------

-- type Hull' = NonEmpty

-- instance Hull NonEmpty where
--   singleton = (:| [])
--   bridgeOf lh rh = undefined
