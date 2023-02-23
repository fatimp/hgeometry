--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Vector.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- typeclass that expresses that we can essentially add vectors
--
--------------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
module HGeometry.Vector.Class
  ( VectorLike_(..)
  -- , generate
  -- , component, xComponent, yComponent, zComponent, wComponent
  , vectorFromList
  , Additive_(..) -- , negated, (*^), (^*), (^/)--, sumV, basis, unit
  -- , foldMapZip
  -- , sameDirection
  -- , scalarMultiple
  -- , Metric_(..)
  -- , isScalarMultipleOf
  ) where

import           Control.Lens
import           Control.Monad.State
import qualified Data.Foldable as F
import qualified Data.Functor.Apply as Apply
import           Data.Functor.Classes (readData, readUnaryWith)
import           Data.Kind (Type)
import qualified Data.List as List
import           Data.Proxy
import           Data.Semigroup
import           Data.Type.Ord
import           GHC.TypeLits (Nat, KnownNat, natVal)
-- import           GHC.Generics
import           HGeometry.Properties
import           Linear.V1 (V1(..))
import           Linear.V2 (V2(..))
import           Linear.V3 (V3(..))
import           Linear.V4 (V4(..))
import           Text.Read (Read (..))
import           System.Random (Random (..))
import           System.Random.Stateful (UniformRange(..), Uniform(..))
import qualified HGeometry.Number.Radical as Radical

--------------------------------------------------------------------------------

-- $setup
-- >>> import HGeometry.Vector.Unpacked
-- >>> let myVec3 = Vector3 1 10 3

--------------------------------------------------------------------------------

-- -- | d-dimensional vectors
-- data family Vector (d :: Nat) (r :: Type) :: Type

-- type instance Index     (Vector d r) = Int
-- type instance IxValue   (Vector d r) = r
-- type instance NumType   (Vector d r) = r
-- type instance Dimension (Vector d r) = d

--------------------------------------------------------------------------------

-- class Vector_ vector where
--   -- | Convert between an arbitrary vector of dimension d storing
--   -- elements of type r and a Vector d r
--   _Vector :: Iso' vector (Vector (Dimension vector) (IxValue vector))
--   default _Vector :: ( VectorLike_ vector
--                      , VectorLike_  (Vector (Dimension vector) (IxValue vector))
--                      )
--                   => Iso' vector (Vector (Dimension vector) (IxValue vector))
--   _Vector = iso (\v -> generate (\i -> v^?!component' i))
--                 (\v -> generate (\i -> v^?!component' i))
--   {-# INLINE _Vector #-}

-- instance Vector_ (Vector d r) where
--   _Vector = id
--   {-# INLINE _Vector #-}

--------------------------------------------------------------------------------


-- | Types that have a 'components' indexed traversal
class VectorLike_ vector where
  {-# MINIMAL generateA, components #-}

  -- | Generates a vector from a monadic operation (that takes the index)
  generateA :: Applicative f => (Int -> f (IxValue vector)) -> f vector

  -- | An Indexed Traversal over the components of a vector
  --
  -- >>> myVec3 ^.. components
  -- [1,10,3]
  -- >>> myVec ^@.. components
  -- [(0,1),(1,10),(2,3)]
  components :: IndexedTraversal1' Int vector (IxValue vector)

  -- | Access the i^th component. Consider using 'component' instead.
  --
  -- >>> myVec ^@. omponent' 0
  -- (0,1)
  -- >>> myVec & component' 2 %@~ \i x -> 100*i + x
  -- Vector3 1 10 303
  component' :: Int -> IndexedTraversal' Int vector (IxValue vector)
  default component' :: (Index vector ~ Int, Ixed vector)
                          => Int -> IndexedTraversal' Int vector (IxValue vector)
  component' = iix
  {-# INLINE component' #-}


-- | Convert a list of exactly d elements into a vector with dimension d.
--
-- >>> vectorFromList [10,2,3] :: Maybe (Vector 3 Int)
-- Just (Vector 10 2 3)
-- >>> vectorFromList [10,2,3,5] :: Maybe (Vector 3 Int)
-- Nothing
-- >>> vectorFromList [10,2] :: Maybe (Vector 3 Int)
-- Nothing
vectorFromList :: VectorLike_ vector => [IxValue vector] -> Maybe vector
vectorFromList = evalStateT $ do v <- generateA next
                                 rest <- get
                                 guard (null rest)
                                 pure v
  where
    -- Note that this depends on the specific order in which we evaluate
    -- elements in generateA, so arguably this is somewhat dangerous.
    next   :: Int -> StateT [r] Maybe r
    next _ = get >>= \case
               []   -> fail "vectorFromList: no next element"
               x:xs -> do put xs
                          pure x
{-# INLINE vectorFromList #-}

--------------------------------------------------------------------------------
-- * Generic functions on VectorLike things


--------------------------------------------------------------------------------



-- | Basically a copy of the Linear.Additive class
class VectorLike_ vector => Additive_ vector where
  {-# MINIMAL liftU2, liftI2A #-}

  -- -- | zero vector
  -- zero :: Num (IxValue vector) => vector
  -- zero = generate (const 0)
  -- {-# INLINE zero #-}

  -- -- | add two vectors
  -- (^+^) :: Num (IxValue vector) => vector -> vector -> vector
  -- u ^+^ v = liftU2 (+) u v
  -- {-# INLINE (^+^) #-}

  -- -- | subtract vectors
  -- (^-^)   :: Num (IxValue vector) => vector -> vector -> vector
  -- u ^-^ v = u ^+^ negated v
  -- {-# INLINE (^-^) #-}

  -- -- | Linearly interpolate between the two vectors
  -- lerp           :: Num (IxValue vector) => IxValue vector -> vector -> vector -> vector
  -- lerp alpha u v = alpha *^ u ^+^ (1-alpha) *^ v
  -- {-# INLINE lerp #-}

  -- | Apply a function to merge the 'non-zero' components of two
  -- vectors, unioning the rest of the values.
  liftU2       :: (IxValue vector -> IxValue vector -> IxValue vector)
               -> vector -> vector -> vector

  -- -- | Apply a function to the components of two vectors.
  -- liftI2       :: (IxValue vector -> IxValue vector -> IxValue vector)
  --              -> vector -> vector -> vector
  -- liftI2 f u v = runIdentity $ liftI2A (\x x' -> Identity $ f x x') u v
  -- {-# INLINE liftI2 #-}

  -- | Apply an Applicative function to the components of two vectors.
  liftI2A :: Apply.Apply f
          => (IxValue vector -> IxValue vector -> f (IxValue vector)) -> vector -> vector
          -> f vector

-- -- | "zip through the two vectors", folding over the result.
-- --
-- -- as an example, we can implement the dot product of two vectors u and v using:
-- --
-- -- >>> let myDot u v = getSum $ foldMapZip (\x x' -> Sum $ x * x') u v
-- -- >>> myDot (Vector3 1 2 3) (Vector3 10 20 30)
-- -- 140
-- foldMapZip       :: (Semigroup m, Additive_ vector)
--                  => (IxValue vector -> IxValue vector -> m) -> vector -> vector -> m
-- foldMapZip f u v = getConst $ liftI2A (\x x' -> Const $ f x x') u v
-- {-# INLINE foldMapZip #-}

-- -- | unit vector
-- unit :: forall vector. (Additive_ vector, Num (IxValue vector)) => vector
-- unit = over components (const 1) (zero @vector)
-- {-# INLINE unit #-}

-- -- | negate v
-- negated :: (Num (IxValue vector), VectorLike_ vector) => vector -> vector
-- negated = ((-1) *^)
-- {-# INLINABLE negated #-}

-- -- | left scalar multiplication
-- (*^)   :: (Num (IxValue vector), VectorLike_ vector) => IxValue vector -> vector -> vector
-- s *^ v = over components (s*) v
-- {-# INLINABLE (*^) #-}

-- -- | right scalar multiplication
-- (^*)   :: (Num (IxValue vector), VectorLike_ vector)
--        => vector -> IxValue vector -> vector
-- v ^* s = s *^ v
-- {-# INLINABLE (^*) #-}

-- -- | scalar division
-- (^/)   :: (VectorLike_ vector, Fractional (IxValue vector))
--        => vector -> IxValue vector -> vector
-- v ^/ s = v ^* (1/s)
-- {-# INLINABLE (^/) #-}

-- -- | sum a collection of vectors.
-- sumV :: (Foldable f, Additive_ vector, Num (IxValue vector)) => f vector -> vector
-- sumV = F.foldl' (^+^) zero
-- {-# INLINABLE sumV #-}

-- -- | Produce a default basis for a vector space. If the dimensionality
-- -- of the vector space is not statically known, see 'basisFor'.
-- basis :: (Additive_ vector, Num (IxValue vector)) => [vector]
-- basis = basisFor zero
-- {-# INLINABLE basis #-}

-- -- | Produce a default basis for a vector space from which the
-- -- argument is drawn.
-- basisFor :: (Additive_ vector, Num (IxValue vector)) => vector -> [vector]
-- basisFor = \t ->
--    ifoldMapOf components ?? t $ \i _ ->
--      return                  $
--        iover  components ?? t $ \j _ ->
--          if i == j then 1 else 0
-- {-# INLINABLE basisFor #-}

-- instance Monoid c => Additive_ (Const c a) where
--   zero = Const mempty
--   liftU2 _f l _ = l
--   liftI2 _f l _ = l

--------------------------------------------------------------------------------
-- * Metric

-- | The equivalent class of Linear.Metric
--
-- Note that we do not define a distance itself, and that norm and
-- signorm have a Radical constraint rather than Floating.
-- class Additive_ vector => Metric_ vector where
--   {-# MINIMAL #-}

--   -- | Compute the inner product of two vectors or (equivalently)
--   -- convert a vector f a into a covector f a -> a.
--   dot :: Num (IxValue vector) => vector -> vector -> IxValue vector
--   dot u v = sumOf components $ liftI2 (*) u v
--   {-# INLINE dot #-}

--   -- | Compute the squared norm. The name quadrance arises from Norman
--   -- J. Wildberger's rational trigonometry.
--   quadrance   :: Num (IxValue vector) => vector -> IxValue vector
--   quadrance v = dot v v
--   {-# INLINE quadrance #-}

--   -- | Compute the quadrance of the difference
--   qd     :: Num (IxValue vector) => vector -> vector -> IxValue vector
--   qd u v = quadrance $ u ^-^ v
--   {-# INLINE qd #-}

--   -- -- | Compute the distance between two vectors in a metric space
--   -- distance :: Radical (IxValue vector) => vector -> vector -> IxValue vector

--   -- | Compute the norm of a vector in a metric space
--   norm :: Radical.Radical (IxValue vector) => vector -> IxValue vector
--   norm = Radical.sqrt . quadrance
--   {-# INLINE norm #-}

--   -- | Convert a non-zero vector to unit vector.
--   signorm   :: ( Radical.Radical (IxValue vector)
--                , Fractional (IxValue vector)
--                ) => vector -> vector
--   signorm v = v ^/ norm v
--   {-# INLINE signorm #-}

{-


--------------------------------------------------------------------------------
-- * Linear implementations

type instance Dimension (V1 r) = 1
type instance Dimension (V2 r) = 2
type instance Dimension (V3 r) = 3
type instance Dimension (V4 r) = 4

instance VectorLike_ (Vector 1 r) => Vector_ (V1 r)
instance VectorLike_ (Vector 2 r) => Vector_ (V2 r)
instance VectorLike_ (Vector 3 r) => Vector_ (V3 r)
instance VectorLike_ (Vector 4 r) => Vector_ (V4 r)

instance VectorLike_ (V1 r) where
  generateA f = V1 <$> f 0
  {-# INLINE generateA #-}
  components = conjoined traverse' (itraverse' . indexed)
    where
      traverse'  :: Apply.Apply f => (r -> f r) -> V1 r -> f (V1 r)
      traverse' f (V1 x)  = V1 <$> f x
      itraverse' :: Apply.Apply f => (Int -> r -> f r) -> V1 r -> f (V1 r)
      itraverse' f (V1 x) = V1 <$> f 0 x
  {-# INLINE components #-}
  component' i f v@(V1 x) = case (i :: Int) of
                              0 -> V1 <$> indexed f i x
                              _ -> pure v
  {-# INLINE component' #-}

instance VectorLike_ (V2 r) where
  generateA f = V2 <$> f 0 <*> f 1
  {-# INLINE generateA #-}
  components = conjoined traverse' (itraverse' . indexed)
    where
      traverse'  :: Apply.Apply f => (r -> f r) -> V2 r -> f (V2 r)
      traverse' f (V2 x y)  = V2 <$> f x Apply.<.> f y
      itraverse' :: Apply.Apply f => (Int -> r -> f r) -> V2 r -> f (V2 r)
      itraverse' f (V2 x y) = V2 <$> f 0 x Apply.<.> f 1 y
  {-# INLINE components #-}
  component' i f v@(V2 x y) = case (i :: Int) of
                                 0 -> (\x' -> V2 x' y) <$> indexed f i x
                                 1 -> (\y' -> V2 x y') <$> indexed f i y
                                 _ -> pure v
  {-# INLINE component' #-}

instance VectorLike_ (V3 r) where
  generateA f = V3 <$> f 0 <*> f 1 <*> f 2
  {-# INLINE generateA #-}
  components = conjoined traverse' (itraverse' . indexed)
    where
      traverse'  :: Apply.Apply f => (r -> f r) -> V3 r -> f (V3 r)
      traverse' f (V3 x y z)  = V3 <$> f x Apply.<.> f y Apply.<.> f z
      itraverse' :: Apply.Apply f => (Int -> r -> f r) -> V3 r -> f (V3 r)
      itraverse' f (V3 x y z) = V3 <$> f 0 x Apply.<.> f 1 y Apply.<.> f 2 z
  {-# INLINE components #-}
  component' i f v@(V3 x y z) = case (i :: Int) of
                                 0 -> (\x' -> V3 x' y z) <$> indexed f i x
                                 1 -> (\y' -> V3 x y' z) <$> indexed f i y
                                 2 -> (\z' -> V3 x y z') <$> indexed f i z
                                 _ -> pure v
  {-# INLINE component' #-}

instance VectorLike_ (V4 r) where
  generateA f = V4 <$> f 0 <*> f 1 <*> f 2 <*> f 3
  {-# INLINE generateA #-}
  components = conjoined traverse' (itraverse' . indexed)
    where
      traverse'  :: Apply.Apply f => (r -> f r) -> V4 r -> f (V4 r)
      traverse' f (V4 x y z w)  = V4 <$> f x Apply.<.> f y Apply.<.> f z Apply.<.> f w
      itraverse' :: Apply.Apply f => (Int -> r -> f r) -> V4 r -> f (V4 r)
      itraverse' f (V4 x y z w) = V4 <$> f 0 x Apply.<.> f 1 y Apply.<.> f 2 z Apply.<.> f 3 w
  {-# INLINE components #-}
  component' i f v@(V4 x y z w) = case (i :: Int) of
                                    0 -> (\x' -> V4 x' y z w) <$> indexed f i x
                                    1 -> (\y' -> V4 x y' z w) <$> indexed f i y
                                    2 -> (\z' -> V4 x y z' w) <$> indexed f i z
                                    3 -> (\w' -> V4 x y z w') <$> indexed f i w
                                    _ -> pure v
  {-# INLINE component' #-}

instance Additive_ (V1 r) where
  zero   = V1 0
  {-# INLINE zero #-}
  liftU2 f (V1 x) (V1 x') = V1 (f x x')
  {-# INLINE liftU2 #-}
  liftI2 f (V1 x) (V1 x') = V1 (f x x')
  {-# INLINE liftI2 #-}
  liftI2A f (V1 x) (V1 x') = V1 <$> f x x'
  {-# INLINE liftI2A #-}

instance Additive_ (V2 r) where
  zero   = V2 0 0
  {-# INLINE zero #-}
  liftU2 f (V2 x y) (V2 x' y') = V2 (f x x') (f y y')
  {-# INLINE liftU2 #-}
  liftI2 f (V2 x y) (V2 x' y') = V2 (f x x') (f y y')
  {-# INLINE liftI2 #-}
  liftI2A f (V2 x y) (V2 x' y') = V2 <$> f x x' Apply.<.> f y y'
  {-# INLINE liftI2A #-}

instance Additive_ (V3 r) where
  zero   = V3 0 0 0
  {-# INLINE zero #-}
  liftU2 f (V3 x y z) (V3 x' y' z') = V3 (f x x') (f y y') (f z z')
  {-# INLINE liftU2 #-}
  liftI2 f (V3 x y z) (V3 x' y' z') = V3 (f x x') (f y y') (f z z')
  {-# INLINE liftI2 #-}
  liftI2A f (V3 x y z) (V3 x' y' z') = V3 <$> f x x' Apply.<.> f y y' Apply.<.> f z z'
  {-# INLINE liftI2A #-}

instance Additive_ (V4 r) where
  zero   = V4 0 0 0 0
  {-# INLINE zero #-}
  liftU2 f (V4 x y z w) (V4 x' y' z' w') = V4 (f x x') (f y y') (f z z') (f w w')
  {-# INLINE liftU2 #-}
  liftI2 f (V4 x y z w) (V4 x' y' z' w') = V4 (f x x') (f y y') (f z z') (f w w')
  {-# INLINE liftI2 #-}
  liftI2A f (V4 x y z w) (V4 x' y' z' w') = V4 <$> f x x' Apply.<.> f y y' Apply.<.> f z z' Apply.<.> f w w'
  {-# INLINE liftI2A #-}

instance Metric_ (V1 r)
instance Metric_ (V2 r)
instance Metric_ (V3 r)
instance Metric_ (V4 r)


--------------------------------------------------------------------------------


-}
