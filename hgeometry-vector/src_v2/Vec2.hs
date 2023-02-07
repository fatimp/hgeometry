{-# LANGUAGE AllowAmbiguousTypes #-}
module Vec2 where

import           Control.DeepSeq
import           Control.Lens
import qualified Data.Functor.Apply as Apply
import           Data.Proxy
import           Data.Type.Ord
import           GHC.Generics (Generic)
import           GHC.TypeLits
import           HGeometry.Properties
import           HGeometry.Sigs.R

--------------------------------------------------------------------------------

type D = 2

data Vector = Vector2 {-# UNPACK #-}!R {-# UNPACK #-}!R
  deriving stock (Show,Eq,Ord,Generic)

instance NFData Vector

type instance NumType   Vector = R
type instance Dimension Vector = D

--------------------------------------------------------------------------------

components :: IndexedTraversal1' Int Vector R
components = conjoined trav (itrav.indexed)
  where
    trav                 :: Apply.Apply f => (R -> f R) -> Vector -> f Vector
    trav f (Vector2 x y) = Vector2 <$> f x Apply.<.> f y

    itrav                 :: Apply.Apply f => (Int -> R -> f R) -> Vector -> f Vector
    itrav f (Vector2 x y) = Vector2 <$> f 0 x Apply.<.> f 1 y
{-# INLINE coordinates #-}

--------------------------------------------------------------------------------

type instance Index   Vector = Int
type instance IxValue Vector = R

instance Ixed Vector where
  ix i f v@(Vector2 x y) = case i of
                             0 -> flip Vector2 y <$> f x
                             1 -> Vector2 x      <$> f y
                             _ -> pure v
  {-# INLINE ix #-}

component :: forall i. (i <= D, KnownNat i) => IndexedLens' Int Vector R
component = let i = fromInteger @Int (natVal $ Proxy @i)
        in case i of
             0 -> ilens (\(Vector2 x _) -> (i,x)) (\(Vector2 _ y) x -> Vector2 x y)
             1 -> ilens (\(Vector2 _ y) -> (i,y)) (\(Vector2 x _) y -> Vector2 x y)
             _ -> error "coord: absurd"
{-# INLINE coord #-}
