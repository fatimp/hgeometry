--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Vector.Unpacked
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Unpacked low dimensional vectors
--
--------------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HGeometry.Vector.Unpacked
  ( Vector(Vector1, Vector2, Vector3, Vector4)
  ) where

import           Control.DeepSeq (NFData)
import           Control.Lens
import           Control.Monad.State
-- import qualified Data.Foldable as F
import qualified Data.Functor.Apply as Apply
import           Data.Functor.Classes (readData, readUnaryWith)
-- import           Data.Kind (Type)
import qualified Data.List as List
import           Data.Proxy
-- import           Data.Semigroup
-- import           Data.Type.Ord
import           GHC.Generics (Generic)
import           GHC.TypeLits (KnownNat, natVal)
import           HGeometry.Vector.Class
import           R
import qualified V1
import qualified V2
import qualified V3
import qualified V4
import           VectorDef
-- import           GHC.Generics
-- import           HGeometry.Properties
import           Text.Read (Read (..))
import           System.Random (Random (..))
import           System.Random.Stateful (UniformRange(..), Uniform(..))
-- import qualified Data.Vector.Generic as GV
-- import           Data.Vector.Generic.Mutable (MVector(basicInitialize))
-- import qualified Data.Vector.Generic.Mutable as GMV
-- import qualified Data.Vector.Unboxed as UV
-- import qualified Data.Vector.Unboxed.Mutable as UMV
-- import qualified HGeometry.Number.Radical as Radical



--------------------------------------------------------------------------------
-- * 1 Dimensional Vectors

-- | 1D vectors
newtype instance Vector 1 R = V_1 V1.Vec
  deriving newtype (Eq,Ord,Generic,NFData)

-- | Construct a 1 dimensional vector
pattern Vector1   :: R -> Vector 1 R
pattern Vector1 x = V_1 (V1.Single x)
{-# COMPLETE Vector1 #-}

_V1 :: Iso' (Vector 1 R) V1.Vec
_V1 = iso (\(V_1 v) -> v) V_1

instance VectorLike_ (Vector 1 R) where
  generateA f = V_1 <$> generateA f
  {-# INLINE generateA #-}
  components = components' _V1
  {-# INLINE components #-}
  component' i = component'' _V1 i
  {-# INLINE component' #-}

instance Additive_ (Vector 1 R) where
  zero = V_1 zero
  {-# INLINE zero #-}
  liftU2 f (V_1 v) (V_1 v')  = V_1 $ liftU2 f v v'
  {-# INLINE liftU2 #-}
  liftI2 f (V_1 v) (V_1 v')  = V_1 $ liftI2 f v v'
  {-# INLINE liftI2 #-}
  liftI2A f (V_1 v) (V_1 v') = V_1 <$> liftI2A f v v'
  {-# INLINE liftI2A #-}

-- we implement the V1 stuff manually; since the VD setup requires the cons library

--------------------------------------------------------------------------------
-- | Convenience constructors

-- | Construct a 2 dimensional vector
pattern Vector2     :: R -> R -> Vector 2 R
pattern Vector2 x y = V2.V_D (V2.Cons x
                                      (V1.Single y)
                             )
{-# COMPLETE Vector2 #-}

-- | Construct a 3 dimensional vector
pattern Vector3       :: R -> R -> R -> Vector 3 R
pattern Vector3 x y z = V3.V_D (V3.Cons x
                                        (V2.Cons y (V1.Single z))
                               )
{-# COMPLETE Vector3 #-}

-- | Construct a 4 dimensional vector
pattern Vector4         :: R -> R -> R -> R -> Vector 4 R
pattern Vector4 x y z w = V4.V_D (V4.Cons x
                                   (V3.Cons y
                                     (V2.Cons z (V1.Single w)))
                                 )
{-# COMPLETE Vector4 #-}

--------------------------------------------------------------------------------
-- * Additional Functionality


--------------------------------------------------------------------------------
-- * Generic instances

instance ( VectorLike_ (Vector d r)
         , Show r
         , KnownNat d
         ) => Show (Vector d r) where
  -- | Show implementation for vectors
  showsPrec k v = showParen (k > app_prec) $
                     showString constr . showChar ' ' .
                     unwordsS (map (showsPrec 11) (v^..components))
    where
      app_prec = 10
      constr   = "Vector" <> show (fromIntegral (natVal @d Proxy))
      unwordsS = foldr (.) id . List.intersperse (showChar ' ')

instance ( VectorLike_ (Vector d r)
         , Read r
         , KnownNat d
         ) => Read (Vector d r) where
  readPrec = readData $
      readUnaryWith (replicateM d readPrec) constr $ \rs ->
        case vectorFromList rs of
          Just p -> p
          _      -> error "internal error in HGeometry.Vector read instance."
    where
      d        = fromIntegral (natVal @d Proxy)
      constr   = "Vector" <> show d


instance Additive_ (Vector d r) => Metric_ (Vector d r)

-- instance Generic (Vector d r) => NFData (Vector d r)

instance ( Additive_ (Vector d r)
         , UniformRange r
         ) => UniformRange (Vector d r) where
  uniformRM (lows,highs) gen = Apply.unwrapApplicative $
      liftI2A (\l h -> Apply.WrapApplicative $ uniformRM (l,h) gen) lows highs

instance (VectorLike_ (Vector d r), Uniform r) => Uniform (Vector d r) where
  uniformM gen = generateA (const $ uniformM gen)
instance (Additive_ (Vector d r), Uniform r, UniformRange r) => Random (Vector d r) where


--------------------------------------------------------------------------------
-- * Helpers

-- | implementation of component
components'    :: (VectorLike_ vecImpl, IxValue vecImpl ~ R)
               => Iso' vector vecImpl -> IndexedTraversal1' Int vector R
components' is = is.components
{-# INLINE components' #-}

-- | implementation of component'
component''      :: (VectorLike_ vecImpl, IxValue vecImpl ~ R)
                      => Iso' vector vecImpl -> Int -> IndexedTraversal' Int vector R
component'' is i = is.component' i
{-# INLINE component'' #-}
