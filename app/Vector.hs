{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Vector
  ( Nat(..)
  , Vec(..)
  , Fin(..)
  , (!)
  , AVec(..)
  , fromList
  , Natty(..)
  ) where

import           Data.Foldable

data Nat = Zero | Suc Nat
  deriving (Show, Eq)

data Fin :: Nat -> * where
  FZero ::Fin (Suc n)
  FSuc ::Fin n -> Fin (Suc n)

deriving instance (Show (Fin n))

--- source: https://softwareengineering.stackexchange.com/questions/276867/is-it-possible-to-bake-dimension-into-a-type-in-haskell
data Vec :: Nat -> * -> * where
  VNil ::Vec Zero a
  VCons ::a -> Vec n a -> Vec (Suc n) a

deriving instance (Show a) => Show (Vec n a)

(!) :: Vec n a -> Fin n -> a
VCons x _  ! FZero  = x
VCons _ xs ! FSuc n = xs ! n

mapV :: (a -> b) -> Vec n a -> Vec n b
mapV _ VNil         = VNil
mapV f (VCons x xs) = VCons (f x) (mapV f xs)

instance Functor (Vec n) where
  fmap = mapV

data Natty (n :: Nat) where
  Zy ::Natty Zero -- pronounced 'zed-y'
  Sy ::Natty n -> Natty (Suc n) -- pronounced 'ess-y'

deriving instance Show (Natty n)

data AVec a = forall n . AVec (Natty n) (Vec n a)

deriving instance (Show a) => Show (AVec a)

fromList :: [a] -> AVec a
fromList = foldr cons nil
 where
  cons x (AVec n xs) = AVec (Sy n) (VCons x xs)
  nil = AVec Zy VNil



















-- foldrV :: (a -> b -> b) -> b -> Vec n a -> b
-- foldrV _ acc VNil         = acc
-- foldrV f acc (VCons x xs) = f x (foldrV f acc xs)

-- instance Foldable (Vec n) where
--   foldr = foldrV


-- lastV :: Vec (Suc n) a -> a
-- lastV (VCons x VNil) = x
-- lastV (x `VCons` (y `VCons` xs)) = lastV (y `VCons` xs)



-- lastV' :: Vec (Suc n) a -> a
-- lastV' (VCons x VNil) = x
-- lastV' (VCons x xs@(VCons y ys)) = lastV' xs

-- initV :: Vec (Suc n) a -> Vec n a
-- initV (x `VCons` (y `VCons` xs)) = VCons x (initV (y `VCons` xs))
-- initV (VCons x VNil) = VNil

-- appendV :: Vec n a -> a -> Vec (Suc n) a
-- appendV VNil a = VCons a VNil
-- appendV (VCons x xs) a = VCons x (appendV xs a)

-- append2V :: Vec n a -> a -> a -> Vec (Suc (Suc n)) a
-- append2V VNil a b = a `VCons` (b `VCons` VNil)
-- append2V (VCons x xs) a b = VCons x (append2V xs a b)

-- sequenceV :: Applicative m => Vec n (m a) -> m (Vec n a)
-- sequenceV VNil         = pure VNil
-- sequenceV (VCons x xs) = VCons <$> x <*> sequenceV xs

-- -- singleton :: a -> Vec (Suc Zero) a
-- -- singleton a = VCons a VNil

-- instance Traversable (Vec n) where
--   sequenceA = sequenceV


--- source: https://softwareengineering.stackexchange.com/questions/276867/is-it-possible-to-bake-dimension-into-a-type-in-haskell
