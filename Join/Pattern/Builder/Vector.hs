{-# LANGUAGE GADTs
            ,TypeOperators
  #-}
{-|
Module      : Join.Pattern.Builder.Vector
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

Non-empty Vectors, indexed by their length.
-}
module Join.Pattern.Builder.Vector
  (Vector(VOne,VAnd)
  ,head
  ,tail
  ,zip
  ,append
  ,snoc
  ,replicate
  ,replicateM
  ,mapVector
  ,mapMVector
  ,toList
  ,fromNatural
  ) where

import Prelude hiding (replicate,zip,tail,head)

import Join.Pattern.Builder.Natural

-- | A Vector which contains One or many elements, indexed in 'n::Nat'.
data Vector n a where

  -- ^ One vector element.
  VOne :: a -> Vector One a

  -- ^ Many vector elements.
  VAnd :: a -> Vector n a -> Vector (Suc n) a

head :: Vector n a -> a
head (VOne a)   = a
head (VAnd a _) = a

tail :: Vector (Suc n) a -> Vector n a
tail (VAnd a as) = as

zip :: Vector n a -> Vector n b -> Vector n (a,b)
zip (VOne a)    (VOne b)    = VOne (a,b)
zip (VAnd a as) (VAnd b bs) = VAnd (a,b) (zip as bs)

append :: Vector n a -> Vector m a -> Vector (n :+: m) a
append (VOne a) vs = VAnd a vs
append (VAnd a as) vs = VAnd a (append as vs)

snoc :: Vector n a -> a -> Vector (Suc n) a
snoc (VOne a) x = VAnd a (VOne x)
snoc (VAnd a as) x = VAnd a (snoc as x)

replicate :: Natural n -> a -> Vector n a
replicate One     a = VOne a
replicate (Suc n) a = VAnd a (replicate n a)

replicateM :: Monad m => Natural n -> m a -> m (Vector n a)
replicateM One     ma = ma >>= return . VOne
replicateM (Suc n) ma = do
  a  <- ma
  as <- replicateM n ma
  return $ VAnd a as

mapVector :: (a -> b) -> Vector n a -> Vector n b
mapVector f (VOne a) = VOne (f a)
mapVector f (VAnd a as) = VAnd (f a) (mapVector f as)

mapMVector :: Monad m => (a -> m b) -> Vector n a -> m (Vector n b)
mapMVector f (VOne a) = f a >>= return . VOne
mapMVector f (VAnd a as) = do
  b  <- f a
  bs <- mapMVector f as
  return $ VAnd b bs

toList :: Vector n a -> [a]
toList (VOne a) = [a]
toList (VAnd a as) = a : toList as

fromNatural :: (Int -> a) -> Natural n -> Vector n a
fromNatural f n = fromNatural' 1 f n
  where
    fromNatural' :: Int -> (Int -> a) -> Natural n -> Vector n a
    fromNatural' i f One     = VOne (f i)
    fromNatural' i f (Suc n) = VAnd (f i) (fromNatural' (i+1) f n)

