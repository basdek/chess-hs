module DChains
(
  D(..),
  isOpen,
  isClosed,
  isSemiClosed,
  close,
  semiClose,
  effectuateDChain,
  liftD,
  unliftD,
  fromD,
  dEq,
) where

import Data.Maybe

data D a = Open a | SemiClosed a | Closed a deriving (Show, Eq, Ord)

isOpen :: D a -> Bool
isOpen (Open a) = True
isOpen _ = False

isClosed :: D a -> Bool
isClosed (Closed a) = True
isClosed _ = False

isSemiClosed :: D a -> Bool
isSemiClosed (SemiClosed a) = True
isSemiClosed _ = False

open :: a -> D a
open x = Open x

semiClose :: a -> D a
semiClose x = SemiClosed x

close :: a -> D a
close x = Closed x

instance Functor D where
  fmap f (Open x) = open $ f x
  fmap f (SemiClosed x) =  semiClose $ f x
  fmap f (Closed x) = close $ f x

fromD :: D a -> a
fromD (Open x) = x
fromD (Closed x) = x
fromD (SemiClosed x) = x


f :: [D a] -> [D a] -> [D a]
f [] _state = _state
f (Open x : xs) _state = f xs (Open x : _state)
f (Closed x : xs) _state = f [] _state
f (SemiClosed x : xs) _state = f [] (SemiClosed x : _state)

effectuateDChain :: [D a] -> [D a]
effectuateDChain xs = f xs []

liftD :: [a] -> [D a]
liftD xs = map open xs

_unliftD :: [D a] -> [a] -> [a]
_unliftD [] xs = xs
_unliftD (Open a : xs)  _state = _unliftD xs (a : _state) --TODO are three pattern matches necessary?
_unliftD (Closed a : xs) _state = _unliftD xs (a : _state)
_unliftD (SemiClosed a : xs) _state = _unliftD xs (a : _state)

unliftD :: [D a] -> [a]
unliftD xs = _unliftD xs []

type DuoPred a = (a -> a -> Bool)




dEq :: Eq a => D a -> D a -> Bool
dEq a b = (fromD a) == (fromD b)

