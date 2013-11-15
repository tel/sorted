{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Data.Sorted
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
--
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
--
-- Operations on lazily sorted lists. Use 'Sorted' to indicate a list
-- with a sorted constraint. Intended to be imported qualified.

module Data.Sorted (
  -- * The base type

  -- /Abstract/
  Sorted, toList, sort,

  -- * Iterative creation
  build, insert, singleton,

  -- * Higher-order operations
  pair, map
  ) where

import qualified Data.Foldable as F
import qualified Data.List     as L
import           Data.Monoid
import           Prelude       hiding (elem, map, null)
import           Prelude       as P

newtype Sorted a = Sorted { toList :: [a] }
                   deriving ( Show, Eq, Ord )

sort :: (Ord a, F.Foldable f) => f a -> Sorted a
sort = Sorted . L.sort . F.toList

build :: Ord a => (forall m . Monoid m => (a -> m) -> m) -> Sorted a
build f = f singleton

fromList :: Ord a => [a] -> Sorted a
fromList xs = build (`F.foldMap` xs)

-- | Inserts a new element, "from the front".
--
-- prop> elem x . insert x == const True
insert :: Ord a => a -> Sorted a -> Sorted a
insert x = Sorted . go . toList where
  go []              = [x]
  go (y:ys) | x <= y = x : y : ys
            | x >  y = y : go ys

singleton :: a -> Sorted a
singleton = Sorted . return

instance Ord a => Monoid (Sorted a) where
  mempty  = Sorted mempty
  -- lazy stable sorted merge
  mappend (Sorted as) (Sorted bs) = Sorted (go as bs) where
    go :: Ord a => [a] -> [a] -> [a]
    go [] xs = xs
    go xs [] = xs
    go (x:xs) (y:ys) | x == y = x : y : go xs     ys
                     | x <  y = x     : go xs     (y:ys)
                     | x >  y =     y : go (x:xs) ys

null :: Sorted a -> Bool
null = P.null . toList

-- | Determines whether an element is in a 'Sorted' set. This function
-- takes advantage of 'Sorted' structure to be more efficient than
-- @elem . toList@.
elem :: Ord a => a -> Sorted a -> Bool
elem a = go . toList where
  go [] = False
  go (x:xs) | a <  x = go xs
            | a == x = True
            | a >  x = False

-- | Produces the product of two sorted sets, resulting in a
-- lexicographically sorted set. Along with singleton forms an
-- \"'Applicative'-like\" interface to 'Sorted'.
pair :: Sorted a -> Sorted b -> Sorted (a, b)
pair (Sorted as) (Sorted bs) = Sorted [(a, b) | a <- as, b <- bs ]

-- | 'Sorted' is a category functor, but cannot instantiate 'Functor'
-- because it constrains its domain category.
map :: Ord b => (a -> b) -> Sorted a -> Sorted b
map f = sort . P.map f . toList

-- monotonicMap
