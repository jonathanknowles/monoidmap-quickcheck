-- |
-- Copyright: © 2025 Jonathan Knowles
-- License: Apache-2.0
module Data.MonoidMap.QuickCheck
    ( -- * Introduction
      -- $_introduction

      -- * Importing from this module
      -- $_importing

      -- * Generating maps
      arbitrary
    , arbitraryBy
    , arbitraryByPairs

      -- * Generating functions
    , coarbitrary
    , function

      -- * Shrinking maps
    , shrink
    , shrinkBy
    , shrinkByPairs
    )
where

import Control.Applicative
    ( (<*>)
    )
import Data.Function
    ( (.)
    )
import Data.Functor
    ( (<$>)
    )
import Data.Monoid.Null
    ( MonoidNull
    )
import Data.MonoidMap
    ( MonoidMap
    )
import Data.Ord
    ( Ord
    )
import Test.QuickCheck.Function
    ( (:->)
    )

import qualified Data.Map.Strict as Map
import qualified Data.MonoidMap as MonoidMap
import qualified Test.QuickCheck as QC

-- $_introduction
-- #_introduction#
--
-- This module contains standalone generators and shrinkers for 'MonoidMap'
-- objects and functions.
--
-- For pre-defined class instances, see "Data.MonoidMap.QuickCheck.Instances".

-- $_importing
-- #_importing#
--
-- This module is designed to be imported __qualified__.
--
-- For example:
--
-- @
-- import qualified Data.MonoidMap            as MonoidMap
-- import qualified Data.MonoidMap.QuickCheck as MonoidMap.QC
-- @

-- | Generates 'MonoidMap' objects.
--
arbitrary
    :: ( QC.Arbitrary k
       , QC.Arbitrary v
       , Ord k
       , MonoidNull v
       )
    => QC.Gen (MonoidMap k v)
arbitrary = arbitraryBy QC.arbitrary QC.arbitrary

-- | Constructs a generator of 'MonoidMap' objects from independent key and
--   value generators.
--
arbitraryBy
    :: ( Ord k
       , MonoidNull v
       )
    => QC.Gen k
    -> QC.Gen v
    -> QC.Gen (MonoidMap k v)
arbitraryBy arbitraryKey arbitraryValue =
    arbitraryByPairs ((,) <$> arbitraryKey <*> arbitraryValue)

-- | Constructs a generator of 'MonoidMap' objects from an integrated generator
--   of key-value pairs.
--
arbitraryByPairs
    :: ( Ord k
       , MonoidNull v
       )
    => QC.Gen (k, v)
    -> QC.Gen (MonoidMap k v)
arbitraryByPairs arbitraryPair =
    MonoidMap.fromList <$> QC.listOf arbitraryPair

-- | Generates arbitrary functions that consume 'MonoidMap' objects.
--
coarbitrary
    :: ( QC.CoArbitrary k
       , QC.CoArbitrary v
       )
    => MonoidMap k v
    -> QC.Gen a
    -> QC.Gen a
coarbitrary = QC.coarbitrary . MonoidMap.toMap

-- | Generates arbitrary functions that consume 'MonoidMap' objects.
--
function
    :: ( QC.Function k
       , QC.Function v
       , MonoidNull v
       , Ord k
       )
    => (MonoidMap k v -> a)
    -> (MonoidMap k v :-> a)
function = QC.functionMap MonoidMap.toMap MonoidMap.fromMap

-- | Shrinks a 'MonoidMap' object.
--
shrink
    :: ( QC.Arbitrary k
       , QC.Arbitrary v
       , Ord k
       , MonoidNull v
       )
    => (MonoidMap k v)
    -> [MonoidMap k v]
shrink = shrinkBy QC.shrink QC.shrink

-- | Constructs a shrinker of 'MonoidMap' objects from independent key and
--   value shrinkers.
--
shrinkBy
    :: (Ord k, MonoidNull v)
    => (k -> [k])
    -> (v -> [v])
    -> (MonoidMap k v -> [MonoidMap k v])
shrinkBy shrinkKey shrinkValue =
    shrinkByPairs (QC.liftShrink2 shrinkKey shrinkValue)

-- | Constructs a shrinker of 'MonoidMap' objects from an integrated shrinker
--   of key-value pairs.
--
shrinkByPairs
    :: (Ord k, MonoidNull v)
    => ((k, v) -> [(k, v)])
    -> (MonoidMap k v -> [MonoidMap k v])
shrinkByPairs shrinkPair m =
    MonoidMap.fromList <$> QC.liftShrink shrinkPair (MonoidMap.toList m)
