module Data.MonoidMap.QuickCheck.Instances.CoArbitrary ()
where

import Data.MonoidMap
    ( MonoidMap
    )
import Test.QuickCheck
    ( CoArbitrary (coarbitrary)
    )

import qualified Data.MonoidMap.QuickCheck as MonoidMap

instance
    (CoArbitrary k, CoArbitrary v)
    => CoArbitrary (MonoidMap k v)
    where
    coarbitrary = MonoidMap.coarbitrary
