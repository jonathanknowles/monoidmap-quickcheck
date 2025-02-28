module Data.MonoidMap.QuickCheck.Instances.Arbitrary ()
where

import Data.Monoid.Null
    ( MonoidNull
    )
import Data.MonoidMap
    ( MonoidMap
    )
import Data.Ord
    ( Ord
    )
import Test.QuickCheck
    ( Arbitrary (arbitrary, shrink)
    )

import qualified Data.MonoidMap.QuickCheck as MonoidMap

instance
    (Arbitrary k, Arbitrary v, Ord k, MonoidNull v)
    => (Arbitrary (MonoidMap k v))
    where
    arbitrary = MonoidMap.arbitrary
    shrink = MonoidMap.shrink
