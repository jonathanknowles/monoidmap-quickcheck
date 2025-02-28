module Data.MonoidMap.QuickCheck.Instances.Function ()
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
    ( Function (function)
    )

import qualified Data.MonoidMap.QuickCheck as MonoidMap

instance
    (Function k, Function v, Ord k, MonoidNull v)
    => Function (MonoidMap k v)
    where
    function = MonoidMap.function
