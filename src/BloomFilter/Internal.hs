module BloomFilter.Internal (
    Bloom(..)
  , MutBloom(..)
) where

import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray)
import Data.Word (Word32)

data Bloom a = B {
    -- function to compute hashes for the given element
    blmHash :: (a -> [Word32])
    -- Word32 is index type, Bool is element type
  , blmArray :: UArray Word32 Bool
}

data MutBloom s a = MB {
    -- function to compute hashes for the given element
    mutHash :: (a -> [Word32])
  , mutArray :: STUArray s Word32 Bool
}
