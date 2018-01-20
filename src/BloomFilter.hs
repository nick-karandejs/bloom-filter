module BloomFilter (
    Bloom
  , length
  , elem
  , notElem
  , fromList
) where

import BloomFilter.Internal
import BloomFilter.Mutable (insert, new)
import Data.Array.ST (runSTUArray)
import Data.Array.IArray ((!), bounds)
import Data.Word (Word32)
import Prelude hiding (length, elem, notElem)

length :: Bloom a -> Int
length = fromIntegral . len

len :: Bloom a -> Word32
len = succ . snd . bounds . blmArray

-- Check element has been recorded by reading bits at computed hash values
elem :: a -> Bloom a -> Bool
element `elem` bfilter =
    all testBit (blmHash bfilter element)
    where testBit hash = blmArray bfilter ! (hash `mod` len bfilter)

notElem :: a -> Bloom a -> Bool
element `notElem` bfilter =
    not $ element `elem` bfilter

-- Could also use `freeze` from MArray here, but it's less efficient
-- as it copies the array from STUArray.
fromList :: (a -> [Word32]) -> Word32 -> [a] -> Bloom a
fromList hashFuns filterSize values =
    B hashFuns updatedArray
    where updatedArray = runSTUArray $ do
            mutBFilter <- new hashFuns filterSize
            mapM_ (insert mutBFilter) values
            return $ mutArray mutBFilter

