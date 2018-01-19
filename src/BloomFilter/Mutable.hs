module BloomFilter.Mutable (
    MutBloom
  , elem
  , notElem
  , insert
  , length
  , new
) where

import Control.Monad (liftM)
import Control.Monad.ST (ST)
import Data.Array.MArray (getBounds, newArray, writeArray, readArray)
import Data.Word (Word32)
import Prelude hiding (elem, length, notElem)

import BloomFilter.Internal (MutBloom(..))

elem :: a -> MutBloom s a -> ST s Bool
elem element bfilter =
    computeIndices bfilter element
    >>= allM (readArray (mutArray bfilter))

notElem :: a -> MutBloom s a -> ST s Bool
notElem element bfilter = liftM not $ elem element bfilter

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM pred (x:xs) = do
    res <- pred x
    if res
        then allM pred xs
        else return False
allM _ [] = return True

length :: MutBloom s a -> ST s Word32
length bfilter = (succ . snd) `liftM` getBounds (mutArray bfilter)

new :: (a -> [Word32]) -> Word32 -> ST s (MutBloom s a)
-- newArray returns an array in a monad, so `MB hash` needs to accept a monad
-- hence the lift
new hash numBits = MB hash `liftM` newArray (0, numBits-1) False

insert :: MutBloom s a -> a -> ST s ()
insert bfilter element =
    computeIndices bfilter element
    >>= mapM_ (\bitIndex -> writeArray (mutArray bfilter) bitIndex True)

computeIndices :: MutBloom s a -> a -> ST s [Word32]
computeIndices bfilter element = do
    modulus <- length bfilter
    return $ map (`mod` modulus) (mutHash bfilter element)
