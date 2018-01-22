{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}
module BloomFilter.Hash (
    Hashable(..)
  , hash
  , doubleHash
) where

import Data.Bits ((.&.), shiftR)
import Foreign.Marshal.Array (withArrayLen)
import Control.Monad (foldM)
import Data.Word (Word32, Word64)
import Foreign.C.Types (CSize(..))
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable, peek, sizeOf)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import System.IO.Unsafe (unsafePerformIO)

-- Here we declare the functions that are implemented in another language
foreign import ccall unsafe "lookup3.h hashword2" hashWord2
    :: Ptr Word32 -> CSize -> Ptr Word32 -> Ptr Word32 -> IO ()

foreign import ccall unsafe "lookup3.h hashlittle2" hashLittle2
    :: Ptr a -> CSize -> Ptr Word32 -> Ptr Word32 -> IO ()

hashIO :: Ptr a -> CSize -> Word64 -> IO Word64
hashIO pVal numBytes salt =
    with (fromIntegral salt) $ \pSalt -> do
        -- p1 points the low word, p2 points to high word
        -- i.e. we split the 64bit salt into two 32bit numbers
        let p1 = castPtr pSalt
            p2 = castPtr pSalt `plusPtr` 4
        computeHashes p1 p2
        -- reads a 64bit value from the pointer
        peek pSalt
    where numWords = numBytes `div` 4
          -- write two hashes into the two pointer locations
          computeHashes p1 p2
            -- check we have a multiple of four numBytes
            | numBytes .&. 3 == 0 = hashWord2 (castPtr pVal) numWords p1 p2
            | otherwise = hashLittle2 (castPtr pVal) numBytes p1 p2


hashStorable :: Storable a => Word64 -> a -> Word64
hashStorable salt k =
    unsafePerformIO . with k $ \ptr ->
        hashIO ptr (fromIntegral $ sizeOf k) salt

hashList :: Storable a => Word64 -> [a] -> IO Word64
hashList salt xs =
    -- put the array into memory and do work on it
    withArrayLen xs $ \len ptr ->
        hashIO ptr (fromIntegral (len * sizeOf x)) salt
    where x = head xs

hashByteString :: Word64 -> Strict.ByteString -> IO Word64
hashByteString salt bs =
    Strict.useAsCStringLen bs $ \(ptr, len) ->
        hashIO ptr (fromIntegral len) salt

rechunk :: Lazy.ByteString -> [Strict.ByteString]
rechunk s
    | Lazy.null s = []
    | otherwise =
        let chunkSize = 64 * 1024
            repack = Strict.concat . Lazy.toChunks
            (pre, suf) = Lazy.splitAt chunkSize s
        in repack pre : rechunk suf


class Hashable a where
    -- | salt -> value to hash -> hash
    hashSalt :: Word64 -> a -> Word64

instance Hashable Char where
    hashSalt = hashStorable

instance Hashable Int where
    hashSalt = hashStorable

instance Hashable Double where
    hashSalt = hashStorable

instance (Storable a) => Hashable [a] where
    hashSalt salt xs = unsafePerformIO $ hashList salt xs

instance (Hashable a, Hashable b) => Hashable (a, b) where
    hashSalt salt (a, b) = hash2 b . hash2 a $ salt

instance (Hashable a, Hashable b, Hashable c) => Hashable (a, b, c) where
    hashSalt salt (a, b, c) = hash2 c . hash2 b . hash2 a $ salt

instance Hashable Strict.ByteString where
    hashSalt salt bs = unsafePerformIO $ hashByteString salt bs

instance Hashable Lazy.ByteString where
    hashSalt salt bs = unsafePerformIO $
        foldM hashByteString salt (rechunk bs)

hash :: Hashable a => a -> Word64
hash = hashSalt 0x106fc397cf62f64d3

hash2 :: Hashable a => a -> Word64 -> Word64
hash2 k salt = hashSalt salt k

-- | Generates a number of hashes from given value
doubleHash :: Hashable a => Int -> a -> [Word32]
doubleHash numHashes value =
    [h1 + h2 * i | i <- [0..num]]
    where num = fromIntegral numHashes
          h = hashSalt 0x9150a946c4a8966e value
          h1 = fromIntegral (h `shiftR` 32) .&. maxBound
          h2 = fromIntegral h
