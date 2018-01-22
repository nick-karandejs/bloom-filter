module BloomFilter.Easy (
    suggestSizes
  , easyList
  , suggestParams

  -- re-export useful names from BloomFilter 
  , B.Bloom
  , B.length
  , B.elem
  , B.notElem
) where

{- import Data.Hashable -}
import BloomFilter.Hash (Hashable, doubleHash)
import qualified BloomFilter as B

import Data.List (genericLength)
import Data.Maybe (catMaybes)
import Data.Word (Word32)

easyList :: (Hashable a) => Double -> [a] -> Either String (B.Bloom a)
easyList errRate values =
    case suggestParams (genericLength values) errRate of
        Left err -> Left err
        Right (numBits, numHashes) -> Right bfilter
            where bfilter = B.fromList (doubleHash numHashes) numBits values

suggestParams :: Integer -> Double -> Either String (Word32, Int)
suggestParams capacity errRate
    | capacity <= 0 = Left "Capacity too small"
    | errRate <= 0 || errRate >= 1 = Left "Invalid error rate"
    | null saneSizes = Left "Capacity too large"
    | otherwise = Right (minimum saneSizes)
    where maxWord32 = fromIntegral (maxBound :: Word32)
          prune (numBits, numHashes)
            | numBits > maxWord32 - 1 = Nothing
            | otherwise = Just (ceiling numBits, truncate numHashes)
          saneSizes = catMaybes . map prune $ suggestSizes capacity errRate

suggestSizes :: Integer -> Double -> [(Double, Double)]
suggestSizes capacity errRate =
    let c = fromIntegral capacity
    in [(((-k) * c / log (1 - (errRate ** (1 / k)))), k) | k <- [1..50]]

