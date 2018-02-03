module Main where

import Control.DeepSeq (NFData(..), deepseq)
{- import Control.Parallel.Strategies (rdeepseq) -}
import Control.Monad (forM_, mapM_)
import qualified Data.ByteString.Char8 as BS
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import qualified BloomFilter.Easy as B


timed :: (NFData a) => String -> IO a -> IO a
timed descr action = do
    start <- getCurrentTime
    ret <- action
    -- Not quite sure, but evaluate `ret` and then get current time,
    -- which is saved into `end`
    end <- ret `deepseq` getCurrentTime
    putStrLn $ show (diffUTCTime end start) ++ " to " ++ descr
    return ret

instance NFData (B.Bloom a) where
    rnf bfilter = B.length bfilter `seq` ()


main = do
    args <- getArgs
    let files | null args = ["words"]
              | otherwise = args
    forM_ files $ \file -> do
        words <- timed "read words" $ BS.lines `fmap` BS.readFile file
        let len = length words
            errRate = 0.01
        putStrLn $ show len ++ " words"
        putStrLn $ "suggested sizings: " ++
                   show (B.suggestParams (fromIntegral len) errRate)
        bfilter <- timed "construct filter" $
            case B.easyList errRate words of
                Left err -> do
                    putStrLn $ "Error: " ++ err
                    exitFailure
                Right bf -> return bf

        timed "query every element" $
            return $ (map (`B.elem` bfilter) words) `deepseq` ()
            {- mapM_ print $ filter (not . (`B.elem` bfilter)) words -}
