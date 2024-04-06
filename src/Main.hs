module Main where

import Text.Printf (printf)
import Data.List (insertBy, sortBy)
import Data.Ord (comparing)
import Control.Monad (filterM, replicateM)
import GHC.Base (liftA2)
import TimeslotGen(gen, TimeslotScaleInfo (TimeslotScaleInfo), genTxIndex, genTsTable, createTimeslotGen, buildCtx, SerialData (serialU8), writeTsInFile, intTo2Word8, intToWord8, magicNum)
import Data.Foldable (Foldable(fold))
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.ByteString.Builder (word16Dec, writeFile)
import Prelude hiding (writeFile)

main :: IO ()
main = do
  -- mapM_ (\(_ , x) -> print x ) (gen 16)
  writeTsInFile ctx "ctx"
  -- print $ intTo2Word8 0xffff
  -- writeFile "b16" $ word16Dec 0xffff
  -- print tst
  -- print ctx
    where 
    ctx = buildCtx tst
    tst = genTsTable 
      [createTimeslotGen [0..4] 1 1, createTimeslotGen [6..20] 2 8] 
      $ 
      TimeslotScaleInfo 10 10
  -- print $ createTimeslotGen [0..4] 0 8
