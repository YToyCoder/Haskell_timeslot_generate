module Main where

import Text.Printf (printf)
import Data.List (insertBy, sortBy)
import Data.Ord (comparing)
import Control.Monad (filterM, replicateM)
import GHC.Base (liftA2)
import TimeslotGen(gen, TimeslotScaleInfo (TimeslotScaleInfo), genTxIndex, genTsTable, createTimeslotGen, buildCtx, SerialData (serialU8))
import Data.Foldable (Foldable(fold))
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

main :: IO ()
main = do
  -- mapM_ (\(_ , x) -> print x ) (gen 16)
  print $ serialU8 ctx
  -- print tst
    where 
    ctx = buildCtx tst
    tst = genTsTable 
      [createTimeslotGen [0..4] 1 1, createTimeslotGen [6..20] 2 8] 
      $ 
      TimeslotScaleInfo 10 10
  -- print $ createTimeslotGen [0..4] 0 8
