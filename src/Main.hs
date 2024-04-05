module Main where

import LiterateTest (add)
import Text.Printf (printf)
import Data.List (insertBy, sortBy)
import Data.Ord (comparing)
import Control.Monad (filterM, replicateM)
import GHC.Base (liftA2)
import TimeslotGen(gen, TimeslotScaleInfo (TimeslotScaleInfo), genTxIndex, genTsTable, createTimeslotGen)
import Data.Foldable (Foldable(fold))
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

main :: IO ()
main =
  -- mapM_ (\(_ , x) -> print x ) (gen 16)
  print 
    $ 
    genTsTable 
    [createTimeslotGen [0..4] 1 1, createTimeslotGen [6..20] 2 8] 
    $ 
    TimeslotScaleInfo 10 10
  -- print $ createTimeslotGen [0..4] 0 8
