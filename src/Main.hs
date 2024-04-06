{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Printf (printf)
import Data.List (insertBy, sortBy)
import Data.Ord (comparing)
import Control.Monad (filterM, replicateM)
import GHC.Base (liftA2)
import TimeslotGen(gen, TimeslotScaleInfo (TimeslotScaleInfo), genTxIndex, genTsTable, createTimeslotGen, buildCtx, SerialData (serialU8), writeTsInFile, intTo2Word8, intToWord8, magicNum, BinTimeslotCtx, TimeslotTable)
import Data.Foldable (Foldable(fold))
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.ByteString.Builder (word16Dec, writeFile)
import Prelude hiding (writeFile)
import TimeslotGenC (TimeslotTableGenC(TimeslotTableGenC), TimeslotTxC (TimeslotTxC, timeslot_index, kind, mem_size), TimeslotTCWrapper (TimeslotTCWrapper, timeslot))
import Data.Aeson (decode, decodeFileStrict)

timeslotC2Ctx :: TimeslotTableGenC -> BinTimeslotCtx
timeslotC2Ctx (TimeslotTableGenC frame slot tx) =
  buildCtx $ genTsTable tsgl $ TimeslotScaleInfo frame slot
  where
    tsgl = map
      (\ x -> createTimeslotGen (timeslot_index x) (kind x) (mem_size x)) tx

timeslotBuild :: TimeslotTableGenC -> (BinTimeslotCtx, TimeslotTable)
timeslotBuild (TimeslotTableGenC frame slot tx) =
  (buildCtx tst, tst)
  where
    tsgl = map
      (\ x -> createTimeslotGen (timeslot_index x) (kind x) (mem_size x)) tx
    tst = genTsTable tsgl $ TimeslotScaleInfo frame slot


main :: IO ()
main = do
  timeslotc <- (decodeFileStrict "ts.conf" :: IO (Maybe TimeslotTCWrapper))
  case timeslotc of
    Just tsc -> do
      print tsc
      print ctx
      print tst
      writeTsInFile ctx "ctx"
      where
        (ctx, tst)= timeslotBuild $ timeslot tsc
    Nothing -> print ""
  -- print (decode tsf :: Maybe TimeslotTCWrapper)
  -- print (decode tx :: Maybe TimeslotTxC)
  -- mapM_ (\(_ , x) -> print x ) (gen 16)
  -- writeTsInFile ctx "ctx"
  -- print $ intTo2Word8 0xffff
  -- writeFile "b16" $ word16Dec 0xffff
  -- print tst
  -- print ctx
    -- ctx = buildCtx tst
    -- tst = genTsTable 
    --   [createTimeslotGen [0..4] 1 1, createTimeslotGen [6..20] 2 8] 
    --   $ 
    --   TimeslotScaleInfo 10 10
  -- print $ createTimeslotGen [0..4] 0 8
