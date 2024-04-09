
module Main where

import Text.Printf (printf)
import Data.List (insertBy, sortBy)
import Data.Ord (comparing)
import Control.Monad (filterM, replicateM)
import GHC.Base (liftA2)
import TimeslotGen(gen, TimeslotScaleInfo (TimeslotScaleInfo), genTsTable, createTimeslotGen, buildCtx, SerialData (serialU8), writeTsInFile, intTo2Word8, intToWord8, magicNum, BinTimeslotCtx, TimeslotTable)
import Data.Foldable (Foldable(fold))
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.ByteString.Builder (word16Dec, writeFile)
import Prelude hiding (writeFile)
import TimeslotGenC (TimeslotTableGenC(TimeslotTableGenC), TimeslotTxC (TimeslotTxC, timeslot_index, kind, mem_size), TimeslotTCWrapper (TimeslotTCWrapper, timeslot))
import Data.Aeson (decode, decodeFileStrict)
import TsCmd(TsCmdOption (TsCmdOption, timeslotPrint, buildCtxPrint, binGen, TsCmdReadBin), option, cmds)
import System.Console.CmdArgs ( cmdArgs )
import TimeslotRead (readTimeslotFromFile, word8l2ToInt)
import Data.Word (Word8)
import qualified Data.ByteString as BS

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
    tst = genTsTable tsgl $ TimeslotScaleInfo slot frame

data CmdlineProcResult =
  CmdlineErr { cmdOption:: TsCmdOption , err :: String }
  | CmdlineSuc { cmdOption :: TsCmdOption, cmdCtx :: BinTimeslotCtx, cmdTst :: TimeslotTable}

printProc :: CmdlineProcResult -> IO CmdlineProcResult
printProc err@(CmdlineErr opt erro) = return err
printProc suc@(CmdlineSuc opt ctx tst) = do
  if timeslotPrint opt then print tst
  else putStrLn ""
  if buildCtxPrint opt then print ctx
  else putStrLn ""
  return suc

strNotEmptyOr :: String -> String -> String
strNotEmptyOr [] default_v = default_v
strNotEmptyOr str _ = str

cmdlinePreProc :: TsCmdOption -> IO TsCmdOption
cmdlinePreProc opt@(TsCmdOption timeslotPrint buildCtxPrint tsg binGen buildFile)
  | not timeslotPrint && not buildCtxPrint && null binGen = return opt
  | otherwise = 
    return 
      $ TsCmdOption timeslotPrint buildCtxPrint tsg binGen 
      $ strNotEmptyOr buildFile "timeslot.conf"
cmdlinePreProc opt = return opt

cmdlineProc :: TsCmdOption -> IO CmdlineProcResult
cmdlineProc opt@(TsCmdReadBin readFile ) = do
  ioCtx <- readTimeslotFromFile readFile
  case ioCtx of
    Just ctx -> print ctx 
    _ -> printf "read binary file (%s) error" readFile
  return $ CmdlineErr opt "read binary file"

cmdlineProc opt@(TsCmdOption timeslotPrint buildCtxPrint _ binGen buildFile)
  | not timeslotPrint && not buildCtxPrint && null binGen = return $ CmdlineErr opt "no input file"
  | otherwise = do
      timeslotc <- decodeFileStrict buildFile :: IO (Maybe TimeslotTCWrapper)
      putStrLn "---------------------- timeslot generation info --------------------"
      print timeslotc
      putStrLn "--------------------------------------------------------------------"
      case timeslotc of
        Just tsc -> 
          return $ CmdlineSuc opt ctx tst
          where (ctx, tst) = timeslotBuild $ timeslot tsc
        Nothing -> return $ CmdlineErr opt $ printf "decode file(%s) error" buildFile

timeslotBinGenProc :: CmdlineProcResult -> IO ()
timeslotBinGenProc err@(CmdlineErr opt erro) = return ()
timeslotBinGenProc suc@(CmdlineSuc opt ctx tst) = 
  case binGen opt of
    [] -> return ()
    outFile -> writeTsInFile ctx outFile

-- timeslotBinRead :: TsCmdOption -> IO CmdlineProcResult
-- timeslotBinRead opt@()

main :: IO ()
main = do
  -- print $ word8l2ToInt [1,0]
  -- ctx <- readTimeslotFromFile "timeslot.bin"
  -- bytestring <- BS.fromFilePath "timeslot.bin"
  -- print ctx
  -- print ctx
  cmdOpt <- cmdArgs cmds
  cmdOpt <- cmdlinePreProc cmdOpt
  cmdRes <- cmdlineProc cmdOpt
  cmdRes <- printProc cmdRes
  timeslotBinGenProc cmdRes
  putStrLn "Timeslot generator by YangFei."
{-
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
-}
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
