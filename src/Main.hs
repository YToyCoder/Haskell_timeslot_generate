
module Main where

import Text.Printf (printf)
import Data.List (insertBy, sortBy)
import Data.Ord (comparing)
import Control.Monad (filterM, replicateM)
import GHC.Base (liftA2)
import TimeslotGen(
  gen, TimeslotScaleInfo (TimeslotScaleInfo),
  genTsTable, createTimeslotGen, buildCtx,
  SerialData (serialU8), writeTsInFile, intTo2Word8, intToWord8,
  magicNum, BinTimeslotCtx, TimeslotTable, TimeslotGenInfo (TimeslotGenInfo),
  createTimeslotGen_, gen_ex)
import Data.Foldable (Foldable(fold))
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.ByteString.Builder (word16Dec, writeFile)
import Prelude hiding (writeFile)
import TimeslotGenC (
  TimeslotTableGenC(TimeslotTableGenC),
  TimeslotTxC (TimeslotTxC, timeslot_index, kind, mem_size),
  TimeslotTCWrapper (TimeslotTCWrapper, timeslot),
  TimeslotConfMode (TimeslotGenByMemS, TimeslotGenByTR, TimeslotGenByMemEx))
import Data.Aeson (decode, decodeFileStrict)
import TsCmd(TsCmdOption (TsCmdOption, timeslotPrint, buildCtxPrint, binGen, TsCmdReadBin, TsCmdGenConf), option, cmds)
import System.Console.CmdArgs ( cmdArgs )
import TimeslotRead (readTimeslotFromFile, word8l2ToInt)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Prelude

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
    tsgl = map _map_to_timeslot_gen tx -- create timeslot generate info list
    tst = genTsTable tsgl $ TimeslotScaleInfo slot frame -- create timeslot table
    _map_to_timeslot_gen :: TimeslotTxC -> TimeslotGenInfo -- map json file obj to timelsot gen info
    _map_to_timeslot_gen (TimeslotTxC kd mem_size timeslot_idxs tr_table mode) = 
      case mode of
        TimeslotGenByMemS -> 
          createTimeslotGen timeslot_idxs kd mem_size
        TimeslotGenByTR -> 
          createTimeslotGen_ timeslot_idxs kd tr_table
        TimeslotGenByMemEx -> 
          createTimeslotGen_ timeslot_idxs kd $ gen_ex mem_size

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
      $ strNotEmptyOr buildFile "timeslot.json"
cmdlinePreProc opt = return opt

templateJson = 
  "{ \n\
  \\"Timeslot\": \n {\n\
  \   \"frame\": 10,\n\
  \   \"slot\": 20,\n\
  \   \"timeslot_tr\": [\n\
  \     { \"kind\":1,\n\
  \       \"mem_size\": 4,\n\
  \       \"timeslot_index\":[18,19,38,39,58,59,78,79,98,99,118,119]\n\
  \     },\n\
  \     { \"kind\":2, \"timeslot_index\": [0,1,2,3]},\n\
  \     { \"kind\":3, \"mode\": \"ByTR\", \"timeslot_index\": [8,9,10,11,12], \n\
  \       \"tr_tab\": [{\"t\":0,\"r\":1}, {\"t\":2, \"r\":3}]},\n\
  \     { \"kind\":4, \"mem_size\":4, \"mode\": \"ByMS\", \"timeslot_index\": [4,5,6,7]}\n\
  \   ]\n }\n}"

-- proc cmd line option
cmdlineProc :: TsCmdOption -> IO CmdlineProcResult
-- cmd : generate configuration template 
cmdlineProc opt@(TsCmdGenConf out) = do
  Prelude.writeFile (strNotEmptyOr out "template.json") templateJson
  return $ CmdlineErr opt "read binary file"

-- cmd : read binary timeslot file
cmdlineProc opt@(TsCmdReadBin readFile ) = do
  ioCtx <- readTimeslotFromFile readFile
  case ioCtx of
    Just ctx -> print ctx 
    _ -> printf "read binary file (%s) error" readFile
  return $ CmdlineErr opt "read binary file"

-- cmd : build timeslot binary file 
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

-- write timeslot to binary file
timeslotBinGenProc :: CmdlineProcResult -> IO ()
timeslotBinGenProc err@(CmdlineErr opt erro) = return ()
timeslotBinGenProc suc@(CmdlineSuc opt ctx tst) = 
  case binGen opt of
    [] -> return ()
    outFile -> writeTsInFile ctx outFile

main :: IO ()
main = do
  cmdOpt <- cmdArgs cmds -- build cmd options
  cmdOpt <- cmdlinePreProc cmdOpt
  cmdRes <- cmdlineProc cmdOpt
  cmdRes <- printProc cmdRes
  timeslotBinGenProc cmdRes
  putStrLn "Timeslot generator by YangFei."

