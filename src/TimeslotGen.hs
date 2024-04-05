{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BlockArguments #-}
module TimeslotGen where
import Data.Foldable (Foldable(fold))
import Text.Printf(printf)
import Data.Map.Strict(Map(..), insert, fromList)
import Data.Array ( Array, (!), array ) 
import Data.Array.Base (IArray(unsafeReplace, unsafeAt), mapArray)
import Data.Map as Map ( empty, foldrWithKey, null, Map, insert )
import qualified GHC.Arr as Data
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

-- t : transmit
-- x : recv
data TxRole = TxRole { t:: Int, x:: Int}

instance Show TxRole where
  show :: TxRole -> String
  show (TxRole trole xrole)= printf "<t%02d-x%02d>" trole xrole

type TxList = [TxRole]

type NetScaleSrcDst = [(Int, TxList)]
intToBinaryString :: Int -> String
intToBinaryString n = showIntAtBase 2 intToDigit n ""

gen :: Int -> NetScaleSrcDst
gen netScale =
  case genScale of 
    0 -> []
    1 -> twoElemsNetScaleTx
    2 -> twoElemsNetScaleTx
    _ -> 
      [ (x - 1, gen' x netScale) | x <- [1 .. netScale - 1] ]
  where 
    genScale = getGenScale netScale

twoElemsNetScaleTx :: [(Int, [TxRole])]
twoElemsNetScaleTx = [(0, [TxRole 0 1])]

getGenScale :: Int -> Int
getGenScale netScale = netScale + mod netScale 2

gen' :: Int -> Int -> TxList
gen' kindCnt genScale = 
  TxRole start target : get_res 1 (target, target)
  where
    start  = 0
    target = kindCnt
    genListSize = div genScale 2

    mv i f
      | 0 == next = mv next f
      | target == next = mv next f
      | otherwise = next
      where next = f i

    mf i = mv i (\x -> mod (x + 1) genScale)
    mb i =
      mv i $ \x ->
        case x of
            0 -> genScale - 1
            _ -> x - 1

    get_res :: Int -> (Int, Int) -> TxList
    get_res cnt (s, t)
      | genListSize - 1 == cnt = [TxRole (mf s)  (mb t)]
      | otherwise = TxRole (mf s)  (mb t) : get_res (cnt + 1) (mf s, mb t)

data TimeslotT = TimeslotT { epoch:: Int, frame :: Int, slot :: Int }

instance Show TimeslotT where
  show :: TimeslotT -> String
  show (TimeslotT e f s) = printf "%04d-%04d-%04d" e f s

data TsInfo = TsInfo { _t :: Int, tx_info :: TxList }

instance Show TsInfo where
  show :: TsInfo -> String
  show (TsInfo t tx) =  show tx 

type TxInfoT = Map Int TxList
type TxArrInfoT = Array Int TxList

txInfoToString :: TxInfoT -> String
txInfoToString tx
  | Map.null tx = "[]"
  | otherwise = 
    Map.foldrWithKey (\k a b -> printf "%08s: %s\n%s" (intToBinaryString k) (show a) b) "" tx

data TimeslotInfo = 
  TimeslotInfo { timeslotKind :: Int, timeslotInfoRef:: Int } | 
  TimeslotInfoRv { timeslotKind :: Int, timeslotInfoRef:: Int } |
  TimeslotInfoIv

instance Show TimeslotInfo where
  show :: TimeslotInfo -> String
  show (TimeslotInfo kind ref)   = printf "+%02d-%04d" kind ref
  show (TimeslotInfoRv kind ref) = printf "-%02d-%04d" kind ref
  show TimeslotInfoIv = "?00-0000"

data TimeslotTable = TimeslotTable {
  slot_cnt :: Int,
  frame_size:: Int,
  table :: Array Int TimeslotT,
  infoTable :: Array Int TimeslotInfo ,
  txInfo :: TxInfoT 
} 

instance Show TimeslotTable where
  show :: TimeslotTable -> String
  show (TimeslotTable slot_cnt frame_size table info tx) =
    printf "%s\n%s \n\n%s"
      (toString table table_size table_size)
      (toString info table_size table_size)
      (txInfoToString tx)
    where
      table_size = slot_cnt * frame_size
      toString ::(Show a) => Array Int a -> Int  -> Int -> String
      toString arr i m = arrToString arr i slot_cnt ""

arrToString :: (Show a) => Array Int a -> Int -> Int -> String -> String
arrToString arr i line_cnt ctx
  | i == 0 = ctx
  | mod (i - 1) line_cnt == 0 =
    arrToString arr (i - 1) line_cnt $ printf "\n%s %s"  (show $ arrGet arr  (i - 1)) ctx
  | otherwise =
    arrToString arr (i - 1) line_cnt $ printf  "%s %s"  (show $ arrGet arr  (i - 1)) ctx

arrGet :: Show a => Array Int a -> Int -> a
arrGet = (Data.Array.!)

data TimeslotGenInfo = 
  TimeslotGenInfo {
    timeslot_kind :: Int,
    timeslot_indexs :: [Int],
    tx_size :: Int,
    tx :: TxArrInfoT} 

instance Show TimeslotGenInfo where
  show :: TimeslotGenInfo -> String
  show (TimeslotGenInfo kind indexs tx_size tx ) =
    printf "size: %4d\n%s\n%s" 
      tx_size
      (Prelude.foldr (printf "%02d %s") "" indexs) 
      (arrToString tx tx_size 1 "")

createTimeslotGen :: [Int] -> Int -> Int -> TimeslotGenInfo
createTimeslotGen timeslot_indexs kind netScale =
  TimeslotGenInfo kind timeslot_indexs (genScale - 1) $ array (0, genScale) $ gen netScale
  where genScale = getGenScale netScale

listToIndexList :: [a] -> [(Int, a)]
listToIndexList l = proc' l [] 0
  where
    proc' [] b _ = b
    proc' (e : rest) b i =  proc' rest ((i, e) : b) $ i + 1

data TimeslotScaleInfo = TimeslotScaleInfo { 
  slot_cnt_in_frame :: Int, 
  frame_cnt :: Int 
}

genTsTable :: [TimeslotGenInfo] -> TimeslotScaleInfo -> TimeslotTable
genTsTable time (TimeslotScaleInfo slot_cnt frame_cnt) =
  gen_loop time $ TimeslotTable slot_cnt frame_cnt timeslot_table info_table empty
  where
    timeslot_table = array (0, slot_cnt * frame_cnt )
     [ ( frame * slot_cnt + slot , TimeslotT 0 frame slot) |
          frame <- [0 .. frame_cnt - 1],
          slot <- [0 .. slot_cnt - 1] ]

    info_table = array (0, slot_cnt * frame_cnt)
     [ ( frame * slot_cnt + slot , TimeslotInfoIv ) |
          frame <- [0 .. frame_cnt - 1],
          slot <- [0 .. slot_cnt - 1] ]

    gen_loop :: [TimeslotGenInfo] -> TimeslotTable -> TimeslotTable
    gen_loop [] ts_table = ts_table
    gen_loop (info : rest) ts_table =
      gen_loop rest $ performTimeslotGenProc info ts_table

genTxIndex :: Int -> Int -> Int
genTxIndex timeslot_kind timeslot_tx_index =
  timeslot_kind * (2 ^ 5) + timeslot_tx_index

performTimeslotGenProc :: TimeslotGenInfo -> TimeslotTable -> TimeslotTable
performTimeslotGenProc (TimeslotGenInfo ts_kind timeslot_indexs tx_size tx) ts_table =
  action timeslot_indexs ts_table 0
  where
    action [] ts _ = ts
    action 
      tsIndexs 
      (TimeslotTable slot_cnt frame_cnt table info txInfo) tx_index 
      =
      action 
        rest
        (TimeslotTable
            slot_cnt
            frame_cnt
            table

            (unsafeReplace 
              info 
              replaceTss)

            (Map.insert (genTxIndex ts_kind tx_index)  (arrGet tx tx_index) txInfo))
        $ mod (tx_index + 1) tx_size
          where 
           (rest, replaceTss ) = case tsIndexs of 
              ( tse : tse2 : rest ) -> 
                (rest, 
                [ (tse, TimeslotInfo ts_kind tx_index), 
                  (tse2, TimeslotInfoRv ts_kind tx_index)])
              ( tse : rest )  -> 
                (rest, 
                [ (tse, TimeslotInfo ts_kind tx_index) ]) 

