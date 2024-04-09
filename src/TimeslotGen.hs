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
import Data.Map as Map ( empty, foldrWithKey, null, Map, insert, lookup )
import qualified GHC.Arr as Data
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.Word ( Word8 )
import Data.ByteString.Builder ( word8, writeFile, word16Dec, byteString )
import Prelude hiding (writeFile)
import Data.ByteString.Char8 (pack, length)

class SerialData a where
  serialU8 :: a -> [Word8]

-- t : transmit
-- r : recv
data TrRole = TrRole { t:: Int, r:: Int}

intToWord8 :: Int -> Word8
intToWord8 i =
  if i > 0 && i <= 255 then fromIntegral i
  else 0

intTo2Word8 :: Int -> [Word8]
intTo2Word8 i = [intToWord8 $ mod i 256, intToWord8 $ div i 256]

instance SerialData TrRole where
  serialU8 :: TrRole -> [Word8]
  serialU8 (TrRole t x) = [intToWord8 t, intToWord8 x]

instance Show TrRole where
  show :: TrRole -> String
  show (TrRole trole xrole)= printf "<t:%02d-r:%02d>" trole xrole

trRoleReverse :: TrRole -> TrRole
trRoleReverse (TrRole t r) = TrRole r t

type NetScaleSrcDst = [(Int, TrList)]
type TrList = [TrRole]

trListReverse :: TrList -> TrList
trListReverse = map trRoleReverse

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

twoElemsNetScaleTx :: [(Int, [TrRole])]
twoElemsNetScaleTx = [(0, [TrRole 0 1])]

getGenScale :: Int -> Int
getGenScale netScale = netScale + mod netScale 2

gen' :: Int -> Int -> TrList
gen' kindCnt genScale =
  TrRole start target : get_res 1 (target, target)
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

    get_res :: Int -> (Int, Int) -> TrList
    get_res cnt (s, t)
      | genListSize - 1 == cnt = [TrRole (mf s)  (mb t)]
      | otherwise = TrRole (mf s)  (mb t) : get_res (cnt + 1) (mf s, mb t)

data TimeslotT = TimeslotT { epoch:: Int, frame :: Int, slot :: Int }

instance Show TimeslotT where
  show :: TimeslotT -> String
  show (TimeslotT e f s) = printf "%02d-%02d" f s

data TsInfo = TsInfo { _t :: Int, tx_info :: TrList }

instance Show TsInfo where
  show :: TsInfo -> String
  show (TsInfo t tx) =  show tx

data TrListSize = TrListSize Int TrList
type TrInfoT = Map Int TrListSize
type TrArrInfoT = Array Int TrList

txInfoToString :: TrInfoT -> String
txInfoToString tr
  | Map.null tr = "[]"
  | otherwise =
    Map.foldrWithKey foldFn "" tr
    where
      foldFn k (TrListSize l a) = printf "%08s: %s\n%s" (intToBinaryString k) (show a)

data TimeslotInfo =
  TimeslotInfo { timeslotKind :: Int, timeslotInfoRef:: Int } |
  TimeslotInfoRv { timeslotKind :: Int, timeslotInfoRef:: Int } |
  TimeslotInfoIv

tsInfo2Key :: TimeslotInfo -> Int
tsInfo2Key TimeslotInfoIv = -1
tsInfo2Key tsi = genTrIndex (timeslotKind tsi) $ timeslotInfoRef tsi

instance Show TimeslotInfo where
  show :: TimeslotInfo -> String
  show (TimeslotInfo kind ref)   = printf "+%02d-%02d" kind ref
  show (TimeslotInfoRv kind ref) = printf "-%02d-%02d" kind ref
  show TimeslotInfoIv = "?00-00"

data TimeslotTable = TimeslotTable {
  slot_cnt :: Int,
  frame_size:: Int,
  table :: Array Int TimeslotT,
  infoTable :: Array Int TimeslotInfo ,
  rtInfo :: TrInfoT
}

instance Show TimeslotTable where
  show :: TimeslotTable -> String
  show (TimeslotTable slot_cnt frame_size table info tr) =
    printf ">>========= timeslot table\
            \%s\n\n\
            \>>========= tr index table%s \n\n\
            \>>========= tr table\n%s\n"
      (toString table table_size table_size)
      (toString info table_size table_size)
      (txInfoToString tr)
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
    tx :: TrArrInfoT}

instance Show TimeslotGenInfo where
  show :: TimeslotGenInfo -> String
  show (TimeslotGenInfo kind indexs tx_size tx ) =
    printf "size: %4d\n%s\n%s"
      tx_size
      (Prelude.foldr (printf "%02d %s") "" indexs)
      (arrToString tx tx_size 1 "")

createTimeslotGen :: [Int] -> Int -> Int -> TimeslotGenInfo
createTimeslotGen timeslot_indexs kind 0 = 
  TimeslotGenInfo kind timeslot_indexs 0 $ array (0,1) [] -- $ array (0, genScale) $ gen netScale
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

genTrIndex :: Int -> Int -> Int
genTrIndex timeslot_kind timeslot_tr_index =
  timeslot_kind * (2 ^ 5) + timeslot_tr_index

performTimeslotGenProc :: TimeslotGenInfo -> TimeslotTable -> TimeslotTable
performTimeslotGenProc (TimeslotGenInfo ts_kind timeslot_indexs tr_size tr) ts_table =
  action timeslot_indexs ts_table 0
  where
    action [] ts _ = ts
    action
      tsIndexs
      (TimeslotTable slot_cnt frame_cnt table info trInfo) tr_index
      =
      action rest (TimeslotTable
                    slot_cnt
                    frame_cnt
                    table -- timeslot table
                    (unsafeReplace info replaceTss) -- timeslot info
                    (addTr tr_index trInfo) ) -- timeslot tr
        $ nextTrIndex tr_index
          where
           (rest, replaceTss ) = case tsIndexs of
              ( tse : tse2 : rest ) ->
                (rest,
                [ (tse, TimeslotInfo ts_kind tr_index),
                  (tse2, TimeslotInfoRv ts_kind tr_index)])
              ( tse : rest )  ->
                (rest,
                [ (tse, TimeslotInfo ts_kind tr_index) ])

    addTr :: Int -> TrInfoT -> TrInfoT
    addTr tri trInfo 
      | tr_size == 0 = trInfo
      | otherwise = 
        let txl = TrListSize (div (tr_size + 1) 2) (arrGet tr tri) 
        in Map.insert (genTrIndex ts_kind tri) txl trInfo
    nextTrIndex :: Int -> Int 
    nextTrIndex i 
      | tr_size == 0 = 0
      | otherwise = mod (i + 1) tr_size

data BinTimeslot =
  BinTimeslot Int {- frame -} Int {- slot -} Int {- kind -} Int {- len -} TrList

instance Show BinTimeslot where
  show :: BinTimeslot -> String
  show (BinTimeslot frame slot kind len tr) =
    printf "[%02d-%02d]: %d -> %02d %s\n" frame slot kind len
      $ Prelude.foldr (printf "%s %s" . show) "" tr

instance SerialData BinTimeslot where
  serialU8 :: BinTimeslot -> [Word8]
  serialU8 (BinTimeslot frame slot kind len tr)=
    [intToWord8 frame, intToWord8 slot, intToWord8 kind] ++
    intTo2Word8 len ++
    Prelude.foldr (\x y  -> serialU8 x ++ y ) [] tr

data BinTimeslotCtx = BinTimeslotCtx { bTimeslots :: [BinTimeslot], fcnt :: Int, scnt :: Int }

instance Show BinTimeslotCtx where
  show :: BinTimeslotCtx -> String
  show ctx@(BinTimeslotCtx tss frame_cnt slot_cnt) =
    printf "---------------------- timeslot binary ------------------------\n\
    \[ frame:%02d slot:%02d ]\n%s" frame_cnt slot_cnt
      $ Prelude.foldr (printf "%s%s" . show) "" tss

instance SerialData BinTimeslotCtx where
  serialU8 :: BinTimeslotCtx -> [Word8]
  serialU8 (BinTimeslotCtx tss frame_cnt slot_cnt) =
    [intToWord8 frame_cnt, intToWord8 slot_cnt] ++
    Prelude.foldr (\x y -> serialU8 x ++ y) [] tss

buildCtx :: TimeslotTable -> BinTimeslotCtx
buildCtx (TimeslotTable slot_cnt frame table info tr) =
  proc' (slot_cnt * frame) $ BinTimeslotCtx [] frame slot_cnt
  where
    proc' :: Int -> BinTimeslotCtx -> BinTimeslotCtx
    proc' i bctx@(BinTimeslotCtx timeslots frame_cnt slot_cnt)
      | i == 0 = bctx
      | otherwise =
        proc' (i - 1) $
          BinTimeslotCtx (putTsi i timeslots) frame_cnt slot_cnt
    putTsi :: Int -> [BinTimeslot] -> [BinTimeslot]
    putTsi i tss =
      let TimeslotT _ frame slot = arrGet table $ i - 1 in
      case arrGet info $ i - 1 of
        TimeslotInfoIv -> BinTimeslot frame slot 0 0 [] : tss
        infoe -> case Map.lookup (tsInfo2Key infoe) tr of
          Just (TrListSize l a)-> BinTimeslot frame slot (timeslotKind infoe) l trl : tss
            where trl = case infoe of
                    TimeslotInfoRv _ _ -> trListReverse a
                    _ -> a
          Nothing -> BinTimeslot frame slot (timeslotKind infoe) 0 [] : tss

magicNum = byteString $ pack "ts"

writeTsInFile :: BinTimeslotCtx -> FilePath -> IO ()
writeTsInFile ctx@(BinTimeslotCtx tss frame_cnt slot_cnt) filePath =
  writeFile filePath
    $ mappend magicNum
    $ Prelude.foldr (mappend . word8 ) mempty
    $ serialU8 ctx
