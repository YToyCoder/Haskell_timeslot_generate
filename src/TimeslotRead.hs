module TimeslotRead where
import TimeslotGen (BinTimeslotCtx (BinTimeslotCtx), BinTimeslot (BinTimeslot), TrRole (TrRole))
import System.IO(withFile, hClose, IOMode(..), hFileSize, hGetContents, openBinaryFile)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import Data.Bits ((.|.), shiftL)
import qualified GHC.TypeError as Data
import Data.Text.Encoding (encodeUtf8)
import Data.Text (pack)

word8l2ToInt :: [Word8] -> Int
word8l2ToInt (e1 : e2 : l) = fromIntegral e1 .|. shiftL (fromIntegral e2) 8

readTimeslot :: [Word8] -> Maybe BinTimeslotCtx
readTimeslot (t : s : rest)
  | [0x74, 0x73] /= [t,s]= Nothing
  | otherwise =  unpackTimeslotCtx rest
  where
    unpackTimeslotCtx :: [Word8] -> Maybe BinTimeslotCtx
    unpackTimeslotCtx (frame_cnt : slot_cnt : l) =
      case unpackTimeslotCtx' l (frame_cnt * slot_cnt) [] of
        Nothing -> Nothing
        Just tss -> 
          Just $ BinTimeslotCtx (reverse tss) (fromIntegral frame_cnt) (fromIntegral slot_cnt)
    unpackTimeslotCtx _ = Nothing

    unpackTimeslotCtx' wd i tss
      | i == 0 = Just tss
      | otherwise = -- Nothing
        case unpackTimeslot wd of
          Nothing -> Nothing
          Just (rl, ts) -> unpackTimeslotCtx' rl (i - 1) $ ts : tss

    unpackTimeslot :: [Word8] -> Maybe ([Word8], BinTimeslot)
    unpackTimeslot (frame : slot : kind : l1 : l2 : l) =
      case unpackTr' l trLen [] of
        Nothing -> Nothing
        Just (rl, tr) ->
          Just (rl , 
                BinTimeslot (fromIntegral frame) (fromIntegral slot) (fromIntegral kind) trLen tr)
        where trLen = word8l2ToInt [l1, l2]
    unpackTimeslot _ = Nothing

    unpackTr' l size tr_
      | size == 0 = Just (l, tr_)
      | otherwise =
        case unpackTr l of
          Nothing -> Nothing
          Just (rl, e) -> unpackTr' rl (size - 1) $ e : tr_

    unpackTr :: [Word8] -> Maybe ([Word8],TrRole)
    unpackTr (t : r : l) = Just (l, TrRole (fromIntegral t) $ fromIntegral r)
    unpackTr _ = Nothing

readTimeslot _ = Nothing

readTimeslotFromFile :: FilePath -> IO (Maybe BinTimeslotCtx)
readTimeslotFromFile filePath = do
  handle <- openBinaryFile filePath ReadMode
  contents <- hGetContents handle
  return $ readTimeslot $ BS.unpack $ encodeUtf8 (pack contents)
