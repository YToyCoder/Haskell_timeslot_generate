{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
module TimeslotGenC where
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, withObject, (.:), (.:?), (.!=), withText, Object, Value (Object))
import Data.Aeson.Types (parseJSON)
import Text.Printf (printf)
import TimeslotGen (TrList, TrRole(TrRole))
import Data.Maybe (fromMaybe)

data TimeslotConfMode = 
    TimeslotGenByMemS -- generate by member size 
  | TimeslotGenByTR   -- generate by tr table
  | TimeslotGenByMemEx -- generate by member-size-extended-mode
  deriving(Show)

-- timeslot gen configuration 
data TimeslotTxC = TimeslotTxC {
  kind :: Int, -- timeslot kind
  mem_size :: Int, -- member size
  timeslot_index :: [Int], -- timeslot index
  tr_tab :: [TrList], -- tx-rx info
  timeslot_gen_mode :: TimeslotConfMode
} deriving (Generic)

instance Show TimeslotTxC where
  show :: TimeslotTxC -> String
  show (TimeslotTxC kind mems tsi _ mode) = printf "kind: %02d mems : %02d mode: %s timeslot index: [%s]" kind mems (show mode) (Prelude.foldr (\x y -> y ++ " " ++ show x) "" tsi )

instance FromJSON TimeslotTxC where
  parseJSON (Object v) = do
      kd <- v .: "kind"
      ms <- v .: "mem_size"
      timeslot_idxs <- v .: "timeslot_index"
      tr_table <- v .:? "tr_tab"
      it_Mode <- v .:? "mode"
      return $ TimeslotTxC kd ms timeslot_idxs ( fromMaybe [] tr_table ) $ _int_to_timeslot_gen_mode it_Mode
      where 
        _int_to_timeslot_gen_mode :: Maybe String -> TimeslotConfMode
        _int_to_timeslot_gen_mode i = 
          case i of
            Nothing -> TimeslotGenByMemS
            Just v -> 
              case v of
              "ByMS" -> TimeslotGenByTR
              "ByEx" -> TimeslotGenByMemEx
              otherwise  -> TimeslotGenByMemS 

instance FromJSON TrRole where
  parseJSON = withObject "Role" $ 
    \x -> TrRole 
      <$> x .: "t"
      <*> x .: "r"

data TimeslotTableGenC = TimeslotTableGenC {
  frame :: Int,
  slot :: Int,
  tr :: [TimeslotTxC]
  } deriving (Generic)

instance Show TimeslotTableGenC where
  show :: TimeslotTableGenC -> String
  show (TimeslotTableGenC frame slot tx) = 
    printf "frame cnt: %d, slot cnt: %d:\n%s" frame slot (foldr (\x y -> y ++ "\n" ++ show x) "" tx)

instance FromJSON TimeslotTableGenC where
  parseJSON = withObject "Timeslot" $
    \x -> TimeslotTableGenC
      <$> x .: "frame"
      <*> x .: "slot"
      <*> x .: "timeslot_tr"

data TimeslotTCWrapper = TimeslotTCWrapper {timeslot:: TimeslotTableGenC}

instance Show TimeslotTCWrapper where
  show :: TimeslotTCWrapper -> String
  show (TimeslotTCWrapper ts) = show ts

instance FromJSON TimeslotTCWrapper where
  parseJSON = withObject "timeslot" $
    \x -> TimeslotTCWrapper
      <$> x .: "Timeslot"
