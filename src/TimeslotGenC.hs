{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}
module TimeslotGenC where
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, withObject, (.:))
import Data.Aeson.Types (parseJSON)
import Text.Printf (printf)

data TimeslotTxC = TimeslotTxC {
  kind :: Int,
  mem_size :: Int,
  timeslot_index :: [Int]
} deriving (Generic)

instance Show TimeslotTxC where
  show :: TimeslotTxC -> String
  show (TimeslotTxC kind mems tsi) = printf "kind: %02d mems : %02d timeslot index: [%s]" kind mems (Prelude.foldr (\x y -> y ++ " " ++ show x) "" tsi )

instance FromJSON TimeslotTxC where
  parseJSON = withObject "timeslotTx" $
    \x -> TimeslotTxC
      <$> x .: "kind"
      <*> x .: "mem_size"
      <*> x .: "timeslot_index"

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
