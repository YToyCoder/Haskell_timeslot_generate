{-# LANGUAGE DeriveGeneric #-} 
{-# LANGUAGE OverloadedStrings #-} 
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}
module TimeslotGenC where
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, withObject, (.:))
import Data.Aeson.Types (parseJSON)

data TimeslotTxC = TimeslotTxC {
  kind :: Int,
  mem_size :: Int,
  timeslot_index :: [Int]
} deriving (Generic, Show)

instance FromJSON TimeslotTxC where
  parseJSON = withObject "timeslotTx" $
    \x -> TimeslotTxC
      <$> x .: "kind"
      <*> x .: "mem_size"
      <*> x .: "timeslot_index"

data TimeslotTableGenC = TimeslotTableGenC { 
  frame :: Int, 
  slot :: Int,
  tx :: [TimeslotTxC]
  } deriving (Generic, Show)

instance FromJSON TimeslotTableGenC where
  parseJSON = withObject "Timeslot" $ 
    \x -> TimeslotTableGenC 
      <$> x .: "frame"
      <*> x .: "slot"
      <*> x .: "timeslot_tx"

data TimeslotTCWrapper = TimeslotTCWrapper {timeslot:: TimeslotTableGenC}

instance Show TimeslotTCWrapper where
  show :: TimeslotTCWrapper -> String
  show (TimeslotTCWrapper ts) = show ts

instance FromJSON TimeslotTCWrapper where
  parseJSON = withObject "timeslot" $
    \x -> TimeslotTCWrapper
      <$> x .: "Timeslot"
