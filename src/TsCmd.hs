{-# LANGUAGE DeriveDataTypeable #-}
module TsCmd where

import System.Console.CmdArgs
    ( Data, Typeable, (&=), help,modes, name, opt, typFile, Default(def), program, details )

data TsCmdOption = 
  TsCmdOption {
    timeslotPrint :: Bool,
    buildCtxPrint :: Bool,
    tsgPrint :: Bool,
    binGen :: FilePath,
    buildFile :: FilePath
  }
  | TsCmdReadBin { readBin :: FilePath }
  | TsCmdGenConf { outConfPath :: FilePath }
  deriving (Show, Data, Typeable, Eq)

helpMsg :: String
helpMsg = "Timeslot Binary File Generator cmd tool By YangFei\n\
\use example: htsg.exe --gen. Default Generated file timeslot.bin \n\
\or use : htsg.exe --gen=Out to set Generated file. \n"

binFormatMsg :: String
binFormatMsg = 
  "----------------timeslot binary format----------------\n\
  \binary format: \n\
  \0~1: ts\n\
  \frame_cnt : 1 byte, slot_cnt : 1 byte \n\
  \[timeslot?frame_cnt*slot_cnt] \n\
  \         frame: 1 byte, slot: 1 byte, kind: 1 byte, len: 2 bytes\n\
  \         [tx?len] t: 1 byte, x: 1 byte\n\
  \------------------------------------------------------"

option :: TsCmdOption
option = TsCmdOption {
  timeslotPrint = def &= name "printTs" &= help "print timeslot",
  buildCtxPrint = def &= name "printCtx" &= help "print timeslot gen context",
  tsgPrint = def &= name "printSrc" &= help "print src ctx",
  binGen = def &= name "gen" &= help "generate timeslot table binary file" &= typFile &= opt "timeslot.bin",
  buildFile = def &= name "in" &= help "build timeslot configuration" &= typFile &= opt "timeslot.json"
} &= help helpMsg -- "Timeslot generation cmd tool "
  &= name "generate"
  &= details ["Haskell timeslot generator by YangFei", "", binFormatMsg,"To generate timeslot binary data, use:", "htsg.exe G --in=FILE --gen=OUT"]

readBinary :: TsCmdOption
readBinary = TsCmdReadBin { 
  readBin = def &= name "bin" &= help "read timeslot binary file" &= typFile &= opt "timeslot.bin"
} &= name "read" &= details ["Timeslot Binary File Read"]

genConfigTemplate = TsCmdGenConf {
  outConfPath = def &= name "out" &= typFile &= opt "template.json"
} &= name "configGenerate" &= details ["Timeslot Binary File's configure file generate", "", "htsg.exe configGenerate [--out=FILE]", "default out file template.json"]
cmds :: TsCmdOption
cmds = 
  modes [option, readBinary, genConfigTemplate] &= 
  program "htsg(Haskell version timeslot generator): v0.0.1"