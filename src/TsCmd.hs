{-# LANGUAGE DeriveDataTypeable #-}
module TsCmd where

import System.Console.CmdArgs
    ( Data, Typeable, (&=), help,modes, name, opt, typFile, Default(def), program, details )

data TsCmdOption = 
  TsCmdOption {
    timeslotPrint :: Bool,
    buildCtxPrint :: Bool,
    binGen :: FilePath,
    buildFile :: FilePath
  }
  deriving (Show, Data, Typeable, Eq)

helpMsg = "Timeslot Binary File Generator cmd tool By YangFei\n\
\ use example: htsg.exe --gen. Default Generated file timeslot.bin \n\
\ or use : htsg.exe --gen=Out to set Generated file. \n"

option :: TsCmdOption
option = TsCmdOption {
  timeslotPrint = def &= name "printTs" &= help "print timeslot",
  buildCtxPrint = def &= name "printCtx" &= help "print timeslot gen context",
  binGen = def &= name "gen" &= help helpMsg &= typFile &= opt "timeslot.bin",
  buildFile = def &= name "in" &= help "build timeslot configuration" &= typFile &= opt "ts.def"
} &= help "Timeslot generation cmd tool "
  &= details ["Haskell timeslot generator by YaiFei", "","To generate timeslot binary data, use:", "htsg.exe --in=FILE --gen=OUT"]

cmds = 
  modes [option] &= 
  program "htsg(Haskell version timeslot generator): v0.0.1"