module PIC.LinkerScript.AST
  ( Script(..)
  , Directive(..)
  , Block(..)
  , Address
  , Identifier
  , Protected
  , Shadow(..)
  , Fill
  ) where

import Text.Printf

data Script = Script [Directive] deriving Eq

instance Show Script where
  show (Script d) = concatMap show d

data Directive
  = Libpath Identifier
  | Lkrpath Identifier
  | Files Identifier
  | Include Identifier
  | Databank Identifier Address Address Protected (Maybe Shadow)
  | Sharebank Identifier Address Address Protected
  | Accessbank Identifier Address Address Protected
  | Linearmem Identifier Address Address
  | Codepage Identifier Address Address Protected (Maybe Fill)
  | Section Identifier Block
  | Stack Int (Maybe Block)
  deriving Eq

instance Show Directive where
  show (Libpath i) = "LIBPATH " ++ show i ++ "\n"
  show (Lkrpath i) = "LKRPATH " ++ show i ++ "\n"
  show (Files i) =   "FILES " ++ show i ++ "\n"
  show (Include i) = "INCLUDE " ++ show i ++ "\n"
  show (Databank name start end protected shadow) = unlines [
      "DATABANK"
    , " NAME=" ++ show name
    , " START=" ++ (printf "0x%x" start)
    , " END=" ++ (printf "0x%x" end)
    , if protected then " PROTECTED" else ""
    , maybe "" (\s -> " SHADOWED=" ++ show s) shadow
    , "\n"
    ]
  show (Sharebank name start end protected) = unlines [
      "SHAREBANK"
    , " NAME=" ++ show name
    , " START=" ++ (printf "0x%x" start)
    , " END=" ++ (printf "0x%x" end)
    , if protected then " PROTECTED" else ""
    , "\n"
    ]
  show (Accessbank name start end protected) = unlines [
      "ACCESSBANK"
    , " NAME=" ++ show name
    , " START=" ++ (printf "0x%x" start)
    , " END=" ++ (printf "0x%x" end)
    , if protected then " PROTECTED" else ""
    , "\n"
    ]
  show (Linearmem name start end) = unlines [
      "LINEARMEM"
    , " NAME=" ++ show name
    , " START=" ++ (printf "0x%x" start)
    , " END=" ++ (printf "0x%x" end)
    , "\n"
    ]
  show (Codepage name start end protected fill) = unlines [
      "CODEPAGE"
    , " NAME=" ++ show name
    , " START=" ++ (printf "0x%x" start)
    , " END=" ++ (printf "0x%x" end)
    , if protected then " PROTECTED" else ""
    , maybe "" (\f -> " FILL=" ++ (printf "0x%x" f)) fill
    , "\n"
    ]
  show (Section name block) = unlines [
      "SECTION"
    , " NAME=" ++ show name
    , show block
    , "\n"
    ]
  show (Stack size block) = unlines [
      "STACK"
    , " SIZE=" ++ (printf "0x%x" size)
    , maybe "" (\b -> show b) block
    , "\n"
    ]

data Block
  = Rom Identifier
  | Ram Identifier 
  deriving Eq

instance Show Block where
  show (Rom name) = "ROM=" ++ show name
  show (Ram name) = "RAM=" ++ show name

type Address = Int

type Identifier = String

type Protected = Bool

data Shadow = Shadow Identifier Address deriving Eq

instance Show Shadow where
  show (Shadow name addr) = show name ++ ":" ++ (printf "0x%x" addr)

type Fill = Int
