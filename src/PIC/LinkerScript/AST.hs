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

showProtected :: Bool -> String
showProtected p = if p then " PROTECTED" else ""

showShadowed :: Maybe Shadow -> String
showShadowed = maybe "" (\s -> " SHADOWED=" ++ show s)

showBlock :: Maybe Block -> String
showBlock = maybe "" show

showFill :: Maybe Fill -> String
showFill = maybe "" (\f -> " FILL=" ++ printf "0x%x" f)

instance Show Directive where
  show (Libpath i) = printf "LIBPATH %s\n" i
  show (Lkrpath i) = printf "LKRPATH %s\n" i
  show (Files i) =   printf "FILES %s\n" i
  show (Include i) = printf "INCLUDE %s\n" i
  show (Databank name start end protected shadow) =
    printf "DATABANK NAME=%s START=0x%x END=0x%x%s%s\n" name start end (showProtected protected) (showShadowed shadow)
  show (Sharebank name start end protected) =
    printf "SHAREBANK NAME=%s START=0x%x END=0x%x%s\n" name start end (showProtected protected)
  show (Accessbank name start end protected) =
    printf "ACCESSBANK NAME=%s START=0x%x END=0x%x%s\n" name start end (showProtected protected)
  show (Linearmem name start end) =
    printf "LINEARMEM NAME=%s START=0x%x END=0x%x\n" name start end
  show (Codepage name start end protected fill) =
    printf "CODEPAGE NAME=%s START=0x%x END=0x%x%s%s\n" name start end (showProtected protected) (showFill fill)
  show (Section name block) =
    printf "SECTION NAME=%s %s\n" name (show block)
  show (Stack size block) =
    printf "STACK SIZE=0x%x%s\n" size (showBlock block)

data Block
  = Rom Identifier
  | Ram Identifier 
  deriving Eq

instance Show Block where
  show (Rom name) = printf "ROM=%s" name
  show (Ram name) = printf "RAM=%s" name

type Address = Int

type Identifier = String

type Protected = Bool

data Shadow = Shadow Identifier Address deriving Eq

instance Show Shadow where
  show (Shadow name addr) = printf "%s:0x%x" name addr

type Fill = Int
