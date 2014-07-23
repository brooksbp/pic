module PIC.LinkerScript.AST
  ( Script(..)
  , Directive(..)
  , Block(..)
  , Address
  , Identifier
  , Protected
  , Shadow
  , Fill
  ) where

data Script = Script [Directive] deriving Eq

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

data Block
  = Rom Identifier
  | Ram Identifier 
  deriving Eq

type Address = Int

type Identifier = String

type Protected = Bool

type Shadow = (Identifier, Address)

type Fill = Int
