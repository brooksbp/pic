module PIC.LinkerScript.Parser.Tokens
  ( Token(..)
  , TokenName(..)
  , Position(..)
  , tokenString
  ) where

import Text.Printf

tokenString :: Token -> String
tokenString (Token _ s _) = s

data Position = Position String Int Int deriving Eq

instance Show Position where
  show (Position f l c) = printf "%s:%d:%d" f l c

data Token = Token TokenName String Position deriving (Show, Eq)

data TokenName
  = KW_libpath     -- ^ Command line information
  | KW_lkrpath
  | KW_files
  | KW_include
  | KW_databank    -- ^ RAM memory regions
  | KW_sharebank
  | KW_accessbank
  | KW_linearmem   -- ^ ROM memory regions
  | KW_codepage
  | KW_section     -- ^ Logical sections
  | KW_stack       -- ^ Stack
  | KW_name
  | KW_start
  | KW_end
  | KW_protected
  | KW_shadowed
  | KW_fill
  | KW_rom
  | KW_ram
  | KW_code
  | KW_size
  | Id_simple
  | Lit_number
  | Sym_eq
  | Sym_col
  | Unknown
  deriving (Show, Eq)
