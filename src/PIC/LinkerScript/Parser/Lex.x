{
{-# OPTIONS_GHC -w #-}
module PIC.LinkerScript.Parser.Lex
  ( alexScanTokens
  ) where

import PIC.LinkerScript.Parser.Tokens

}

%wrapper "posn"

$digit = 0-9
@hexDigit = [0-9a-fA-F]

@decimalNumber = $digit+

@hexNumber = 0[xX] @hexDigit+

@number = @decimalNumber | @hexNumber

@identifier = [a-zA-Z_\.] [a-zA-Z0-9_\.]*

tokens :-

  "LIBPATH"         { tok KW_libpath }
  "LKRPATH"         { tok KW_lkrpath }
  "FILES"           { tok KW_files }
  "INCLUDE"         { tok KW_include }
  "DATABANK"        { tok KW_databank }
  "SHAREBANK"       { tok KW_sharebank }
  "ACCESSBANK"      { tok KW_accessbank }
  "LINEARMEM"       { tok KW_linearmem }
  "CODEPAGE"        { tok KW_codepage }
  "SECTION"         { tok KW_section }
  "STACK"           { tok KW_stack }
  "NAME"            { tok KW_name }
  "START"           { tok KW_start }
  "END"             { tok KW_end }
  "PROTECTED"       { tok KW_protected }
  "SHADOWED"        { tok KW_shadowed }
  "FILL"            { tok KW_fill }
  "ROM"             { tok KW_rom }
  "RAM"             { tok KW_ram }
  "CODE"            { tok KW_code }
  "SIZE"            { tok KW_size }

  @identifier       { tok Id_simple }
  @number           { tok Lit_number }

  "="               { tok Sym_eq }
  ":"               { tok Sym_col }

  $white+ ;
  .                 { tok Unknown }

{
tok :: TokenName -> AlexPosn -> String -> Token
tok t (AlexPn _ l c) s = Token t s $ Position "" l c
}
