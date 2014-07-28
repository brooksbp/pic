{
module PIC.LinkerScript.Parser.Parse (script) where

import PIC.LinkerScript.AST
import PIC.LinkerScript.Parser.Tokens
}

%name script
%tokentype { Token }
%error { parseError }

%expect 0

%token

"LIBPATH"         { Token KW_libpath    _ _ }
"LKRPATH"         { Token KW_lkrpath    _ _ }
"FILES"           { Token KW_files      _ _ }
"INCLUDE"         { Token KW_include    _ _ }
"DATABANK"        { Token KW_databank   _ _ }
"SHAREBANK"       { Token KW_sharebank  _ _ }
"ACCESSBANK"      { Token KW_accessbank _ _ }
"LINEARMEM"       { Token KW_linearmem  _ _ }
"CODEPAGE"        { Token KW_codepage   _ _ }
"SECTION"         { Token KW_section    _ _ }
"STACK"           { Token KW_stack      _ _ }
"NAME"            { Token KW_name       _ _ }
"START"           { Token KW_start      _ _ }
"END"             { Token KW_end        _ _ }
"PROTECTED"       { Token KW_protected  _ _ }
"SHADOWED"        { Token KW_shadowed   _ _ }
"FILL"            { Token KW_fill       _ _ }
"ROM"             { Token KW_rom        _ _ }
"RAM"             { Token KW_ram        _ _ }
"CODE"            { Token KW_code       _ _ }
"SIZE"            { Token KW_size       _ _ }

identifier        { Token Id_simple     _ _ }
number            { Token Lit_number    _ _ }

"="               { Token Sym_eq        _ _ }
":"               { Token Sym_col       _ _ }

%right "="

%%

Script :: { Script }
: Directives { Script $1 }

Directives :: { [Directive] }
:                      { [] }
| Directives Directive { $1 ++ [$2] }

Directive :: { Directive }
: "LIBPATH" Identifier { Libpath $2 }
| "LKRPATH" Identifier { Lkrpath $2 }
| "FILES" Identifier { Files $2 }
| "INCLUDE" Identifier { Include $2 }
| "DATABANK" "NAME" "=" Identifier "START" "=" Address "END" "=" Address Protected MaybeShadow { Databank $4 $7 $10 $11 $12 }
| "SHAREBANK" "NAME" "=" Identifier "START" "=" Address "END" "=" Address Protected { Sharebank $4 $7 $10 $11 }
| "ACCESSBANK" "NAME" "=" Identifier "START" "=" Address "END" "=" Address Protected { Accessbank $4 $7 $10 $11 }
| "LINEARMEM" "NAME" "=" Identifier "START" "=" Address "END" "=" Address "PROTECTED" { Linearmem $4 $7 $10 }
| "CODEPAGE" "NAME" "=" Identifier "START" "=" Address "END" "=" Address Protected MaybeFill { Codepage $4 $7 $10 $11 $12 }
| "SECTION" "NAME" "=" Identifier Block { Section $4 $5 }
| "STACK" "SIZE" "=" number MaybeRam { Stack (toNumber $4) $5 }

Identifier :: { Identifier }
: identifier { tokenString $1 }

Address :: { Address }
: number { toNumber $1 }

Protected :: { Bool }
:             { False }
| "PROTECTED" { True }

MaybeShadow :: { Maybe Shadow }
:        { Nothing }
| Shadow { Just $1 }

Shadow :: { Shadow }
: "SHADOWED" "=" Identifier ":" Address { Shadow $3 $5 }

MaybeFill :: { Maybe Fill }
:      { Nothing }
| Fill { Just $1 }

Fill :: { Fill }
: "FILL" "=" number { toNumber $3 }

Block :: { Block }
: "ROM" "=" Identifier { Rom $3 }
| "RAM" "=" Identifier { Ram $3 }

MaybeRam :: { Maybe Block }
:                      { Nothing }
| "RAM" "=" Identifier { Just (Ram $3) }

{
parseError :: [Token] -> a
parseError a = case a of
  []              -> error "Parse error: no tokens left to parse."
  Token _ s p : _ -> error $ "Parse error: unexpected token '" ++ s ++ "' at " ++ show p ++ "."

toNumber :: Token -> Int
toNumber = number . tokenString
  where
    number a = fromInteger $ read a
}
