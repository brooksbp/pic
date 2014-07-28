module PIC.LinkerScript.Parser
  ( parseFile
  , preprocess
  , uncomment
  ) where

import PIC.LinkerScript.AST
import PIC.LinkerScript.Parser.Lex
import PIC.LinkerScript.Parser.Parse
import PIC.LinkerScript.Parser.Preprocess
import PIC.LinkerScript.Parser.Tokens

-- | Parses a file given a table of predefined macros, the file name, and the
-- file contents.
parseFile :: [(String, String)] -> FilePath -> String -> Script
parseFile env file content = script tokens
  where
    tokens = map relocate $ alexScanTokens $ preprocess env file content
    relocate :: Token -> Token
    relocate (Token t s (Position _ l c)) = Token t s $ Position file l c
