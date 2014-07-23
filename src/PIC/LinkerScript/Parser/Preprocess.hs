module PIC.LinkerScript.Parser.Preprocess
  ( uncomment
  , preprocess
  ) where

-- | Remove "//"-style comments from code. Is not string-literal-aware.
uncomment :: FilePath -> String -> String
uncomment file a = uncomment a
  where
    uncomment a = case a of
      ""             -> ""
      '/' : '/' : xs -> "  " ++ toWhiteUntilEOL xs
      x         : xs -> x : uncomment xs

    toWhiteUntilEOL a = case a of
      ""              -> ""
      '\n' : xs -> '\n' : uncomment xs
      '\t' : xs -> '\t' : toWhiteUntilEOL xs
      _    : xs -> ' '  : toWhiteUntilEOL xs

-- | A simple preprocessor.
preprocess :: [(String, String)] -> FilePath -> String -> String
preprocess env file content = undefined
