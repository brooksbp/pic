module PIC.LinkerScript.Parser.Preprocess
  ( uncomment
  , preprocess
  , defaultMacros
  ) where

-- | Remove "//"-style comments from code. Is not string-literal-aware.
uncomment :: String -> String
uncomment = uncomment'
  where
    uncomment' a = case a of
      ""             -> ""
      '/' : '/' : xs -> "  " ++ toWhiteUntilEOL xs
      x         : xs -> x : uncomment' xs

    toWhiteUntilEOL a = case a of
      ""              -> ""
      '\n' : xs -> '\n' : uncomment' xs
      '\t' : xs -> '\t' : toWhiteUntilEOL xs
      _    : xs -> ' '  : toWhiteUntilEOL xs

-- | Default macros.  TODO: may need to remove since some are just existance chks.
defaultMacros :: [(String, String)]
defaultMacros = [
    ("_CRUNTIME","")
  , ("_EXTENDEDMODE","")
  , ("_DEBUG","")
  , ("_DEBUGCODESTART","0x0")
  , ("_DEBUGCODELEN","0xFF")
  , ("_DEBUGDATASTART","0x100")
  , ("_DEBUGDATALEN","0x1FF")
  ]

-- | A simple preprocessor.
preprocess :: [(String, String)] -> FilePath -> String -> String
preprocess env file content = unlines $ pp True [] env $ lines $ uncomment content
  where
    pp :: Bool -> [Bool] -> [(String, String)] -> [String] -> [String]
    pp _ _ _ [] = []
    pp on stack env (a : rest) = case words a of
      "#ERROR"  : _            -> error $ show a
      "#DEFINE" : name : value -> "" : pp on stack (if on then (name, ppLine env $ unwords value) : env else env) rest
      "#IFDEF"  : name : _     -> "" : pp (on && elem name (fst $ unzip env)) (on : stack) env rest
      "#ELSE" : _
        | not $ null stack     -> "" : pp (head stack && not on) stack env rest
        | otherwise            -> error "#ELSE without associated #IFDEF"
      "#FI" : _
        | not $ null stack     -> "" : pp (head stack) (tail stack) env rest
        | otherwise            -> error "#FI without associated #IFDEF"
      _                        -> (if on then ppLine env a else "") : pp on stack env rest


ppLine :: [(String, String)] -> String -> String
ppLine _ "" = ""
ppLine env ('_' : a) = case lookup ("_" ++ name) env of
  Just value -> value ++ ppLine env rest
  Nothing    -> error $ "Undefined macro: _" ++ name ++ " Env: " ++ show env
  where
    name = takeWhile (flip elem $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_") a
    rest = drop (length name) a
ppLine env (a : b) = a : ppLine env b
