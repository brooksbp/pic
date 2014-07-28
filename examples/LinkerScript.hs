import Control.Monad
import System.Environment

import qualified PIC.LinkerScript.Parser as LS

main :: IO ()
main = do
  (f:_) <- getArgs
  contents <- readFile f
  putStrLn $ LS.preprocess LS.defaultMacros f contents
  -- putStrLn $ show $ LS.parseFile LS.defaultMacros f contents
