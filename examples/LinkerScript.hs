import Control.Monad
import System.Environment

import qualified PIC.LinkerScript.Parser as LS

main :: IO ()
main = do
  (f:_) <- getArgs
  contents <- readFile f
  putStrLn $ show $ LS.parseFile [] f contents
