import Control.Monad
import System.Environment

import Data.IntelHex as I
import PIC.MachInst as P

main :: IO ()
main = do
  r <- liftM (either (error . show) id) $ getArgs >>= I.parseFile . head
  let dis = P.disassemble $ P.fromIntelHex r
  putStrLn $ P.toString True True dis
