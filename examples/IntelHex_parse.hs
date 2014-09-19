import Control.Monad
import Data.IntelHex as IH
import System.Environment

main :: IO ()
main = do
  r <- liftM (either (error . show) id) $ getArgs >>= IH.parseFile . head
  putStrLn $ toString r
