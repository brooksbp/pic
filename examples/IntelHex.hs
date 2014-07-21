import Control.Monad
import Data.IntelHex as Ihx
import System.Environment

main :: IO ()
main = do
  r <- liftM (either (error . show) id) $ getArgs >>= Ihx.parseFile . head
  putStrLn (toString r)
