--------------------------------------------------------------------------------
-- |
-- Module      : Data.IntelHex
-- Copyright   : (C) 2014 Brian Brooks
-- License     : BSD-style (see the file LICENSE)
-- Maintainer  : Brian Brooks <brooks.brian@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
--------------------------------------------------------------------------------
module Data.IntelHex
  ( Record(..)
  , RecordType(..)
  , parse
  , parseFile
  , toString
  , toFile
  ) where

import Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as P8
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Word
import Text.Printf


data RecordType
  = Data
  | EOF
  | ExtSeg
  | StartSeg
  | ExtLinear
  | StartLinear

data Record = Record
  { length        :: !Word8
  , startAddr     :: !Word16
  , recordType    :: !RecordType
  , dataBytes     :: [Word8]
  , chksum        :: !Word8
  }

-- TODO: validate stuff: starting addresses & checksums, etc.

--------------------------------------------------------------------------------
-- Parsing

hexN :: (Num a, Bits a) => Int -> P8.Parser a
hexN n = B.foldl' step 0 `fmap` P8.take n
  where
    step a w | w >= 48 && w <= 57 = (a `shiftL` 4) .|. fromIntegral (w - 48)
             | w >= 97            = (a `shiftL` 4) .|. fromIntegral (w - 87)
             | otherwise          = (a `shiftL` 4) .|. fromIntegral (w - 55)

recordTypeParser :: P8.Parser RecordType
recordTypeParser =
  (P8.string (B8.pack "00") >> return Data) <|>
  (P8.string (B8.pack "01") >> return EOF) <|>
  (P8.string (B8.pack "02") >> return ExtSeg) <|>
  (P8.string (B8.pack "03") >> return StartSeg) <|>
  (P8.string (B8.pack "04") >> return ExtLinear) <|>
  (P8.string (B8.pack "05") >> return StartLinear)

recordParser :: P8.Parser Record
recordParser = do
  _   <- P8.char8 ':'
  len <- hexN 2
  sa  <- hexN 4
  ty  <- recordTypeParser
  db  <- P8.count (fromIntegral len) (hexN 2)
  cs  <- hexN 2 <* P8.endOfLine
  return $! Record len sa ty db cs

-- | Parse INHX8m bytestring.
parse :: B8.ByteString -> Either String [Record]
parse = P8.parseOnly (parseRecords <* P8.endOfInput)
  where
    parseRecords = P8.many1' recordParser

-- | Parse file.
parseFile :: FilePath -> IO (Either String [Record])
parseFile f = parse `fmap` B8.readFile f

--------------------------------------------------------------------------------
-- Printing

instance Show RecordType where
  show Data        = "00"
  show EOF         = "01"
  show ExtSeg      = "02"
  show StartSeg    = "03"
  show ExtLinear   = "04"
  show StartLinear = "05"

instance Show Record where
  show (Record len sa rt db cs) =
    printf ":%02X%04X%s%s%02X" len sa (show rt) (showListFmt "%02X" db) cs

showListFmt :: (PrintfArg a) => String -> [a] -> String
showListFmt s = concatMap (printf s)

-- | Pretty print.
toString :: [Record] -> String
toString rs = unlines $ map show rs

-- | Pretty print to file.
toFile :: FilePath -> [Record] -> IO ()
toFile f rs = writeFile f (toString rs)
