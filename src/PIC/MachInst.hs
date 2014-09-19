--------------------------------------------------------------------------------
-- |
-- Module      : PIC.MachInst
-- Copyright   : (C) 2014 Brian Brooks
-- License     : BSD-style (see the file LICENSE)
-- Maintainer  : Brian Brooks <brooks.brian@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
--------------------------------------------------------------------------------
module PIC.MachInst
  ( MachInst(..)
  , toString
  , assemble
  , disassemble
  , fromIntelHex
  ) where

import Data.Bits
import Data.Word
import qualified Data.IntelHex as I
import Text.Printf

-- | Register file address (0x00 to 0x7F).
type FReg = Word8

-- | Destination select.
type DSel = Bool

-- | Literal, constant data or label.
type Lit  = Word16

-- | Bit location within 8-bit file register.
type Bit  = Word8

-- | Machine instructions
data MachInst
  -- ^ Byte-oriented file register operations
  = MI_addwf    FReg DSel
  | MI_andwf    FReg DSel
  | MI_clrf     FReg
  | MI_clrw
  | MI_comf     FReg DSel
  | MI_decf     FReg DSel
  | MI_decfsz   FReg DSel
  | MI_incf     FReg DSel
  | MI_incfsz   FReg DSel
  | MI_iorwf    FReg DSel
  | MI_movf     FReg DSel
  | MI_movwf    FReg
  | MI_nop
  | MI_rlf      FReg DSel
  | MI_rrf      FReg DSel
  | MI_subwf    FReg DSel
  | MI_swapf    FReg DSel
  | MI_xorwf    FReg DSel
  -- ^ Bit-oriented file register operations
  | MI_bcf      FReg Bit
  | MI_bsf      FReg Bit
  | MI_btfsc    FReg Bit
  | MI_btfss    FReg Bit
  -- ^ Literal and control operations
  | MI_addlw    Lit
  | MI_andlw    Lit
  | MI_call     Lit
  | MI_clrwdt
  | MI_goto     Lit
  | MI_iorlw    Lit
  | MI_movlw    Lit
  | MI_retfie
  | MI_retlw    Lit
  | MI_return
  | MI_sleep
  | MI_sublw    Lit
  | MI_xorlw    Lit
  deriving (Eq)

--------------------------------------------------------------------------------
-- Printer

showDSel :: DSel -> String
showDSel d = if d then "f" else "w"

showByteOp :: String -> FReg -> DSel -> String
showByteOp op f d = printf "%s\t0x%x, %s" op f (showDSel d)

showByteOp1 :: String -> FReg -> String
showByteOp1 = printf "%s\t0x%x"

showBinOp :: String -> FReg -> Bit -> String
showBinOp = printf "%s\t0x%x, 0x%x"

showLit :: String -> Lit -> String
showLit = printf "%s\t0x%x"

instance Show MachInst where
  show (MI_addwf   f d) = showByteOp "addwf" f d
  show (MI_andwf   f d) = showByteOp "andwf" f d
  show (MI_clrf    f  ) = showByteOp1 "clrf" f
  show (MI_clrw       ) = "clrw"
  show (MI_comf    f d) = showByteOp "comf" f d
  show (MI_decf    f d) = showByteOp "decf" f d
  show (MI_decfsz  f d) = showByteOp "decfsz" f d
  show (MI_incf    f d) = showByteOp "incf" f d
  show (MI_incfsz  f d) = showByteOp "incfsz" f d
  show (MI_iorwf   f d) = showByteOp "iorwf" f d
  show (MI_movf    f d) = showByteOp "movf" f d
  show (MI_movwf   f  ) = showByteOp1 "movwf" f
  show (MI_nop        ) = "nop"
  show (MI_rlf     f d) = showByteOp "rlf" f d
  show (MI_rrf     f d) = showByteOp "rrf" f d
  show (MI_subwf   f d) = showByteOp "subwf" f d
  show (MI_swapf   f d) = showByteOp "swapf" f d
  show (MI_xorwf   f d) = showByteOp "xorwf" f d
  show (MI_bcf     f b) = showBinOp "bcf" f b
  show (MI_bsf     f b) = showBinOp "bsf" f b
  show (MI_btfsc   f b) = showBinOp "btfsc" f b
  show (MI_btfss   f b) = showBinOp "btfss" f b
  show (MI_addlw   k  ) = showLit "addlw" k
  show (MI_andlw   k  ) = showLit "andlw" k
  show (MI_call    k  ) = showLit "call" k
  show (MI_clrwdt     ) = "clrwdt"
  show (MI_goto    k  ) = showLit "goto" k
  show (MI_iorlw   k  ) = showLit "iorlw" k
  show (MI_movlw   k  ) = showLit "movlw" k
  show (MI_retfie     ) = "retfie"
  show (MI_retlw   k  ) = showLit "retlw" k
  show (MI_return     ) = "return"
  show (MI_sleep      ) = "sleep"
  show (MI_sublw   k  ) = showLit "sublw" k
  show (MI_xorlw   k  ) = showLit "xorlw" k

-- | Pretty print.
toString :: Bool -> Bool -> [MachInst] -> String
toString withAddrs withMachCode ms =
  unlines $ if withAddrs then addAddrs 0 $ showMI withMachCode ms else
    showMI withMachCode ms
  where
    showMI :: Bool -> [MachInst] -> [String]
    showMI _ [] = []
    showMI mc (x:xs) = if mc then
      printf "%04x     %s" (assembleInst x) (show x) : showMI mc xs else
      show x : showMI mc xs
    
    addAddrs :: Int -> [String] -> [String]
    addAddrs _ [] = []
    addAddrs a (x:xs) = printf "%06x   %s" a x : addAddrs (a+1) xs

--------------------------------------------------------------------------------
-- Parser


--------------------------------------------------------------------------------
-- Machine code

assembleInst :: MachInst -> Word16
assembleInst i = case i of
  MI_addwf   f d -> 0x0700 .|. dsel d .|. freg f
  MI_andwf   f d -> 0x0500 .|. dsel d .|. freg f
  MI_clrf    f   -> 0x0180            .|. freg f
  MI_clrw        -> 0x0100
  MI_comf    f d -> 0x0900 .|. dsel d .|. freg f
  MI_decf    f d -> 0x0300 .|. dsel d .|. freg f
  MI_decfsz  f d -> 0x0b00 .|. dsel d .|. freg f
  MI_incf    f d -> 0x0a00 .|. dsel d .|. freg f
  MI_incfsz  f d -> 0x0f00 .|. dsel d .|. freg f
  MI_iorwf   f d -> 0x0400 .|. dsel d .|. freg f
  MI_movf    f d -> 0x0800 .|. dsel d .|. freg f
  MI_movwf   f   -> 0x0080            .|. freg f
  MI_nop         -> 0x0000
  MI_rlf     f d -> 0x0d00 .|. dsel d .|. freg f
  MI_rrf     f d -> 0x0c00 .|. dsel d .|. freg f
  MI_subwf   f d -> 0x0200 .|. dsel d .|. freg f
  MI_swapf   f d -> 0x0e00 .|. dsel d .|. freg f
  MI_xorwf   f d -> 0x0600 .|. dsel d .|. freg f
  MI_bcf     f b -> 0x1000 .|. bpos b .|. freg f
  MI_bsf     f b -> 0x1400 .|. bpos b .|. freg f
  MI_btfsc   f b -> 0x1800 .|. bpos b .|. freg f
  MI_btfss   f b -> 0x1c00 .|. bpos b .|. freg f
  MI_addlw   k   -> 0x3e00 .|. (k .&. 0x00ff)
  MI_andlw   k   -> 0x3900 .|. (k .&. 0x00ff)
  MI_call    k   -> 0x2000 .|. (k .&. 0x07ff)
  MI_clrwdt      -> 0x0064
  MI_goto    k   -> 0x2800 .|. (k .&. 0x07ff)
  MI_iorlw   k   -> 0x3800 .|. (k .&. 0x00ff)
  MI_movlw   k   -> 0x3000 .|. (k .&. 0x00ff)
  MI_retfie      -> 0x0009
  MI_retlw   k   -> 0x3400 .|. (k .&. 0x00ff)
  MI_return      -> 0x0008
  MI_sleep       -> 0x0063
  MI_sublw   k   -> 0x3c00 .|. (k .&. 0x00ff)
  MI_xorlw   k   -> 0x3a00 .|. (k .&. 0x00ff)
  where
    dsel d' = fromIntegral (fromEnum d') `shiftL` 0x7
    freg f' = fromIntegral f' .&. 0x007f
    bpos b' = (fromIntegral b' .&. 0x0003) `shiftL` 0x7

assemble :: [MachInst] -> [Word16]
assemble = map assembleInst

disassembleInst :: Word16 -> MachInst
disassembleInst w16 = case w16 of
  0x0100 -> MI_clrw
  0x0000 -> MI_nop
  0x0064 -> MI_clrwdt
  0x0009 -> MI_retfie
  0x0008 -> MI_return
  0x0063 -> MI_sleep
  _ -> case (w16 .&. 0x3000) `shiftR` 12 of
    0 -> case (w16 .&. 0x0f00) `shiftR` 8 of
      0x7 -> MI_addwf  (fromIntegral w16 .&. 0x007f) (testBit w16 7)
      0x5 -> MI_andwf  (fromIntegral w16 .&. 0x007f) (testBit w16 7)
      0x1 -> MI_clrf   (fromIntegral w16 .&. 0x007f)
      0x9 -> MI_comf   (fromIntegral w16 .&. 0x007f) (testBit w16 7)
      0x3 -> MI_decf   (fromIntegral w16 .&. 0x007f) (testBit w16 7)
      0xb -> MI_decfsz (fromIntegral w16 .&. 0x007f) (testBit w16 7)
      0xa -> MI_incf   (fromIntegral w16 .&. 0x007f) (testBit w16 7)
      0xf -> MI_incfsz (fromIntegral w16 .&. 0x007f) (testBit w16 7)
      0x4 -> MI_iorwf  (fromIntegral w16 .&. 0x007f) (testBit w16 7)
      0x8 -> MI_movf   (fromIntegral w16 .&. 0x007f) (testBit w16 7)
      0x0 -> MI_movwf  (fromIntegral w16 .&. 0x007f)
      0xd -> MI_rlf    (fromIntegral w16 .&. 0x007f) (testBit w16 7)
      0xc -> MI_rrf    (fromIntegral w16 .&. 0x007f) (testBit w16 7)
      0x2 -> MI_subwf  (fromIntegral w16 .&. 0x007f) (testBit w16 7)
      0xe -> MI_swapf  (fromIntegral w16 .&. 0x007f) (testBit w16 7)
      0x6 -> MI_xorwf  (fromIntegral w16 .&. 0x007f) (testBit w16 7)
      _ -> error "unknown byte op"
    1 -> case (w16 .&. 0x0c00) `shiftR` 10 of
      0 -> MI_bcf   (fromIntegral w16 .&. 0x007f) (fromIntegral ((w16 .&. 0x0380) `shiftR` 0x7))
      1 -> MI_bsf   (fromIntegral w16 .&. 0x007f) (fromIntegral ((w16 .&. 0x0380) `shiftR` 0x7))
      2 -> MI_btfsc (fromIntegral w16 .&. 0x007f) (fromIntegral ((w16 .&. 0x0380) `shiftR` 0x7))
      3 -> MI_btfss (fromIntegral w16 .&. 0x007f) (fromIntegral ((w16 .&. 0x0380) `shiftR` 0x7))
      _ -> error "impossible bit op"
    2 -> case (w16 .&. 0x0800) `shiftR` 11 of
      0 -> MI_call $ w16 .&. 0x07ff
      1 -> MI_goto $ w16 .&. 0x07ff
      _ -> error "impossible literal op"
    3 -> case (w16 .&. 0x0f00) `shiftR` 8 of
      0xe -> MI_addlw $ w16 .&. 0x00ff
      0x9 -> MI_andlw $ w16 .&. 0x00ff
      0x8 -> MI_iorlw $ w16 .&. 0x00ff
      0x0 -> MI_movlw $ w16 .&. 0x00ff
      0x4 -> MI_retlw $ w16 .&. 0x00ff
      0xc -> MI_sublw $ w16 .&. 0x00ff
      0xa -> MI_xorlw $ w16 .&. 0x00ff
      _ -> error "unknown constant op"
    _ -> error "impossible"

disassemble :: [Word16] -> [MachInst]
disassemble = map disassembleInst

--------------------------------------------------------------------------------
-- IntelHex

-- | Extract machine code from records.
fromIntelHex :: [I.Record] -> [Word16]
fromIntelHex = concatBytes . getWords
  where
    concatBytes :: [Word8] -> [Word16]
    concatBytes [] = []
    concatBytes (lsb:msb:xs) = (fromIntegral msb `shiftL` 8 .|. fromIntegral lsb) : concatBytes xs
    concatBytes _ = error "/= pow 2"
    
    getWords :: [I.Record] -> [Word8]
    getWords [] = []
    getWords (I.Record _ _ I.Data d _:rs) = d ++ getWords rs
    getWords (_:rs) = getWords rs  -- ignore non-Data records
