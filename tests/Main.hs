module Main where

import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import Data.List

import PIC.MachInst

main :: IO ()
main = defaultMainWithOpts [
    testCase "MI1" testMI_1
  , testProperty "MIAsmDisId" propMIAsmDisId
  ] mempty

testMI_1 :: Assertion
testMI_1 = disassemble (assemble insts) @?= insts
  where insts = [MI_nop, MI_incf 4 True]

instance Arbitrary MachInst where
  arbitrary = elements [
      MI_clrw
    , MI_nop
    , MI_incf 0x7f True
    ]

propMIAsmDisId :: [MachInst] -> Property
propMIAsmDisId xs = not (null xs) ==> disassemble (assemble xs) == xs
