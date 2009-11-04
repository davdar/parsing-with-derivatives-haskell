module Main where

import SimpleTesting
import DerParser

main :: IO ()
main = do
  putStr "DerParser Tests: "
  putStr $ uncurry displayTestResults $ execTestResult derParserTests
