module DerParser.Test where

import SimpleTesting.Base
import DerParser.Base

main :: IO ()
main = do
  putStr "DerParser Tests: "
  putStr $ uncurry displayTestResults $ execTestResult derParserTests
