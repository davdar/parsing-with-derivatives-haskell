module DerParser.Test 
  ( main
  , module DerParser.Tests
  , module DerParser.Base
  ) where

import SimpleTesting.Base
import DerParser.Tests
import DerParser.Base

main :: IO ()
main = do
  putStr "DerParser Tests: "
  putStr . (uncurry displayTestResults) =<< execTestResultT derParserTests

