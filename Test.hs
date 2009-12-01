{-# LANGUAGE RankNTypes #-}

module DerParser.Test 
  ( module DerParser.Test
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

testStr :: String
testStr = "ssss"

testPFor :: String -> Context s (CachedParserRef s Char String)
testPFor s = foldl f init' s
  where
    f :: Context s (CachedParserRef s Char String) -> Char -> Context s (CachedParserRef s Char String)
    f parser c = der c =<< parser
    init' :: Context s (CachedParserRef s Char String)
    init' = sxList

showTestP :: (forall s. Context s (CachedParserRef s Char String)) -> IO ()
showTestP prefC = putStrLn $ runContext displayResult
  where 
    displayResult :: Context s String
    displayResult = showBase =<< prefC

    showBase :: CachedParserRef s Char String -> Context s String
    showBase p = showRec p [] 0

showTestPFor :: String -> IO ()
showTestPFor s = showTestP $ testPFor s
