{-# LANGUAGE RankNTypes #-}

module DerParser.Test 
  ( module DerParser.Test
  , module DerParser.Tests
  , module DerParser.Base
  ) where

import SimpleTesting.Base
import DerParser.Tests
import DerParser.Base
import System.IO.Unsafe
import Control.Applicative

main :: IO ()
main = do
  putStr "DerParser Tests: "
  putStr . (uncurry displayTestResults) =<< execTestResultT derParserTests

testPFor :: String -> Context (CachedParserRef Char String)
testPFor s = foldl f init' s
  where
    f :: Context (CachedParserRef Char String) -> Char -> Context (CachedParserRef Char String)
    f parser c = der c =<< parser
    init' :: Context (CachedParserRef Char String)
    init' = sxList

showTestP :: Context (CachedParserRef Char String) -> IO ()
showTestP prefC = putStrLn $ unsafePerformIO $ runContext displayResult
  where 
    displayResult :: Context String
    displayResult = showBase =<< prefC

    showBase :: CachedParserRef Char String -> Context String
    showBase p = showRec p [] 0

showTestPFor :: String -> IO ()
showTestPFor = showTestP . testPFor

parsePFor :: String -> Context [(String, [Char])]
parsePFor s = flip parse s =<< sxList

showParseP :: Context [(String, [Char])] -> IO ()
showParseP pResult = putStrLn $ unsafePerformIO $ runContext $ show <$> pResult

showParsePFor :: String -> IO ()
showParsePFor = showParseP . parsePFor

sizePFor :: String -> IO ()
sizePFor s = putStrLn . show . unsafePerformIO . runContext $ flip parserSize [] =<< derived
  where
    derived = testPFor s
