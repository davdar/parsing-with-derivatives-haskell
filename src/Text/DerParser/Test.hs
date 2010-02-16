{-# LANGUAGE RankNTypes #-}

module Text.DerParser.Test
  ( module Text.DerParser.Test
  , module Text.DerParser.Tests
  , module Text.DerParser
  ) where

import Test.SimpleTesting
import Text.DerParser.Tests
import Text.DerParser
import System.IO.Unsafe
import Control.Applicative
import System.IO

main :: IO ()
main = do
{-
  [input] <- lines <$> hGetContents stdin
  result <-
    evalContext $ do
      p <- sxList
      parseFull p input
  putStrLn (show result)
  -}

  putStr "DerParser Tests: "
  runTestingT derParserTests

testPFor :: String -> Context (CachedParserRef Char String)
testPFor s = foldl f init' s
  where
    f :: Context (CachedParserRef Char String) -> Char -> Context (CachedParserRef Char String)
    f parser c = der c =<< parser
    init' :: Context (CachedParserRef Char String)
    init' = sxList

showTestP :: Context (CachedParserRef Char String) -> IO ()
showTestP prefC = putStrLn $ unsafePerformIO $ evalContext displayResult
  where 
    displayResult :: Context String
    displayResult = showRec =<< prefC

showTestPFor :: String -> IO ()
showTestPFor = showTestP . testPFor

parsePFor :: String -> Context [(String, [Char])]
parsePFor s = flip parse s =<< sxList

showParseP :: Context [(String, [Char])] -> IO ()
showParseP pResult = putStrLn $ unsafePerformIO $ evalContext $ show <$> pResult

showParsePFor :: String -> IO ()
showParsePFor = showParseP . parsePFor

sizePFor :: String -> IO ()
sizePFor s = putStrLn . show . unsafePerformIO . evalContext $ parserSize =<< derived
  where
    derived = testPFor s
