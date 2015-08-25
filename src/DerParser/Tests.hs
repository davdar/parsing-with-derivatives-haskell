{-# LANGUAGE RecursiveDo, TemplateHaskell, RankNTypes, ScopedTypeVariables #-}

module DerParser.Tests where

import Test.SimpleTesting
import DerParser
import Control.Monad.Fix
import Control.Monad.Trans
import System.IO.Unsafe
import Data.Typeable

testParse :: (Ord a, Typeable a, Show b, Ord b, Typeable b)
          => Context (CachedParserRef a b)
          -> [a]
          -> [b]
          -> Bool
testParse parser input expected =
  foldl (\t exp' -> t && exp' `elem` matches) True expected
    && (length expected) == (length matches)
  where
    matches = map fst $ unsafePerformIO $ evalContext $ parser >>= flip parse input

-- Match single 'x' 
matchX :: Context (CachedParserRef Char Char)
matchX = termEq 'x'

matchXTests :: (MonadTest m, MonadIO m) => m ()
matchXTests = do 
  $(testQ [| testParse matchX "x" ['x'] |])

-- Match a list of one or more 'x'
matchXL :: Context (CachedParserRef Char String)
matchXL = mfix $ \xl -> 
  xl ~> matchX ==> uncurry (flip (:)) <|> matchX ==> (:[])

matchXLTests :: (MonadTest m, MonadIO m) => m ()
matchXLTests = do 
  $(testQ [| testParse matchXL "xxxx" ["x", "xx", "xxx", "xxxx"] |])
  $(testQ [| testParse matchXL "" [] |])

-- Match a list of zero or more 'x'
matchXLEps :: Context (CachedParserRef Char String)
matchXLEps = mfix $ \xle -> 
  xle ~> matchX ==> uncurry (flip (:)) <|> eps [""]

matchXLEpsTests :: (MonadTest m, MonadIO m) => m ()
matchXLEpsTests = do 
  $(testQ [| testParse matchXLEps "xxx" ["", "x", "xx", "xxx"] |])
  $(testQ [| testParse matchXLEps "" [""] |])

-- Match a list of zero or more with pattern 'xyxy...' or 'yxyx...'
matchXYL_matchYXL :: Context (CachedParserRef Char String, CachedParserRef Char String)
matchXYL_matchYXL = mdo
  xyl <- 
    termEq 'x' <~ yxl
      ==> uncurry (:)
    <|> 
    eps [""]

  yxl <- 
    termEq 'y' <~ xyl
      ==> uncurry (:)
    <|> 
    eps [""]

  return (xyl, yxl)

matchXYL :: Context (CachedParserRef Char String)
matchXYL = fst <$> matchXYL_matchYXL

matchYXL :: Context (CachedParserRef Char String)
matchYXL = snd <$> matchXYL_matchYXL

matchXYL_matchYXLTests :: (MonadTest m, MonadIO m) => m ()
matchXYL_matchYXLTests = do 
  $(testQ [| testParse matchXYL "xyxyxy" ["", "x", "xy", "xyx", "xyxy", "xyxyx", "xyxyxy"] |])
  $(testQ [| testParse matchXYL "yxyxyx" [""] |])
  $(testQ [| testParse matchXYL "" [""] |])

  $(testQ [| testParse matchYXL "xyxyxy" [""] |])
  $(testQ [| testParse matchYXL "yxyxyx" ["", "y", "yx", "yxy", "yxyx", "yxyxy", "yxyxyx"] |])
  $(testQ [| testParse matchYXL "" [""] |])

-- A grammer for s-expressions
sx_sxList :: Context (CachedParserRef Char String, CachedParserRef Char String)
sx_sxList = mdo
  sx' <- 
    termEq '(' <~ sxList' <~> termEq ')' 
      ==> uncurry2 (\'(' list ')' -> "(" ++ list ++ ")")
    <|> 
    termEq 's' 
      ==> (:[])

  -- sxStar <- rep sx' [] (\s1 s2 -> s1 ++ s2)
  -- FAST (linear for depth)
  sxList' <- eps [""] <|> sx' .~ sxList' ==> uncurry (++)
  
  -- SLOW (exponential for depth)
  -- sxConsList <- sx' .~ sxStar <~> termEq '.' <~ sx' ==> uncurry3 (\s1 s2 '.' s3 -> s1 ++ s2 ++ s3)
  -- sxList' <- sxStar .| sxConsList

  -- ALSO FAST (linear for depth)
  -- sxList' <- 
  --   eps [""]
  --   <|> 
  --   sx'.~ sxStar <~> (termEq '.' <~ sx' ==> uncurry (\ '.' s -> s) <|> eps [""]) ==> uncurry2 (\ s1 s2 s3 -> s1 ++ s2 ++ s3)

  return (sx', sxList')

  where
    uncurry2 = uncurry . uncurry
    -- uncurry3 = uncurry . uncurry2

sx :: Context (CachedParserRef Char String)
sx = fst <$> sx_sxList

sxList :: Context (CachedParserRef Char String)
sxList = snd <$> sx_sxList

sx_sxListTests :: (MonadTest m, MonadIO m) => m ()
sx_sxListTests = do 
  $(testQ [| testParse sx "s" ["s"] |])
  $(testQ [| testParse sx "(sss(ss)s)" ["(sss(ss)s)"] |])
  $(testQ [| testParse sxList "sss" ["", "s", "ss", "sss"] |])
  $(testQ [| testParse sxList "s(s)()" ["", "s", "s(s)", "s(s)()"] |])

-- A nasty ambiguous grammar for things like "1+1+1"
addExp :: Context (CachedParserRef Char String)
addExp = mdo 
  addExp <- 
    termEq '1' ==> (:[])
    <|> 
    termEq '1' <~> termEq '+' <~ addExp
      ==> uncurry2 (\'1' '+' e -> '{':'1':'+':e ++ "}")
    <|> 
    addExp ~> termEq '+' <~> termEq '1'
      ==> uncurry2 (\e '+' '1' -> "{" ++ e ++ '+':'1':'}':[])
    <|> 
    addExp ~> termEq '+' <~ addExp
      ==> uncurry2 (\e1 '+' e2 -> "{" ++ e1 ++ "+" ++ e2 ++ "}")
    <| addExp
    <| addExp
  return addExp

  where uncurry2 = uncurry . uncurry

addExpTests :: (MonadTest m, MonadIO m) => m ()
addExpTests = do 
  $(testQ [| testParse addExp "1+1" ["1", "{1+1}"] |])
  $(testQ [| testParse addExp "1+1+1" [ "1", "{1+1}"
                                             , "{1+{1+1}}"
                                             , "{{1+1}+1}" 
                                             ] |])
  $(testQ [| testParse addExp "x" [] |])

{-
repFoo :: Context (CachedParserRef Char String)
repFoo = mdo
  foo <- 
    termEq 'f' <~> termEq 'o' <~> termEq 'o'
      ==> uncurry2 (\f o1 o2 -> f:o1:o2:[])
  repFoo' <- rep foo [] (\x1 x2 -> x1 ++ x2)
  return repFoo'

  where uncurry2 = uncurry . uncurry

repFooTests :: (MonadTest m, MonadIO m) => m ()
repFooTests = do
  $(testQ [| testParse repFoo "foofoo" ["", "foo", "foofoo"] |])
  $(testQ [| testParse repFoo "foobar" ["", "foo" ] |])
  -}

derParserTests :: (MonadTest m, MonadIO m) => m ()
derParserTests = do 
  matchXTests
  matchXLTests
  matchXLEpsTests
  matchXYL_matchYXLTests
  sx_sxListTests
  addExpTests
  -- repFooTests
