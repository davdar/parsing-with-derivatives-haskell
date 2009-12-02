{-# LANGUAGE RecursiveDo, TemplateHaskell, RankNTypes #-}
module DerParser.Tests where

import SimpleTesting.Base
import DerParser.Base
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad
import Control.Applicative((<$>))

testParse :: (Ord a, Ord b, Show b)
          => (forall s. Context s (CachedParserRef s a b))
          -> [a]
          -> [b]
          -> Bool
testParse parser input expected =
  foldl (\t exp' -> t && exp' `elem` matches) True expected
    && (length expected) == (length matches)
  where
    matches = map fst $ runContext $ parser >>= flip parse input

-- Match single 'x' 
matchX :: Context s (CachedParserRef s Char Char)
matchX = termEq 'x'

matchXTests :: (MonadTestResult m, MonadIO m) => m ()
matchXTests = do 
  $(performTest' [| 
    testParse matchX "x" ['x'] 
    |])

-- Match a list of one or more 'x'
matchXL :: Context s (CachedParserRef s Char String)
matchXL = mfix $ \xl -> 
  xl ~> matchX ==> uncurry (flip (:)) <|> matchX ==> (:[])

matchXLTests :: (MonadTestResult m, MonadIO m) => m ()
matchXLTests = do 
  $(performTest' [| testParse matchXL "xxxx" ["x", "xx", "xxx", "xxxx"] |])
  $(performTest' [| testParse matchXL "" [] |])

-- Match a list of zero or more 'x'
matchXLEps :: Context s (CachedParserRef s Char String)
matchXLEps = mfix $ \xle -> 
  xle ~> matchX ==> uncurry (flip (:)) <|> eps [""]

matchXLEpsTests :: (MonadTestResult m, MonadIO m) => m ()
matchXLEpsTests = do 
  $(performTest' [| testParse matchXLEps "xxx" ["", "x", "xx", "xxx"] |])
  $(performTest' [| testParse matchXLEps "" [""] |])

-- Match a list of zero or more with pattern 'xyxy...' or 'yxyx...'
matchXYL_matchYXL :: Context s (CachedParserRef s Char String, CachedParserRef s Char String)
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

matchXYL :: Context s (CachedParserRef s Char String)
matchXYL = fst <$> matchXYL_matchYXL

matchYXL :: Context s (CachedParserRef s Char String)
matchYXL = snd <$> matchXYL_matchYXL

matchXYL_matchYXLTests :: (MonadTestResult m, MonadIO m) => m ()
matchXYL_matchYXLTests = do 
  $(performTest' [| testParse matchXYL "xyxyxy" ["", "x", "xy", "xyx", "xyxy", "xyxyx", "xyxyxy"] |])
  $(performTest' [| testParse matchXYL "yxyxyx" [""] |])
  $(performTest' [| testParse matchXYL "" [""] |])

  $(performTest' [| testParse matchYXL "xyxyxy" [""] |])
  $(performTest' [| testParse matchYXL "yxyxyx" ["", "y", "yx", "yxy", "yxyx", "yxyxy", "yxyxyx"] |])
  $(performTest' [| testParse matchYXL "" [""] |])

-- A grammer for s-expressions
sx_sxList :: Context s (CachedParserRef s Char String, CachedParserRef s Char String)
sx_sxList = mdo
  sx <- 
    termEq '(' <~ sxList <~> termEq ')' 
      ==> uncurry2 (\'(' list ')' -> "(" ++ list ++ ")")
    <|> 
    termEq 's' 
      ==> (:[])

  sxList <- rep sx [] (\s ss -> s ++ ss)

  return (sx, sxList)

  where uncurry2 = uncurry . uncurry

sx :: Context s (CachedParserRef s Char String)
sx = fst <$> sx_sxList

sxList :: Context s (CachedParserRef s Char String)
sxList = snd <$> sx_sxList

sx_sxListTests :: (MonadTestResult m, MonadIO m) => m ()
sx_sxListTests = do 
  $(performTest' [| testParse sx "s" ["s"] |])
  $(performTest' [| testParse sx "(sss(ss)s)" ["(sss(ss)s)"] |])
  $(performTest' [| testParse sxList "sss" ["", "s", "ss", "sss"] |])
  $(performTest' [| testParse sxList "s(s)()" ["", "s", "s(s)", "s(s)()"] |])

-- A nasty ambiguous grammar for things like "1+1+1"
addExp :: Context s (CachedParserRef s Char String)
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

addExpTests :: (MonadTestResult m, MonadIO m) => m ()
addExpTests = do 
  $(performTest' [| testParse addExp "1+1" ["1", "{1+1}"] |])
  $(performTest' [| testParse addExp "1+1+1" [ "1", "{1+1}"
                                             , "{1+{1+1}}"
                                             , "{{1+1}+1}" 
                                             ] |])
  $(performTest' [| testParse addExp "x" [] |])

repFoo :: Context s (CachedParserRef s Char String)
repFoo = mdo
  foo <- 
    termEq 'f' <~> termEq 'o' <~> termEq 'o'
      ==> uncurry2 (\f o1 o2 -> f:o1:o2:[])
  repFoo' <- rep foo [] (\x1 x2 -> x1 ++ x2)
  return repFoo'

  where uncurry2 = uncurry . uncurry

repFooTests :: (MonadTestResult m, MonadIO m) => m ()
repFooTests = do
  $(performTest' [| testParse repFoo "foofoo" ["", "foo", "foofoo"] |])
  $(performTest' [| testParse repFoo "foobar" ["", "foo" ] |])

derParserTests :: (MonadTestResult m, MonadIO m) => m ()
derParserTests = do 
  matchXTests
  matchXLTests
  matchXLEpsTests
  matchXYL_matchYXLTests
  sx_sxListTests
  addExpTests
  repFooTests
