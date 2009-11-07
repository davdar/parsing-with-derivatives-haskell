{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module DerParser.Tests where

import SimpleTesting.Base
import DerParser.Base
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad

testParse :: (Ord a, Ord b)
          => (forall s. Context s (CachedParserRef s a b))
          -> [a]
          -> [b]
          -> Bool
testParse parser input expected
  = let matches = map fst $ runContext $ parser >>= flip parse input
    in foldl (\t exp' -> t && exp' `elem` matches) True expected
    && (length expected) == (length matches)

-- Match single 'x' 
matchX :: Context s (CachedParserRef s Char Char)
matchX = termEq 'x'

matchXTests :: (MonadTestResult m, MonadIO m) => m ()
matchXTests 
  = do $(performTest' [| testParse matchX "x" ['x'] |])

-- Match a list of one or more 'x'
matchXL :: Context s (CachedParserRef s Char String)
matchXL 
  = mfix $ \xl -> 
      xl <:~> matchX ==> uncurry (flip (:)) <|> matchX ==> (:[])

matchXLTests :: (MonadTestResult m, MonadIO m) => m ()
matchXLTests 
  = do $(performTest' [| testParse matchXL "xxxx" ["x", "xx", "xxx", "xxxx"] |])
       $(performTest' [| testParse matchXL "" [] |])

-- Match a list of zero or more 'x'
matchXLEps :: Context s (CachedParserRef s Char String)
matchXLEps 
  = mfix $ \xle -> 
    xle <:~> matchX ==> uncurry (flip (:)) <|> eps [""]

matchXLEpsTests :: (MonadTestResult m, MonadIO m) => m ()
matchXLEpsTests
  = do $(performTest' [| testParse matchXLEps "xxx" ["", "x", "xx", "xxx"] |])
       $(performTest' [| testParse matchXLEps "" [""] |])

-- Match a list of zero or more with pattern 'xyxy...' or 'yxyx...'
matchXYL_matchYXL :: Context s (CachedParserRef s Char String, CachedParserRef s Char String)
matchXYL_matchYXL = do
  xyl' <- dummyRef
  yxl' <- dummyRef

  xyl'' <- 
    termEq 'x' <~:> yxl'
      ==> uncurry (:)
    <|> 
    eps [""]
  yxl'' <- 
    termEq 'y' <~:> xyl'
      ==> uncurry (:)
    <|> 
    eps [""]

  link xyl' xyl''
  link yxl' yxl''

  return (xyl', yxl')

matchXYL :: Context s (CachedParserRef s Char String)
matchXYL = liftM fst matchXYL_matchYXL

matchYXL :: Context s (CachedParserRef s Char String)
matchYXL = liftM snd matchXYL_matchYXL

matchXYL_matchYXLTests :: (MonadTestResult m, MonadIO m) => m ()
matchXYL_matchYXLTests 
  = do $(performTest' [| testParse matchXYL "xyxyxy" ["", "x", "xy", "xyx", "xyxy", "xyxyx", "xyxyxy"] |])
       $(performTest' [| testParse matchXYL "yxyxyx" [""] |])
       $(performTest' [| testParse matchXYL "" [""] |])

       $(performTest' [| testParse matchYXL "xyxyxy" [""] |])
       $(performTest' [| testParse matchYXL "yxyxyx" ["", "y", "yx", "yxy", "yxyx", "yxyxy", "yxyxyx"] |])
       $(performTest' [| testParse matchYXL "" [""] |])

-- A grammer for s-expressions
sx_sxList :: Context s (CachedParserRef s Char String, CachedParserRef s Char String)
sx_sxList = do
  sx' <- dummyRef
  sxList' <- dummyRef

  sx'' <- 
    termEq '(' <~> return sxList' <~> termEq ')' 
      ==> (\('(',(list,')')) -> "(" ++ list ++ ")")
    <|> 
    termEq 's' 
      ==> (:[])

  sxList'' <- 
    return sx' <~> return sxList'
      ==> uncurry (++) 
    <|> 
    eps [""]

  link sx' sx''
  link sxList' sxList''

  return (sx', sxList')

sx :: Context s (CachedParserRef s Char String)
sx = liftM fst sx_sxList

sxList :: Context s (CachedParserRef s Char String)
sxList = liftM snd sx_sxList

sx_sxListTests :: (MonadTestResult m, MonadIO m) => m ()
sx_sxListTests
  = do $(performTest' [| testParse sx "s" ["s"] |])
       $(performTest' [| testParse sx "(sss(ss)s)" ["(sss(ss)s)"] |])
       $(performTest' [| testParse sxList "sss" ["", "s", "ss", "sss"] |])
       $(performTest' [| testParse sxList "s(s)()" ["", "s", "s(s)", "s(s)()"] |])

-- A nasty ambiguous grammar for things like "1+1+1"
addExp :: Context s (CachedParserRef s Char String)
addExp
 = do addExp' <- dummyRef
      addExp'' <- termEq '1' ==> (:[])
                  <|> 
                  termEq '1' <~> termEq '+' <~:> addExp'
                    ==> (\('1',('+',e)) -> '{':'1':'+':e ++ "}")
                  <|> 
                  addExp' <:~> termEq '+' <~> termEq '1'
                    ==> (\(e,('+','1')) -> "{" ++ e ++ '+':'1':'}':[])
                  <|> 
                  addExp' <:~> termEq '+' <~:> addExp'
                    ==> (\(e1,('+',e2)) -> "{" ++ e1 ++ "+" ++ e2 ++ "}")
      link addExp' addExp''
      return addExp'

addExpTests :: (MonadTestResult m, MonadIO m) => m ()
addExpTests
  = do $(performTest' [| testParse addExp "1+1" ["1", "{1+1}"] |])
       $(performTest' [| testParse addExp "1+1+1" [ "1", "{1+1}"
                                                  , "{1+{1+1}}"
                                                  , "{{1+1}+1}" 
                                                  ] |])

derParserTests :: (MonadTestResult m, MonadIO m) => m ()
derParserTests 
  = do matchXTests
       matchXLTests
       matchXLEpsTests
       matchXYL_matchYXLTests
       sx_sxListTests
       addExpTests
