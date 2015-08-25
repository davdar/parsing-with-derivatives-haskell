{-# LANGUAGE TemplateHaskell #-}

module Test.SimpleTesting where

import Language.Haskell.TH
import Control.Monad
import Control.Monad.Trans
import Text.Printf

-- Test interface
class (Monad m) => MonadTest m where
  testSuccess :: m ()
  testFailure :: String -> Maybe String -> m ()

data TestStatus = TestSuccess | TestFailure

combineTestStatus :: TestStatus -> TestStatus -> TestStatus
combineTestStatus TestSuccess TestSuccess = TestSuccess
combineTestStatus _ _ = TestFailure

-- Simple Test Monad
newtype Test a = Test 
  { runTest :: (TestStatus, [(String, Maybe String)], a) }

instance Functor Test where {fmap = liftM}
instance Applicative Test where {pure = return;(<*>) = ap}
instance Monad Test where
  return = Test . (,,) TestSuccess []
  (Test (status, msgs, v)) >>= f = Test $
    let 
      (status', msgs', v') = runTest $ f v
      statusResult = combineTestStatus status status'
      msgsResult = msgs ++ msgs'
    in (statusResult, msgsResult, v')

instance MonadTest Test where
  testSuccess = return ()
  testFailure desc msgM = Test (TestFailure, [(desc, msgM)], ())

-- Test Monad Transformer
newtype TestT m a = TestT 
  { runTestT :: m (TestStatus, [(String, Maybe String)], a) }

instance MonadTrans TestT where
  lift = TestT . liftM ((,,) TestSuccess [])

instance (Monad m) => Functor (TestT m) where {fmap = liftM}
instance (Monad m) => Applicative (TestT m) where {pure = return;(<*>) = ap}
instance (Monad m) => Monad (TestT m) where
  return = lift . return
  (TestT m) >>= f = TestT $ do
    (status, msgs, v) <- m
    (status', msgs', v') <- runTestT $ f v
    let 
      statusResult = combineTestStatus status status'
      msgResult = msgs ++ msgs'
    return (statusResult, msgResult, v')

instance (Monad m) => MonadTest (TestT m) where
  testSuccess = return ()
  testFailure desc msgM = TestT $ return (TestFailure, [(desc, msgM)], ())

instance (MonadIO m) => MonadIO (TestT m) where
  liftIO = lift . liftIO

-- Testing

test :: (MonadTest m) => Bool -> m ()
test = testMsgM Nothing

testMsg :: (MonadTest m) => String -> Bool -> m ()
testMsg msg t = testMsgM (Just msg) t

testMsgM :: (MonadTest m) => Maybe String -> Bool -> m ()
testMsgM _ True = testSuccess
testMsgM msgM False = testFailure "unknownTest" msgM

testQ :: Q Exp -> Q Exp
testQ = testQMsgM Nothing

testQMsg :: String -> Q Exp -> Q Exp
testQMsg msg expQ = testQMsgM (Just msg) expQ

testQMsgM :: Maybe String -> Q Exp -> Q Exp
testQMsgM msgM expQ = do
  (Loc fn _ _ s e) <- location
  let eShow = printf "test at %s:%s:%s" fn (show s) (show e) :: String
  [| if $expQ 
        then testSuccess
        else testFailure eShow msgM |]

testC :: Bool -> Q [Dec]
testC True = return []
testC False = do
  (Loc fn _ _ s e) <- location
  error $ printf "test failed at %s:%s:%s" fn (show s) (show e)
  
runTestingC :: Test () -> Q [Dec]
runTestingC (Test (TestSuccess, _, ())) = do
  runIO $ putStrLn "Testing Succeeded"
  return []
runTestingC (Test (TestFailure, msgs, ())) = do
  (Loc fn _ _ s e) <- location
  runIO $ putStrLn $ printf "TestingFailed at %s:%s:%s" fn (show s) (show e)
  runIO $ forM_ msgs $ uncurry dispTest
  return []
  where
    dispTest desc msgM = do
      putStr $ printf "Failure for: %s" desc
      case msgM of
        Just msg -> putStrLn $ printf " ** %s" msg
        Nothing -> putStrLn ""

runTestingCT :: TestT IO () -> Q [Dec]
runTestingCT (TestT action) = do
  (status, msgs, ()) <- runIO $ action
  case status of
    TestSuccess -> do
      runIO $ putStrLn "Testing Succeeded"
      return []
    TestFailure -> do
      (Loc fn _ _ s e) <- location
      runIO $ putStrLn $ printf "Testing Failed at %s:%s:%s" fn (show s) (show e)
      runIO $ forM_ msgs $ uncurry dispTest
      return []
  where
    dispTest desc msgM = do
      putStr $ printf "Failure for: %s" desc
      case msgM of
        Just msg -> putStrLn $ printf " ** %s" msg
        Nothing -> putStrLn ""

runTesting :: Test () -> IO ()
runTesting (Test (TestSuccess, _, ())) = putStrLn "Testing Succeeded"
runTesting (Test (TestFailure, msgs, ())) = do
  putStrLn "Testing Failed"
  forM_ msgs $ uncurry dispTest
  where
    dispTest desc msgM = do
      putStr $ printf "Failure for: %s" desc
      case msgM of
        Just msg -> putStrLn $ printf " ** %s" msg
        Nothing -> putStrLn ""

runTestingT :: (MonadIO m) => TestT m () -> m ()
runTestingT (TestT m) = do
  (status, msgs, ()) <- m
  case status of
    TestSuccess -> liftIO $ putStrLn "Testing Succeeded"
    TestFailure -> do
      liftIO $ putStrLn "Testing Failed"
      liftIO $ forM_ msgs $ uncurry dispTest
  where
    dispTest desc msgM = do
      putStr $ printf "Failure for: %s" desc
      case msgM of
        Just msg -> putStrLn $ printf " ** %s" msg
        Nothing -> putStrLn ""
