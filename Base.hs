{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, RecursiveDo, TemplateHaskell, RankNTypes, GeneralizedNewtypeDeriving, GADTs, MultiParamTypeClasses, FlexibleInstances #-}

-- Author: David Darais

module DerParser.Base
  ( Context, runContext, CachedParserRef
  , term, termEq
  , (.~), (~>), (<~), (<~>)
  , (.|), (|>), (<|), (<|>)
  , eps, emp, (==>), rep
  , parse, parseFull
  , isEmpty, der, derInner
  , showRec
  , weed
  , parserSize
  ) where

import Text.Printf
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Control.Applicative hiding ((<|>))
import Data.IORef
import Data.Typeable

-- ID Generator (Monad Transformer)
newtype IDHandlerT m a =
  IDHandlerT { unwrapIDHandlerT :: StateT Int m a }
  deriving (Monad, MonadTrans, MonadFix, Functor)

instance (Monad m) => Applicative (IDHandlerT m) where
  pure = return
  (<*>) = ap

nextID :: (Monad m) => IDHandlerT m Int
nextID = IDHandlerT $ do
  n <- get
  put $ n + 1
  return n

evalIDT :: (Monad m) => IDHandlerT m a -> m a
evalIDT = ($ 0) . evalStateT . unwrapIDHandlerT

-- ChangeCell (Monad Transformer)
newtype ChangeCellT m a =
  ChangeCellT 
  { unwrapChangeCellT :: StateT (Bool, [Int]) m a }
  deriving (Monad, MonadTrans, MonadFix)

flagChanged :: (Monad m) => ChangeCellT m ()
flagChanged = ChangeCellT $ do
  (_, seen) <- get
  put (True, seen)

flagSeen :: (Monad m) => Int -> ChangeCellT m ()
flagSeen ident = ChangeCellT $ do
  (changed, seen) <- get
  put (changed, ident:seen)

hasSeen :: (Monad m) => Int -> ChangeCellT m Bool
hasSeen ident = ChangeCellT $ do
  (_, seen) <- get
  return $ ident `elem` seen

execChangeCellT :: (Monad m, Functor m) => ChangeCellT m a -> m Bool
execChangeCellT cellt = fst <$> execStateT (unwrapChangeCellT cellt) (False, [])

-- Context and ChangeContext (Monads)
type Context a = IDHandlerT IO a
type ChangeContext a = ChangeCellT (IDHandlerT IO) a

runContext :: Context a -> IO a
runContext = evalIDT

execChangeContext :: ChangeContext a -> Context Bool
execChangeContext = execChangeCellT

-- Parser: The main parser type. Note recursive structures contain references
-- to children (instead of values.)
--
-- Typing relies heavily on GADTs.
--
data Parser t a where
  Terminal
    :: (t -> Bool) -> Parser t t

  Con
    :: (Show a, Ord a, Typeable a, Show b, Ord b, Typeable b)
    => CachedParserRef t a
    -> CachedParserRef t b
    -> Parser t (a, b)

  Alt
    :: CachedParserRef t a
    -> CachedParserRef t a
    -> Parser t a

  Epsilon
    :: [a] -> Parser t a

  Empty
    :: Parser t a

  Reduction
    :: (Show a, Ord a, Typeable a)
    => CachedParserRef t a
    -> (a -> b)
    -> Int
    -> Parser t b

  Repetition
    :: CachedParserRef t a
    -- initial
    -> a
    -- reduction function
    -> (a -> a -> a)
    -- function id
    -> Int
    -- progress
    -> [[a]]
    -- `clean' version (no progress)
    -> CachedParserRef t a
    -> Parser t a

  deriving (Typeable)

-- IDParser: Wraps around a parser and gives it an ID
data IDParser t a = IDParser (Parser t a) Int

idParserParser :: IDParser t a -> Parser t a
idParserParser (IDParser p _) = p

-- CachedParser: Wraps around an IDParser and caches things.
data CachedParser t a =
  CachedParser
  { cachedParserIDParser
      :: (IDParser t a)
  , cachedParserIsInit
      :: Bool
  , cachedParserIsNullable
      :: Bool
  , cachedParserIsEmpty
      :: Bool
  , cachedParserParseNull
      :: Set.Set a
  , cachedParserCachedDerivatives
      :: Map.Map t (CachedParserRef t a)
  }

-- CachedParserRef: A wrapper around a reference to a CachedParser.  This
-- newtype is necessary in order to typeclass with partially applied type args.
newtype CachedParserRef t a =
  CachedParserRef 
  { getCachedRef :: IORef (CachedParser t a) }
  deriving (Eq, Typeable)

-- Recursive Operations on parser children
parserChildrenDo
  :: (Monad m, Ord a, Typeable a) 
  => Parser t a 
  -> ( forall b. (Ord b, Typeable b)
              => CachedParserRef t b 
              -> m ()
     )
  -> m ()
parserChildrenDo (Terminal _) _ = return ()
parserChildrenDo (Epsilon _) _ = return ()
parserChildrenDo Empty _ = return ()
parserChildrenDo (Con first second) f = f first >> f second
parserChildrenDo (Alt lhs rhs) f = f lhs >> f rhs
parserChildrenDo (Reduction p _ _) f = f p
parserChildrenDo (Repetition p _ _ _ _ _) f = f p

-- Direct reading and writing reference
writeCachedParserRef :: CachedParserRef t a -> CachedParser t a -> Context ()
writeCachedParserRef ref parser = lift $ writeIORef (getCachedRef ref) parser

readCachedParserRefCached :: CachedParserRef t a -> Context (CachedParser t a)
readCachedParserRefCached ref = lift $ readIORef $ getCachedRef ref

readCachedParserRefID :: CachedParserRef t a -> Context (IDParser t a)
readCachedParserRefID ref = cachedParserIDParser <$> readCachedParserRefCached ref

readCachedParserRef :: CachedParserRef t a -> Context (Parser t a)
readCachedParserRef ref = idParserParser <$> readCachedParserRefID ref

-- Reference accessors
isInit :: CachedParserRef t a -> Context Bool
isInit = liftM cachedParserIsInit . readCachedParserRefCached

cachedDerivatives :: CachedParserRef t a -> Context (Map.Map t (CachedParserRef t a))
cachedDerivatives = liftM cachedParserCachedDerivatives . readCachedParserRefCached

isEmpty :: (Ord a, Typeable a) => CachedParserRef t a -> Context Bool
isEmpty ref = do
  initialize ref 
  cachedParserIsEmpty <$> readCachedParserRefCached ref

isNullable :: (Ord a, Typeable a) => CachedParserRef t a -> Context Bool
isNullable ref = do
  initialize ref
  cachedParserIsNullable <$> readCachedParserRefCached ref

parseNull :: (Ord a, Typeable a) => CachedParserRef t a -> Context (Set.Set a)
parseNull ref = do
  initialize ref
  cachedParserParseNull <$> readCachedParserRefCached ref

-- Reference mutators
setInitialized :: CachedParserRef t a -> Bool -> Context ()
setInitialized ref iToSet = do
  cp <- readCachedParserRefCached ref
  if cachedParserIsInit cp /= iToSet
    then writeCachedParserRef ref cp { cachedParserIsInit = iToSet }
    else return ()

setCachedDerivatives :: (Eq t) => CachedParserRef t a -> Map.Map t (CachedParserRef t a) -> Context ()
setCachedDerivatives ref dToSet = do
  cp <- readCachedParserRefCached ref
  if cachedParserCachedDerivatives cp /= dToSet
    then writeCachedParserRef ref cp { cachedParserCachedDerivatives = dToSet }
    else return ()

setNullableWithChange :: CachedParserRef t a -> Bool -> ChangeContext ()
setNullableWithChange ref nToSet = do
  cp <- lift $ readCachedParserRefCached ref
  if cachedParserIsNullable cp /= nToSet
    then do
      lift $ writeCachedParserRef ref cp { cachedParserIsNullable = nToSet }
      flagChanged
    else return ()

setEmptyWithChange :: CachedParserRef t a -> Bool -> ChangeContext ()
setEmptyWithChange ref eToSet = do
  cp <- lift $ readCachedParserRefCached ref
  if cachedParserIsEmpty cp /= eToSet
    then do
      lift $ writeCachedParserRef ref cp { cachedParserIsEmpty = eToSet }
      flagChanged
    else return ()

setParseNullWithChange
  :: (Eq a) 
  => CachedParserRef t a
  -> Set.Set a
  -> ChangeContext ()
setParseNullWithChange ref pnToSet = do
  cp <- lift $ readCachedParserRefCached ref
  if cachedParserParseNull cp /= pnToSet
    then do
      lift $ writeCachedParserRef ref cp { cachedParserParseNull = pnToSet }
      flagChanged
    else return ()

-- Derivative cache manipulation
getCachedDerivative
  :: (Ord t) 
  => t 
  -> CachedParserRef t a 
  -> Context (Maybe (CachedParserRef t a))
getCachedDerivative token parserRef = Map.lookup token <$> cachedDerivatives parserRef

addDerivativeToCache
  :: (Ord t) 
  => t 
  -> CachedParserRef t a 
  -> CachedParserRef t a 
  -> Context ()
addDerivativeToCache token valueRefToAdd parserRef =
  setCachedDerivatives parserRef =<< updated
  where
    updated = Map.insert token valueRefToAdd <$> cachedDerivatives parserRef

-- Initializing
initialize
  :: (Ord a, Typeable a) 
  => CachedParserRef t a 
  -> Context ()
initialize ref = do
  myInit <- isInit ref
  if myInit then return ()
    else do
      setInitialized ref True
      updateChildBasedAttributesWhileChanged ref

updateChildBasedAttributesWhileChanged
  :: (Ord a, Typeable a) 
  => CachedParserRef t a 
  -> Context ()
updateChildBasedAttributesWhileChanged ref = do
  changed <- execChangeContext $ updateChildBasedAttributes ref
  if changed then updateChildBasedAttributesWhileChanged ref else return ()

ifUnseen :: Int -> ChangeContext () -> ChangeContext ()
ifUnseen ident ctxt = do
  seen <- hasSeen ident
  if seen == False 
    then ctxt 
    else return ()

updateChildren :: (Ord a, Typeable a) => CachedParserRef t a -> ChangeContext ()
updateChildren ref = do
  (IDParser parser ident) <- lift $ readCachedParserRefID ref
  ifUnseen ident $ do
    flagSeen ident
    parserChildrenDo parser updateChildBasedAttributes

-- Fixed point algorithm for determining child-based attributes (a little messy)
updateChildBasedAttributes :: forall a t. (Ord a, Typeable a) => CachedParserRef t a -> ChangeContext ()
updateChildBasedAttributes ref = do
  updateChildren ref
  parser <- lift $ idParserParser <$> readCachedParserRefID ref
  updateCBA' parser
  where
    updateCBA' :: Parser t a -> ChangeContext ()
    updateCBA' (Terminal _) = return ()
    updateCBA' Empty = return ()
    updateCBA' (Repetition _ _ _ _ _ _) = return ()

    updateCBA' (Epsilon nullMatches) = do
      let parseNullToSet = Set.fromList nullMatches
      setParseNullWithChange ref parseNullToSet

    updateCBA' (Con first second) = do
      parseNullToSet <- lift $ return combineParses <*> parseNull first <*> parseNull second
      setParseNullWithChange ref parseNullToSet
      emptyToSet <- lift $ return (||) <*> isEmpty first <*> isEmpty second
      setEmptyWithChange ref emptyToSet
      nullableToSet <- lift $ return (&&) <*> return (emptyToSet == False)
                                          <*> (return (&&) <*> isNullable first
                                                           <*> isNullable second)
      setNullableWithChange ref nullableToSet
      where
        combineParses :: (Ord a', Ord b') => Set.Set a' -> Set.Set b' -> Set.Set (a', b')
        combineParses p1 p2 = Set.fromList [ (x,y)
                                           | x <- Set.toList p1
                                           , y <- Set.toList p2
                                           ]

    updateCBA' (Alt lhs rhs) = do
      parseNullToSet <- lift $ return Set.union <*> parseNull lhs <*> parseNull rhs
      setParseNullWithChange ref parseNullToSet
      emptyToSet <- lift $ return (&&) <*> isEmpty lhs <*> isEmpty rhs
      setEmptyWithChange ref emptyToSet
      nullableToSet <- lift $ return (&&) <*> return (emptyToSet == False)
                                          <*> (return (||) <*> isNullable lhs
                                                           <*> isNullable rhs)
      setNullableWithChange ref nullableToSet

    updateCBA' (Reduction p f _) = do
      parseNullToSet <- lift $ Set.map f <$> parseNull p
      setParseNullWithChange ref parseNullToSet
      emptyToSet <- lift $ isEmpty p
      setEmptyWithChange ref emptyToSet
      nullableToSet <- lift $ isNullable p
      setNullableWithChange ref nullableToSet

-- Building Parsers
mkCached :: (Ord a) => Parser t a -> Context (CachedParserRef t a)
mkCached p = do
  n <- nextID
  let cached = initialOf $ IDParser p n
  lift $ liftM CachedParserRef (newIORef cached)

initialOf :: (Ord a) => IDParser t a -> CachedParser t a
initialOf idp = case idParserParser idp of
  (Terminal _) -> base
  (Con _ _) -> base
  (Alt _ _) -> base
  (Epsilon _) -> base { cachedParserIsNullable = True }
  Empty -> base { cachedParserIsEmpty = True }
  (Reduction _ _ _) -> base
  (Repetition _ init' init'f _ collected _) ->
    base 
    { cachedParserParseNull  = figure init' init'f collected
    , cachedParserIsNullable = True
    }
  where 
    base = 
      CachedParser
      { cachedParserIDParser          = idp
      , cachedParserIsInit            = False
      , cachedParserIsNullable        = False
      , cachedParserIsEmpty           = False
      , cachedParserParseNull         = Set.empty
      , cachedParserCachedDerivatives = Map.empty
      }
    figure :: forall a b. (Ord b) => b -> (a -> b -> b) -> [[a]] -> Set.Set b
    figure init'' init''f collected' =
      Set.fromList $ map convert reversed
      where
        convert :: [a] -> b
        convert = foldr init''f init''
        reversed :: [[a]]
        reversed = map id collected'

-- Parser Constructors
term :: (Ord t) => (t -> Bool) -> Context (CachedParserRef t t)
term = mkCached . Terminal
termEq
  :: (Ord t)
  => t 
  -> Context (CachedParserRef t t)
termEq = term . (==)
(.~)
  :: (Show a, Ord a, Typeable a, Show b, Ord b, Typeable b)
  => CachedParserRef t a
  -> CachedParserRef t b
  -> Context (CachedParserRef t (a, b))
(.~) x y = mkCached $ Con x y
(~>)
  :: (Show a, Ord a, Typeable a, Show b, Ord b, Typeable b)
  => CachedParserRef t a
  -> Context (CachedParserRef t b)
  -> Context (CachedParserRef t (a, b))
(~>) x ym = (x .~) =<< ym
(<~)
  :: (Show a, Ord a, Typeable a, Show b, Ord b, Typeable b)
  => Context (CachedParserRef t a)
  -> CachedParserRef t b
  -> Context (CachedParserRef t (a, b))
(<~) xm y = (.~ y) =<< xm
(<~>)
  :: (Show a, Ord a, Typeable a, Show b, Ord b, Typeable b)
  => Context (CachedParserRef t a)
  -> Context (CachedParserRef t b)
  -> Context (CachedParserRef t (a, b))
(<~>) xm ym = uncurry (.~) =<< liftM2 (,) xm ym

(.|)
  :: (Ord a)
  => CachedParserRef t a
  -> CachedParserRef t a
  -> Context (CachedParserRef t a)
(.|) x y = mkCached $ Alt x y
(|>)
  :: (Ord a)
  => CachedParserRef t a
  -> Context (CachedParserRef t a)
  -> Context (CachedParserRef t a)
(|>) x ym = (x .|) =<< ym
(<|)
  :: (Ord a)
  => Context (CachedParserRef t a)
  -> CachedParserRef t a
  -> Context (CachedParserRef t a)
(<|) xm y = (.| y) =<< xm
(<|>)
  :: (Ord a)
  => Context (CachedParserRef t a)
  -> Context (CachedParserRef t a)
  -> Context (CachedParserRef t a)
(<|>) xm ym = uncurry (.|) =<< liftM2 (,) xm ym

infixl 3 .~, ~>, <~, <~>
infixl 1 .|, |>, <|, <|>

eps :: (Ord a) => [a] -> Context (CachedParserRef t a)
eps = mkCached . Epsilon
emp :: (Ord a) => Context (CachedParserRef t a)
emp = mkCached Empty
(==>|)
  :: (Show a, Ord a, Typeable a, Ord b)
  => Context (CachedParserRef t a)
  -> (a -> b)
  -> Int
  -> Context (CachedParserRef t b)
(==>|) pm f fid =
  mkCached =<< return Reduction <*> pm <*> return f <*> return fid
(==>)
  :: (Show a, Ord a, Typeable a, Ord b)
  => Context (CachedParserRef t a)
  -> (a -> b)
  -> Context (CachedParserRef t b)
(==>) pm f = (==>|) pm f =<< nextID

rep
  :: (Ord a)
  => CachedParserRef t a 
  -> a 
  -> (a -> a -> a) 
  -> Context (CachedParserRef t a)
rep p init' tallyf = mdo
  n <- nextID
  result <- repLoaded p init' tallyf n [[]] result
  return result

repLoaded
  :: (Ord a)
  => CachedParserRef t a
  -> a
  -> (a -> a -> a)
  -> Int
  -> [[a]]
  -> CachedParserRef t a
  -> Context (CachedParserRef t a)
repLoaded p init' tallyf id' progress clean = mkCached $ Repetition p init' tallyf id' progress clean

infix 2 ==>, ==>|

-- Weeding is intended to keep the data structure size of a parser small after
-- computing a derivative.
weed :: (Ord a, Typeable a, Typeable t) => CachedParserRef t a -> Context ()
weed ref = do
  execChangeContext $ weed' ref
  return ()

weed' :: forall a t. (Ord a, Typeable a, Typeable t) => CachedParserRef t a -> ChangeContext ()
weed' ref = do
  empty' <- lift $ isEmpty ref
  if empty'
    then do
      emp' <- lift $ readCachedParserRefCached =<< emp
      lift $ writeCachedParserRef ref emp'
    else do
      (IDParser parser ident) <- lift $ readCachedParserRefID ref
      ifUnseen ident $ do
        flagSeen ident
        parserChildrenDo parser weed'
        handleSpecial parser
  where
    handleSpecial :: Parser t a -> ChangeContext ()
    handleSpecial (Alt lhs rhs) = do
      lhsEmpty <- lift $ isEmpty lhs
      rhsEmpty <- lift $ isEmpty rhs
      case (lhsEmpty, rhsEmpty) of
        (True, False) ->
          lift $ writeCachedParserRef ref =<< readCachedParserRefCached rhs
        (False, True) ->
          lift $ writeCachedParserRef ref =<< readCachedParserRefCached lhs
        (False, False) -> return ()
        (True, True) -> error "case should never happen"
    handleSpecial (Reduction p_r _ fid) = do -- f is :: (a, a) -> a
      p <- lift $ readCachedParserRef p_r
      case p of
        (Con pFir_r pSec_r) -> do
          pSec :: Parser t a2 <- lift $ readCachedParserRef pSec_r
          pFirNullable <- lift $ isNullable pFir_r
          case (pSec, pFirNullable) of
            (Repetition _ _ _ testfid _ _, True) -- f' is :: a -> a -> a (same id as f above)
              | fid == testfid -> do
                let
                  (Just pFir_rC) = cast pFir_r :: Maybe (CachedParserRef t a)
                  (Just pSecC)   = cast pSec   :: Maybe (Parser t a)
                  (Repetition repP repInit repF repFid repProg repClean) = pSecC
                pFirParseNull :: [a]   <- lift $ Set.toList <$> parseNull pFir_rC
                pRepProgress  :: [[a]] <- return repProg
                newProgress   :: [[a]] <- return $
                  [ x : xs | x <- pFirParseNull, xs <- pRepProgress ]
                let finalP = repLoaded repP repInit repF repFid newProgress repClean
                lift $ writeCachedParserRef ref =<< readCachedParserRefCached =<< finalP
            _ -> return ()
        _ -> return ()
    handleSpecial _ = return ()
  
-- Derivative
der
  :: (Show a, Ord a, Typeable a, Ord t, Typeable t)
  => t 
  -> CachedParserRef t a 
  -> Context (CachedParserRef t a)
der token ref = do
  d <- derInner token ref
  weed d
  return d

derInner
  :: (Show a, Ord a, Typeable a, Ord t, Typeable t)
  => t 
  -> CachedParserRef t a 
  -> Context (CachedParserRef t a)
derInner token pCachedRef = do
  -- if empty, just return empty
  pEmpty <- isEmpty pCachedRef
  if pEmpty
    then emp
    else do
      -- try cached
      maybeCached <- getCachedDerivative token pCachedRef
      case maybeCached of
        -- cached result exists, use it
        Just resultRef -> return resultRef
        -- cached result doesn't exist, compute it
        Nothing -> mdo
          -- Using `myDer' before its monadic binding is what ties the knot and
          -- allows nested calls to derInner to access this parser's derivative
          -- in its cache
          addDerivativeToCache token myDer pCachedRef
          p <- readCachedParserRef pCachedRef
          myDer <- derRaw token p pCachedRef
          return myDer

derRaw
  :: (Show a, Ord a, Typeable a, Ord t, Typeable t)
  => t 
  -> Parser t a 
  -> CachedParserRef t a 
  -> Context (CachedParserRef t a)
derRaw token (Terminal t1test) _ 
  | t1test token = eps $ [token]
  | otherwise = emp
derRaw token (Con first second) _ = do
  isFirstNullable <- isNullable first
  if isFirstNullable
    then do
      emptyFirstParses <- parse first []
      derInner token first <~> return second 
        <|> 
        eps [a | (a,_) <- emptyFirstParses] <~> derInner token second
    else derInner token first <~> return second
derRaw token (Alt lhs rhs) _ = do
  lhsIsEmpty <- isEmpty lhs
  rhsIsEmpty <- isEmpty rhs
  case (lhsIsEmpty, rhsIsEmpty) of
    (True, _) -> derInner token rhs
    (_, True) -> derInner token lhs
    _         -> derInner token lhs <|> derInner token rhs
derRaw _ Empty _ = emp
derRaw _ (Epsilon _) _ = emp
derRaw token (Reduction parser f fid) _ = derInner token parser ==>| f $ fid
derRaw token (Repetition parser _ tallyf' fid progress clean) ref
  | progress /= [[]] = do
    myNullParses <- Set.toList <$> parseNull ref
    ((eps myNullParses <~>
      (derInner token parser <~ clean ==>| uncurry tallyf' $ fid))
        ==>| uncurry tallyf' $ fid)
  | otherwise = do
    derInner token parser <~ ref ==>| uncurry tallyf' $ fid

-- Parsing
parse
  :: (Show a, Ord a, Typeable a, Ord t, Typeable t)
  => CachedParserRef t a 
  -> [t] 
  -> Context [(a, [t])]
parse parserRef input = do
  parser <- readCachedParserRef parserRef
  case parser of
    (Terminal ttest) -> case input of
      [] -> return []
      (x:xs) | ttest x -> return [(x,xs)]
      _ | otherwise -> return []
    (Con _ _) -> doDerivParse parserRef input
    (Alt _ _) -> doDerivParse parserRef input
    (Epsilon nullMatches) -> return [(a, input) | a <- nullMatches]
    Empty -> return []
    (Reduction p f _) -> do
      innerParse <- parse p input
      return [(f a, rest) | (a, rest) <- innerParse]
    (Repetition _ _ _ _ _ _) -> doDerivParse parserRef input

doDerivParse
  :: (Show a, Ord a, Typeable a, Ord t, Typeable t)
  => CachedParserRef t a 
  -> [t] 
  -> Context [(a, [t])]
doDerivParse parserRef [] = do
  nullParses <- parseNull parserRef
  return [(a,[]) | a <- Set.toList nullParses]
doDerivParse parserRef (x:xs) = do
  parseFullOnEmpty <- parseFull parserRef []
  derivParse <- der x parserRef >>= flip parse xs
  return $ combineEven derivParse [(a,(x:xs)) | a <- parseFullOnEmpty]

parseFull
  :: (Show a, Ord a, Typeable a, Ord t, Typeable t)
  => CachedParserRef t a 
  -> [t] 
  -> Context [a]
parseFull parserRef input = do
  parser <- readCachedParserRef parserRef
  case parser of
    (Terminal _) -> doDerivParseFull parserRef input
    (Con _ _) -> doDerivParseFull parserRef input
    (Alt _ _) -> doDerivParseFull parserRef input
    (Epsilon _) -> doDerivParseFull parserRef input
    Empty -> doDerivParseFull parserRef input
    (Reduction p f _) -> do
      innerParseFull <- parseFull p input
      return [f a | a <- innerParseFull]
    (Repetition _ _ _ _ _ _) -> doDerivParseFull parserRef input

doDerivParseFull
  :: (Show a, Ord a, Typeable a, Ord t, Typeable t) 
  => CachedParserRef t a 
  -> [t] 
  -> Context [a]
doDerivParseFull parserRef [] = liftM Set.toList $ parseNull parserRef
doDerivParseFull parserRef (x:xs) = do
  derived <- der x parserRef
  parseFull derived xs

combineEven:: [a] -> [a] -> [a]
combineEven (x:xs) ys = x:combineOdd xs ys
combineEven xs (y:ys) = y:combineOdd xs ys
combineEven _ _ = []

combineOdd :: [a] -> [a] -> [a]
combineOdd xs (y:ys) = y:combineEven xs ys
combineOdd (x:xs) ys = x:combineEven xs ys
combineOdd _ _ = []

-- A pretty print
showRec :: (Show a) => CachedParserRef t a -> Context String
showRec r = fst <$> showRec' r [] 0
showRec' :: (Show a) => CachedParserRef t a -> [Int] -> Int -> Context (String, [Int])
showRec' cachedRef seen depth = do
  (CachedParser idp i n e pn _) <- readCachedParserRefCached cachedRef

  let initMsg = printf "[init: %s]" (show i)
      nullMsg = printf "[nullable: %s]" (show n)
      emptMsg = printf "[empty: %s]" (show e)
      pnulMsg = printf "[parseNull: %s]" (show pn)

      (IDParser p g) = idp
      idMsg = printf "[id: %s]" (show g)
      seen' = (g:seen)

      cachMsg = intercalate " " [idMsg, initMsg, nullMsg, emptMsg, pnulMsg]

      seenResult = printf "(SEEN %s)" idMsg

      depthBuffer = replicate depth ' '
      nextDepth = depth + 2

  if g `elem` seen then return (seenResult, seen)
    else case p of
      (Terminal _) -> return (printf "(Terminal %s)" cachMsg, seen')
      (Con first second) -> do
        let resultFormat = concat [   "(Con %s\n"
                                  , "%s first:\n"
                                  , "%s  %s\n"
                                  , "%s second:\n"
                                  , "%s  %s"
                                  ]
        (showFirst, seen1) <- showRec' first seen' nextDepth
        (showSecond, seen2) <- showRec' second seen1 nextDepth
        let result = printf resultFormat 
                            cachMsg 
                            depthBuffer depthBuffer showFirst 
                            depthBuffer depthBuffer showSecond
        return (result, seen2)
      (Alt lhs rhs) -> do
        let resultFormat = concat [   "(Alt %s\n"
                                  , "%s lhs:\n"
                                  , "%s  %s\n"
                                  , "%s rhs:\n"
                                  , "%s  %s"
                                  ]
        (showLhs, seen1) <- showRec' lhs seen' nextDepth
        (showRhs, seen2) <- showRec' rhs seen1 nextDepth
        let result = printf resultFormat
                            cachMsg
                            depthBuffer depthBuffer showLhs
                            depthBuffer depthBuffer showRhs
        return (result, seen2)
      (Epsilon nullMatches) -> do
        let result = printf "(Epsilon %s %s)" (show nullMatches) cachMsg
        return (result, seen')
      Empty -> return (printf "(Empty %s)" cachMsg, seen')
      (Reduction rp _ fid) -> do
        let resultFormat = concat [   "(Reduction %s [fid: %s]\n"
                                  , "%s p:\n"
                                  , "%s  %s"
                                  ]
        (showRP, seen1) <- showRec' rp seen' nextDepth
        let result = printf resultFormat
                            cachMsg (show fid)
                            depthBuffer depthBuffer showRP
        return (result, seen1)
      (Repetition rp _ _ fid progress clean) -> do
        let resultFormat = concat [   "(Repetion %s [fid: %s]\n"
                                  , "%s [progress: %s] [clean: %s]\n"
                                  , "%s p:\n"
                                  , "%s  %s"
                                  ]
        (IDParser _ cid) <- readCachedParserRefID clean
        (showRP, seen1) <- showRec' rp seen' nextDepth
        let result = printf resultFormat
                            cachMsg (show fid)
                            depthBuffer (show progress) (show cid)
                            depthBuffer depthBuffer showRP
        return (result, seen1)

parserSize :: CachedParserRef t a -> Context Int
parserSize = (fst <$>) . flip parserSize' []

parserSize' :: CachedParserRef t a -> [Int] -> Context (Int, [Int])
parserSize' ref seen = flip parserSize'' seen =<< readCachedParserRefID ref

parserSize'' :: IDParser t a -> [Int] -> Context (Int, [Int])
parserSize'' (IDParser _ id') seen
  | id' `elem` seen = return (0, seen)
parserSize'' (IDParser (Terminal _) id') seen = return (1, id':seen) 
parserSize'' (IDParser (Epsilon _) id') seen = return (1, id':seen)
parserSize'' (IDParser Empty id') seen = return (1, id':seen)
parserSize'' (IDParser (Con first second) id') seen = do
  (firstSize, newSeen) <- parserSize' first (id':seen)
  (secondSize, newSeen1) <- parserSize' second newSeen
  return (firstSize + secondSize + 1, newSeen1)
parserSize'' (IDParser (Alt lhs rhs) id') seen = do
  (lhsSize, newSeen) <- parserSize' lhs (id':seen)
  (rhsSize, newSeen1) <- parserSize' rhs newSeen
  return (lhsSize + rhsSize + 1, newSeen1)
parserSize'' (IDParser (Reduction p _ _) id') seen = do
  (pSize, newSeen) <- parserSize' p (id':seen)
  return (pSize + 1, newSeen)
parserSize'' (IDParser (Repetition p _ _ _ _ _) id') seen = do
  (pSize, newSeen) <- parserSize' p (id':seen)
  return (pSize + 1, newSeen)
