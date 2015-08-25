{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, RankNTypes, GeneralizedNewtypeDeriving, GADTs, MultiParamTypeClasses, FlexibleInstances #-}

-- Author: David Darais

module DerParser
  ( Context, evalContext, CachedParserRef
  , term, termEq
  , (.~), (~>), (<~), (<~>)
  , (.|), (|>), (<|), (<|>)
  , eps, emp, (==>)
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
-- import Control.Applicative hiding ((<|>))
import Data.IORef
import Data.Typeable
import System.IO.Unsafe

-- ID Generator (Monad Transformer)
newtype IDHandlerT m a =
  IDHandlerT { runIDHandlerT :: StateT Int m a }
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
evalIDT = ($ 0) . evalStateT . runIDHandlerT

-- ChangeCell (Monad Transformer)
newtype ChangeCellT m a =
  ChangeCellT { runChangeCellT :: StateT (Bool, [Int]) m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadFix)

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
execChangeCellT cellT = fst <$> execStateT (runChangeCellT cellT) (False, [])

-- Context and ChangeContext (Monads)
type Context a = IDHandlerT IO a
type ChangeContext a = ChangeCellT (IDHandlerT IO) a

evalContext :: Context a -> IO a
evalContext = evalIDT

unsafeInterleaveContext :: Context a -> Context a
unsafeInterleaveContext c =
  let 
    stateT = runIDHandlerT c
    f = runStateT stateT
  in IDHandlerT $ StateT $ \s' -> unsafeInterleaveIO $ f s'

execChangeContext :: ChangeContext a -> Context Bool
execChangeContext = execChangeCellT

-- Parser: The main parser type. Note recursive structures contain references
-- to children (instead of values.)
--
-- Typing relies heavily on GADTs.
--
data Parser t a where
  Terminal :: (t -> Bool) -> Parser t t
  Epsilon :: [a] -> Parser t a
  Empty :: Parser t a

  Alt
    :: CachedParserRef t a
    -> CachedParserRef t a
    -> Parser t a

  Con
    :: (Show a, Ord a, Typeable a, Show b, Ord b, Typeable b)
    => CachedParserRef t a
    -> CachedParserRef t b
    -> Parser t (a, b)

  Reduction
    :: (Show a, Ord a, Typeable a)
    => CachedParserRef t a
    -> (a -> b)
    -> Int
    -> Parser t b

  deriving (Typeable)

-- IDParser: Wraps around a parser and gives it an ID
data IDParser t a = IDParser (Parser t a) Int

idParserParser :: IDParser t a -> Parser t a
idParserParser (IDParser p _) = p

-- CachedParser: Wraps around an IDParser and caches things.
data CachedParser t a =
  CachedParser
  { cachedParserIDParser :: (IDParser t a)
  , cachedParserIsInit :: Bool
  , cachedParserIsFinalized :: Bool
  , cachedParserIsWeeded :: Bool
  , cachedParserIsNullable :: Bool
  , cachedParserIsEmpty :: Bool
  , cachedParserParseNull :: Set.Set a
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
  -> (forall b. (Ord b, Typeable b) => CachedParserRef t b -> m ())
  -> m ()
parserChildrenDo (Terminal _) _ = return ()
parserChildrenDo (Epsilon _) _ = return ()
parserChildrenDo Empty _ = return ()
parserChildrenDo (Alt lhs rhs) f = f lhs >> f rhs
parserChildrenDo (Con first second) f = f first >> f second
parserChildrenDo (Reduction p _ _) f = f p

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

isFinalized :: CachedParserRef t a -> Context Bool
isFinalized = liftM cachedParserIsFinalized . readCachedParserRefCached

isWeeded :: CachedParserRef t a -> Context Bool
isWeeded = liftM cachedParserIsWeeded . readCachedParserRefCached

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
  when (cachedParserIsInit cp /= iToSet) $
    writeCachedParserRef ref cp { cachedParserIsInit = iToSet }

setFinalized :: CachedParserRef t a -> Bool -> Context ()
setFinalized ref fToSet = do
  cp <- readCachedParserRefCached ref
  when (cachedParserIsFinalized cp /= fToSet) $
    writeCachedParserRef ref cp { cachedParserIsFinalized = fToSet }

setWeeded :: CachedParserRef t a -> Bool -> Context ()
setWeeded ref wToSet = do
  cp <- readCachedParserRefCached ref
  when (cachedParserIsWeeded cp /= wToSet) $
    writeCachedParserRef ref cp { cachedParserIsWeeded = wToSet }

setCachedDerivatives :: (Eq t) => CachedParserRef t a -> Map.Map t (CachedParserRef t a) -> Context ()
setCachedDerivatives ref dToSet = do
  cp <- readCachedParserRefCached ref
  when (cachedParserCachedDerivatives cp /= dToSet) $
    writeCachedParserRef ref cp { cachedParserCachedDerivatives = dToSet }

setNullableWithChange :: CachedParserRef t a -> Bool -> ChangeContext ()
setNullableWithChange ref nToSet = do
  cp <- lift $ readCachedParserRefCached ref
  when (cachedParserIsNullable cp /= nToSet) $ do
    lift $ writeCachedParserRef ref cp { cachedParserIsNullable = nToSet }
    flagChanged

setEmptyWithChange :: CachedParserRef t a -> Bool -> ChangeContext ()
setEmptyWithChange ref eToSet = do
  cp <- lift $ readCachedParserRefCached ref
  when (cachedParserIsEmpty cp /= eToSet) $ do
    lift $ writeCachedParserRef ref cp { cachedParserIsEmpty = eToSet }
    flagChanged

setParseNullWithChange
  :: (Eq a) 
  => CachedParserRef t a
  -> Set.Set a
  -> ChangeContext ()
setParseNullWithChange ref pnToSet = do
  cp <- lift $ readCachedParserRefCached ref
  when (cachedParserParseNull cp /= pnToSet) $ do
    lift $ writeCachedParserRef ref cp { cachedParserParseNull = pnToSet }
    flagChanged

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
  unless myInit $ do
    setInitialized ref True
    updateChildBasedAttributesWhileChanged ref
    setFinalized ref True

updateChildBasedAttributesWhileChanged
  :: (Ord a, Typeable a) 
  => CachedParserRef t a 
  -> Context ()
updateChildBasedAttributesWhileChanged ref = do
  changed <- execChangeContext $ updateChildBasedAttributes ref
  when changed $ updateChildBasedAttributesWhileChanged ref

ifUnseen :: Int -> ChangeContext () -> ChangeContext ()
ifUnseen ident ctxt = do
  seen <- hasSeen ident
  unless seen ctxt

updateChildren :: (Ord a, Typeable a) => CachedParserRef t a -> ChangeContext ()
updateChildren ref = do
  (IDParser parser ident) <- lift $ readCachedParserRefID ref
  ifUnseen ident $ do
    flagSeen ident
    parserChildrenDo parser updateChildBasedAttributes

-- Fixed point algorithm for determining child-based attributes
updateChildBasedAttributes :: forall a t. (Ord a, Typeable a) => CachedParserRef t a -> ChangeContext ()
updateChildBasedAttributes ref = do
  finalized <- lift $ isFinalized ref
  unless finalized $ do
    updateChildren ref
    parser <- lift $ idParserParser <$> readCachedParserRefID ref
    updateCBA' parser
  where
    updateCBA' :: Parser t a -> ChangeContext ()
    updateCBA' (Alt lhs rhs) = do
      parseNullToSet <- lift $ Set.union <$> parseNull lhs <*> parseNull rhs
      setParseNullWithChange ref parseNullToSet
      emptyToSet <- lift $ (&&) <$> isEmpty lhs <*> isEmpty rhs
      setEmptyWithChange ref emptyToSet
      nullableToSet <- lift $ (&& not emptyToSet) <$> 
                              ((||) <$> isNullable lhs <*> isNullable rhs)
      setNullableWithChange ref nullableToSet

    updateCBA' (Con first second) = do
      parseNullToSet <- lift $ combineParses <$> parseNull first <*> parseNull second
      setParseNullWithChange ref parseNullToSet
      emptyToSet <- lift $ (||) <$> isEmpty first <*> isEmpty second
      setEmptyWithChange ref emptyToSet
      nullableToSet <- lift $ (&& not emptyToSet) <$> 
                              ((&&) <$> isNullable first <*> isNullable second)
      setNullableWithChange ref nullableToSet
      where
        combineParses :: (Ord a', Ord b') => Set.Set a' -> Set.Set b' -> Set.Set (a', b')
        combineParses p1 p2 = Set.fromList [ (x,y)
                                           | x <- Set.toList p1
                                           , y <- Set.toList p2
                                           ]

    updateCBA' (Reduction p f _) = do
      parseNullToSet <- lift $ Set.map f <$> parseNull p
      setParseNullWithChange ref parseNullToSet
      emptyToSet <- lift $ isEmpty p
      setEmptyWithChange ref emptyToSet
      nullableToSet <- lift $ isNullable p
      setNullableWithChange ref nullableToSet

    updateCBA' _ = error "should not get here"

-- Building Parsers
mkCached :: (Ord a) => Parser t a -> Context (CachedParserRef t a)
mkCached p = do
  n <- nextID
  let cached = initialOf $ IDParser p n
  lift $ liftM CachedParserRef (newIORef cached)

initialOf :: (Ord a) => IDParser t a -> CachedParser t a
initialOf idp = case idParserParser idp of
  (Terminal _) -> leaf
  (Epsilon np) -> leaf { cachedParserIsNullable = True, cachedParserParseNull = Set.fromList np }
  Empty -> leaf { cachedParserIsEmpty = True }
  (Alt _ _) -> base
  (Con _ _) -> base
  (Reduction _ _ _) -> base
  where 
    leaf =
      CachedParser
      { cachedParserIDParser          = idp
      , cachedParserIsInit            = True
      , cachedParserIsFinalized       = True
      , cachedParserIsWeeded          = True
      , cachedParserIsNullable        = False
      , cachedParserIsEmpty           = False
      , cachedParserParseNull         = Set.empty
      , cachedParserCachedDerivatives = Map.empty
      }

    base = leaf
      { cachedParserIsInit            = False
      , cachedParserIsFinalized       = False
      , cachedParserIsWeeded          = False
      }

-- Parser Constructors
term :: (Ord t) => (t -> Bool) -> Context (CachedParserRef t t)
term = mkCached . Terminal
termEq
  :: (Ord t)
  => t 
  -> Context (CachedParserRef t t)
termEq = term . (==)
eps :: (Ord a) => [a] -> Context (CachedParserRef t a)
eps = mkCached . Epsilon
emp :: (Typeable a, Ord a) => Context (CachedParserRef t a)
emp = do
  result <- mkCached Empty
  initialize result
  return result
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

infixl 3 .~, ~>, <~, <~>
infixl 1 .|, |>, <|, <|>
infix 2 ==>, ==>|

-- Weeding is intended to keep the data structure size of a parser small after
-- computing a derivative.
weed :: (Ord a, Typeable a) => CachedParserRef t a -> Context ()
weed ref = do
  void $ execChangeContext $ weed' ref
  return ()

weed' :: forall a t. (Ord a, Typeable a) => CachedParserRef t a -> ChangeContext ()
weed' ref = do
  weeded <- lift $ isWeeded ref
  unless weeded $ do
    lift $ setWeeded ref True
    empty' <- lift $ isEmpty ref
    if empty'
      then lift $ writeCachedParserRef ref =<< readCachedParserRefCached =<< emp
      else do
        (IDParser parser ident) <- lift $ readCachedParserRefID ref
        ifUnseen ident $ do
          flagSeen ident
          parserChildrenDo parser weed'
  
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
  maybeCached <- getCachedDerivative token pCachedRef
  case maybeCached of
    -- cached result exists, use it
    Just resultRef -> return resultRef
    -- cached result doesn't exist, compute it
    Nothing -> do
      p <- readCachedParserRef pCachedRef
      -- thunk the derivative computation so it can see itself in its cache
      myDer <- unsafeInterleaveContext $ derRaw token p pCachedRef
      addDerivativeToCache token myDer pCachedRef
      return myDer

derRaw
  :: (Show a, Ord a, Typeable a, Ord t, Typeable t)
  => t 
  -> Parser t a 
  -> CachedParserRef t a 
  -> Context (CachedParserRef t a)
derRaw token (Terminal t1test) _ 
  | t1test token = eps [token]
  | otherwise = emp
derRaw token (Con first second) _ = do
  firstNullable <- isNullable first
  firstEmpty <- isEmpty first
  secondEmpty <- isEmpty second
  case (firstNullable, firstEmpty || secondEmpty) of
    (_, True) -> emp
    (True, False) -> do
      emptyFirstParses <- parse first []
      derInner token first <~> return second 
        <|> 
        eps [a | (a,_) <- emptyFirstParses] <~> derInner token second
    (False, False) ->
      derInner token first <~> return second
derRaw token (Alt lhs rhs) _ = do
  lhsIsEmpty <- isEmpty lhs
  rhsIsEmpty <- isEmpty rhs
  case (lhsIsEmpty, rhsIsEmpty) of
    (True, True)   -> emp
    (False, True)  -> derInner token lhs
    (True, False)  -> derInner token rhs
    (False, False) -> derInner token lhs <|> derInner token rhs
derRaw _ Empty _ = emp
derRaw _ (Epsilon _) _ = emp
derRaw token (Reduction parser f fid) _ = derInner token parser ==>| f $ fid

-- Parsing
parse
  :: (Show a, Ord a, Typeable a, Ord t, Typeable t)
  => CachedParserRef t a 
  -> [t] 
  -> Context [(a, [t])]
parse parserRef [] = do
  nullParses <- parseNull parserRef
  return [(a,[]) | a <- Set.toList nullParses]
parse parserRef input@(x:xs) = do
  parseFullOnEmpty <- parseFull parserRef []
  let emptyParse = [(a, input) | a <- parseFullOnEmpty]
  derivParse <- der x parserRef >>= flip parse xs
  return $ combineEven derivParse emptyParse

parseFull
  :: (Show a, Ord a, Typeable a, Ord t, Typeable t)
  => CachedParserRef t a 
  -> [t] 
  -> Context [a]
parseFull parserRef [] = liftM Set.toList $ parseNull parserRef
parseFull parserRef (x:xs) = flip parseFull xs =<< der x parserRef

combineEven:: [a] -> [a] -> [a]
combineEven (x:xs) ys = x:combineOdd xs ys
combineEven [] ys = ys

combineOdd :: [a] -> [a] -> [a]
combineOdd xs (y:ys) = y:combineEven xs ys
combineOdd xs [] = xs

-- A pretty print
showRec :: (Show a, Show t) => CachedParserRef t a -> Context String
showRec r = fst <$> showRec' r [] 0
showRec' :: (Show a, Show t) => CachedParserRef t a -> [Int] -> Int -> Context (String, [Int])
showRec' cachedRef seen depth = do
  (CachedParser idp i f w n e pn derCache) <- readCachedParserRefCached cachedRef

  let initMsg = printf "[init: %s]" (show i)
      finlMsg = printf "[final: %s]" (show f)
      weedMsg = printf "[weeded: %s]" (show w)
      nullMsg = printf "[nullable: %s]" (show n)
      emptMsg = printf "[empty: %s]" (show e)
      pnulMsg = printf "[parseNull: %s]" (show pn)
      derCMsg = printf "[cachedFor: %s]" (show $ Map.keys derCache)

      (IDParser p g) = idp
      idMsg = printf "[id: %s]" (show g)
      seen' = (g:seen)

      cachMsg = intercalate " " [idMsg, initMsg, finlMsg, weedMsg, nullMsg, emptMsg, pnulMsg, derCMsg]

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
