{-# LANGUAGE TemplateHaskell, RankNTypes, GeneralizedNewtypeDeriving, GADTs, MultiParamTypeClasses, FlexibleInstances #-}

-- Author: David Darais

module DerParser.Base
  ( Context, runContext, CachedParserRef
  , term, termEq
  , (<:~:>), (<:~>), (<~:>), (<~>)
  , (<:|:>), (<:|>), (<|:>), (<|>)
  , eps, emp, (==>)
  , parse, parseFull
  , isEmpty, der
  , dummyRef, link
  -- , demo1, demo2, demo3, demo4, demo5
  -- , niceFormatDemo
  ) where

import Text.Printf
import Control.Monad.State
import Data.STRef.Strict
import Control.Monad.ST.Strict
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List

-- ID Generator (Monad Transformer)
newtype IDHandlerT m a =
  IDHandlerT { unwrapIDHandlerT :: StateT Int m a }
  deriving (Monad, MonadTrans, MonadFix)

nextID :: (Monad m) => IDHandlerT m Int
nextID = IDHandlerT $ do
  n <- get
  put $ n + 1
  return n

evalIDT :: (Monad m) => IDHandlerT m a -> m a
evalIDT = ($ 0) . evalStateT . unwrapIDHandlerT

-- ChangeCell (Monad Transformer)
newtype ChangeCellT m a =
  ChangeCellT { unwrapChangeCellT :: StateT (Bool, [Int]) m a }
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

execChangeCellT :: (Monad m) => ChangeCellT m a -> m Bool
execChangeCellT cellt = liftM fst $ execStateT (unwrapChangeCellT cellt) (False, [])

-- Context and ChangeContext (Monads)
type Context s a = IDHandlerT (ST s) a
type ChangeContext s a = ChangeCellT (IDHandlerT (ST s)) a

runContext :: (forall s. Context s a) -> a
runContext context = runST $ evalIDT context

execChangeContext :: ChangeContext s a -> Context s Bool
execChangeContext = execChangeCellT

-- Parser: The main parser type. Note contains references to children. This
-- relies heavily on GADTs.
data Parser s t a where
  Terminal  :: (t -> Bool) -> Parser s t t

  Con       :: (Show a', Show b', Ord a', Ord b')
            => CachedParserRef s t a'
            -> CachedParserRef s t b'
            -> Parser s t (a', b')

  Alt       :: CachedParserRef s t a
            -> CachedParserRef s t a
            -> Parser s t a

  Epsilon   :: [a] -> Parser s t a

  Empty     :: Parser s t a

  Reduction :: (Ord a', Show a')
            => (a' -> b')
            -> CachedParserRef s t a'
            -> Parser s t b'

-- IDParser: Wraps around a parser and gives it an ID
data IDParser s t a = IDParser (Parser s t a) Int

idParserParser :: IDParser s t a -> Parser s t a
idParserParser (IDParser p _) = p

-- CachedParser: Wraps around an IDParser and caches things.
data CachedParser s t a =
  CachedParser
    { cachedParserIDParser :: (IDParser s t a)
    , cachedParserIsInit :: Bool
    , cachedParserIsNullable :: Bool
    , cachedParserIsEmpty :: Bool
    , cachedParserParseNull :: Set.Set a
    , cachedParserCachedDerivatives :: Map.Map t (CachedParserRef s t a)
    }

-- CachedParserRef: A wrapper around a reference to a CachedParser.  This
-- newtype is necessary in order to typeclass with partially applied type args.
newtype CachedParserRef s t a =
  CachedParserRef { getCachedRef :: STRef s (CachedParser s t a) }

-- Helpers for tying knots
dummyRef :: Context s (CachedParserRef s t a)
dummyRef = lift $ liftM CachedParserRef $ newSTRef (error "access to dummy reference")

link :: CachedParserRef s t a -> CachedParserRef s t a -> Context s ()
link destination source = writeCachedParserRef destination =<< readCachedParserRefCached source

-- Direct reading and writing reference
writeCachedParserRef :: CachedParserRef s t a -> CachedParser s t a -> Context s ()
writeCachedParserRef ref parser = lift $ writeSTRef (getCachedRef ref) parser

readCachedParserRefCached :: CachedParserRef s t a -> Context s (CachedParser s t a)
readCachedParserRefCached ref = lift $ readSTRef $ getCachedRef ref

readCachedParserRefID :: CachedParserRef s t a -> Context s (IDParser s t a)
readCachedParserRefID ref = liftM cachedParserIDParser $ readCachedParserRefCached ref

readCachedParserRef :: CachedParserRef s t a -> Context s (Parser s t a)
readCachedParserRef ref = liftM idParserParser $ readCachedParserRefID ref

-- Reference accessors
isInit :: CachedParserRef s t a -> Context s Bool
isInit = liftM cachedParserIsInit . readCachedParserRefCached

isNullable :: (Ord a) => CachedParserRef s t a -> Context s Bool
isNullable ref = do
  empty <- isEmpty ref
  if empty then return False
    else do
      initialize ref
      liftM cachedParserIsNullable $ readCachedParserRefCached ref

isEmpty :: (Ord a) => CachedParserRef s t a -> Context s Bool
isEmpty ref = do
  initialize ref 
  liftM cachedParserIsEmpty $ readCachedParserRefCached ref

parseNull :: (Ord a) => CachedParserRef s t a -> Context s (Set.Set a)
parseNull ref = do
  empty <- isEmpty ref
  if empty then return Set.empty
    else do
      initialize ref
      liftM cachedParserParseNull $ readCachedParserRefCached ref

cachedDerivatives :: CachedParserRef s t a -> Context s (Map.Map t (CachedParserRef s t a))
cachedDerivatives = liftM cachedParserCachedDerivatives . readCachedParserRefCached

-- Reference mutators
setInitialized :: CachedParserRef s t a -> Bool -> Context s ()
setInitialized ref iToSet = do
  (CachedParser p _ n e pn d) <- readCachedParserRefCached ref
  writeCachedParserRef ref (CachedParser p iToSet n e pn d)

setNullableWithChange :: CachedParserRef s t a -> Bool -> ChangeContext s ()
setNullableWithChange ref nToSet = do
  (CachedParser p i n e pn d) <- lift $ readCachedParserRefCached ref
  if (nToSet /= n)
    then do
      lift $ writeCachedParserRef ref (CachedParser p i nToSet e pn d)
      flagChanged
    else return ()

setEmptyWithChange :: CachedParserRef s t a -> Bool -> ChangeContext s ()
setEmptyWithChange ref eToSet = do
  (CachedParser p i n e pn d) <- lift $ readCachedParserRefCached ref
  if (eToSet /= e)
    then do
      lift $ writeCachedParserRef ref (CachedParser p i n eToSet pn d)
      flagChanged
    else return ()

setParseNullWithChange :: (Eq a) => CachedParserRef s t a -> Set.Set a -> ChangeContext s ()
setParseNullWithChange ref pnToSet = do
  (CachedParser p i n e pn d) <- lift $ readCachedParserRefCached ref
  if (pnToSet /= pn)
    then do
      lift $ writeCachedParserRef ref (CachedParser p i n e pnToSet d)
      flagChanged
    else return ()

setCachedDerivatives :: CachedParserRef s t a -> Map.Map t (CachedParserRef s t a) -> Context s ()
setCachedDerivatives ref dToSet = do
  (CachedParser p i n e pn _) <- readCachedParserRefCached ref
  writeCachedParserRef ref (CachedParser p i n e pn dToSet)

-- Derivative cache manipulation
getCachedDerivative :: (Ord t) => t -> CachedParserRef s t a -> Context s (Maybe (CachedParserRef s t a))
getCachedDerivative token parserRef =
  liftM (Map.lookup token) $ cachedDerivatives parserRef

addDerivativeToCache :: (Ord t) => t -> CachedParserRef s t a -> CachedParserRef s t a -> Context s ()
addDerivativeToCache token valueRefToAdd parserRef =
  setCachedDerivatives parserRef . Map.insert token valueRefToAdd =<< cachedDerivatives parserRef

-- Initializing
initialize :: (Ord a) => CachedParserRef s t a -> Context s ()
initialize ref = do
  myInit <- isInit ref
  if myInit then return ()
    else do
      setInitialized ref True
      updateChildBasedAttributesWhileChanged ref

updateChildBasedAttributesWhileChanged :: (Ord a) => CachedParserRef s t a -> Context s ()
updateChildBasedAttributesWhileChanged ref = do
  changed <- execChangeContext $ updateChildBasedAttributes ref
  if changed then updateChildBasedAttributesWhileChanged ref else return ()

ifUnseen :: Int -> ChangeContext s () -> ChangeContext s ()
ifUnseen ident ctxt = do
  seen <- hasSeen ident
  if seen == False then ctxt else return ()

updateChildBasedAttributes :: (Ord a) => CachedParserRef s t a -> ChangeContext s ()
updateChildBasedAttributes ref = do
  (IDParser parser ident) <- lift $ readCachedParserRefID ref
  case parser of
    (Terminal _) -> return ()

    (Con first second) -> do
      ifUnseen ident $ do
        flagSeen ident
        updateChildBasedAttributes first
        updateChildBasedAttributes second
        lift $ setInitialized ref True

      firstParseNull <- lift $ parseNull first
      secondParseNull <- lift $ parseNull second
      let parseNullToSet = Set.fromList [(x,y) | x <- Set.toList firstParseNull, y <- Set.toList secondParseNull]
      setParseNullWithChange ref parseNullToSet

      firstIsEmpty <- lift $ isEmpty first
      secondIsEmpty <- lift $ isEmpty second
      let emptyToSet = firstIsEmpty || secondIsEmpty
      setEmptyWithChange ref emptyToSet

      firstIsNullable <- lift $ isNullable first
      secondIsNullable <- lift $ isNullable second
      let nullableToSet = (emptyToSet == False) && (firstIsNullable && secondIsNullable)
      setNullableWithChange ref nullableToSet

    (Alt lhs rhs) -> do
      ifUnseen ident $ do
        flagSeen ident
        updateChildBasedAttributes lhs
        updateChildBasedAttributes rhs
        lift $ setInitialized ref True

      lhsParseNull <- lift $ parseNull lhs
      rhsParseNull <- lift $ parseNull rhs
      let parseNullToSet = lhsParseNull `Set.union` rhsParseNull
      setParseNullWithChange ref parseNullToSet

      lhsIsEmpty <- lift $ isEmpty lhs
      rhsIsEmpty <- lift $ isEmpty rhs
      let emptyToSet = lhsIsEmpty && rhsIsEmpty
      setEmptyWithChange ref emptyToSet

      lhsIsNullable <- lift $ isNullable lhs
      rhsIsNullable <- lift $ isNullable rhs
      let nullableToSet = (emptyToSet == False) && (lhsIsNullable || rhsIsNullable)
      setNullableWithChange ref nullableToSet

    (Epsilon nullMatches) -> do
      let parseNullToSet = Set.fromList nullMatches
      setParseNullWithChange ref parseNullToSet

    Empty -> return ()

    (Reduction f p) -> do
      ifUnseen ident $ do
        flagSeen ident
        updateChildBasedAttributes p
        lift $ setInitialized ref True

      parseNullToSet <- lift $ liftM (Set.map f) $ parseNull p
      setParseNullWithChange ref parseNullToSet

      emptyToSet <- lift $ isEmpty p
      setEmptyWithChange ref emptyToSet

      nullableToSet <- lift $ isNullable p
      setNullableWithChange ref nullableToSet

-- Building Parsers
mkCached :: Parser s t a -> Context s (CachedParserRef s t a)
mkCached p = do
  n <- nextID
  let cached = initialOf $ IDParser p n
  lift $ liftM CachedParserRef (newSTRef cached)

initialOf :: IDParser s t a -> CachedParser s t a
initialOf idp = case idParserParser idp of
  (Terminal _) -> CachedParser idp False False False Set.empty Map.empty
  (Con _ _) -> CachedParser idp False False False Set.empty Map.empty
  (Alt _ _) -> CachedParser idp False False False Set.empty Map.empty
  (Epsilon _) -> CachedParser idp False True False Set.empty Map.empty
  Empty -> CachedParser idp False False True Set.empty Map.empty
  (Reduction _ _) -> CachedParser idp False False False Set.empty Map.empty

-- Parser Constructors
term :: (t -> Bool) -> Context s (CachedParserRef s t t)
term = mkCached . Terminal
termEq :: (Eq t) => t -> Context s (CachedParserRef s t t)
termEq = term . (==)
(<:~:>) :: (Show a, Ord a, Show b, Ord b)
        => CachedParserRef s t a
        -> CachedParserRef s t b
        -> Context s (CachedParserRef s t (a, b))
(<:~:>) x y = mkCached $ Con x y
(<:~>) :: (Show a, Ord a, Show b, Ord b)
       => CachedParserRef s t a
       -> Context s (CachedParserRef s t b)
       -> Context s (CachedParserRef s t (a, b))
(<:~>) x ym = (x <:~:>) =<< ym
(<~:>) :: (Show a, Ord a, Show b, Ord b)
       => Context s (CachedParserRef s t a)
       -> CachedParserRef s t b
       -> Context s (CachedParserRef s t (a, b))
(<~:>) xm y = (<:~:> y) =<< xm
(<~>) :: (Show a, Ord a, Show b, Ord b)
      => Context s (CachedParserRef s t a)
      -> Context s (CachedParserRef s t b)
      -> Context s (CachedParserRef s t (a, b))
(<~>) xm ym = uncurry (<:~:>) =<< liftM2 (,) xm ym

(<:|:>) :: CachedParserRef s t a
        -> CachedParserRef s t a
        -> Context s (CachedParserRef s t a)
(<:|:>) x y = mkCached $ Alt x y
(<:|>) :: CachedParserRef s t a
       -> Context s (CachedParserRef s t a)
       -> Context s (CachedParserRef s t a)
(<:|>) x ym = (x <:|:>) =<< ym
(<|:>) :: Context s (CachedParserRef s t a)
       -> CachedParserRef s t a
       -> Context s (CachedParserRef s t a)
(<|:>) xm y = (<:|:> y) =<< xm
(<|>) :: Context s (CachedParserRef s t a)
      -> Context s (CachedParserRef s t a)
      -> Context s (CachedParserRef s t a)
(<|>) xm ym = uncurry (<:|:>) =<< liftM2 (,) xm ym

infixr 3 <:~:>, <:~>, <~:>, <~>
infixr 1 <:|:>, <:|>, <|:>, <|>

eps :: [a] -> Context s (CachedParserRef s t a)
eps = mkCached . Epsilon
emp :: Context s (CachedParserRef s t a)
emp = mkCached Empty
(==>) :: (Show a, Ord a)
      => Context s (CachedParserRef s t a)
      -> (a -> b)
      -> Context s (CachedParserRef s t b)
(==>) pm f = mkCached . Reduction f =<< pm

infix 2 ==>

-- Derivative
der :: (Ord t, Ord a) => t -> CachedParserRef s t a -> Context s (CachedParserRef s t a)
der token pCachedRef = do
  -- if empty, just return empty
  pEmpty <- isEmpty pCachedRef
  if pEmpty
    then emp
    else do
      -- try cached
      maybeCached <- getCachedDerivative token pCachedRef
      case maybeCached of
        Just resultRef -> return resultRef
        Nothing -> do
          -- set dummy in cache
          myDer <- dummyRef
          addDerivativeToCache token myDer pCachedRef
          -- do derivative of inner parser
          p <- readCachedParserRef pCachedRef
          myDer' <- derRaw token p
          -- link result
          link myDer myDer'
          return myDer

derRaw :: (Ord t, Ord a) => t -> Parser s t a -> Context s (CachedParserRef s t a)
derRaw token (Terminal t1test) | t1test token = eps $ [token]
derRaw _ (Terminal _) | otherwise = emp
derRaw token (Con first second) = do
  isFirstNullable <- isNullable first
  if isFirstNullable
    then do
      { emptyFirstParses <- parse first []
      ; der token first <~> return second 
        <|> 
        eps [a | (a,_) <- emptyFirstParses] <~> der token second
      }
    else der token first <~> return second
derRaw token (Alt lhs rhs) = do
  lhsIsEmpty <- isEmpty lhs
  rhsIsEmpty <- isEmpty rhs
  case (lhsIsEmpty, rhsIsEmpty) of
    (True, _) -> der token rhs
    (_, True) -> der token lhs
    _         -> der token lhs <|> der token rhs
derRaw _ Empty = emp
derRaw _ (Epsilon _) = emp
derRaw token (Reduction f parser) = der token parser ==> f

parse :: (Ord t, Ord a) => CachedParserRef s t a -> [t] -> Context s [(a, [t])]
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
    (Reduction f p) -> do
      innerParse <- parse p input
      return [(f a, rest) | (a, rest) <- innerParse]

doDerivParse :: (Ord a, Ord t) => CachedParserRef s t a -> [t] -> Context s [(a, [t])]
doDerivParse parserRef [] = do
  nullParses <- parseNull parserRef
  return [(a,[]) | a <- Set.toList nullParses]
doDerivParse parserRef (x:xs) = do
  parseFullOnEmpty <- parseFull parserRef []
  derivParse <- der x parserRef >>= flip parse xs
  return $ combineEven derivParse [(a,(x:xs)) | a <- parseFullOnEmpty]

-- Parsing
parseFull :: (Ord a, Ord t) => CachedParserRef s t a -> [t] -> Context s [a]
parseFull parserRef input = do
    parser <- readCachedParserRef parserRef
    case parser of
      (Terminal _) -> doDerivParseFull parserRef input
      (Con _ _) -> doDerivParseFull parserRef input
      (Alt _ _) -> doDerivParseFull parserRef input
      (Epsilon _) -> doDerivParseFull parserRef input
      Empty -> doDerivParseFull parserRef input
      (Reduction f p) -> do
        innerParseFull <- parseFull p input
        return [f a | a <- innerParseFull]

doDerivParseFull :: (Ord a, Ord t) => CachedParserRef s t a -> [t] -> Context s [a]
doDerivParseFull parserRef [] = liftM Set.toList $ parseNull parserRef
doDerivParseFull parserRef (x:xs) = do
  derived <- der x parserRef
  parseFull derived xs

combineEven :: [a] -> [a] -> [a]
combineEven (x:xs) ys = x:combineOdd xs ys
combineEven xs (y:ys) = y:combineOdd xs ys
combineEven _ _ = []

combineOdd :: [a] -> [a] -> [a]
combineOdd xs (y:ys) = y:combineEven xs ys
combineOdd (x:xs) ys = x:combineEven xs ys
combineOdd _ _ = []

-- Helper for displaying
class ShowRec s a where
  showRec :: a -> [Int] -> Int -> Context s String

instance (Show a) => ShowRec s (Parser s t a) where
  showRec (Terminal _) _ _ = return $ printf "Terminal"
  showRec (Con first second) seen depth = do
    showFirst <- (showRec first seen (depth + 1))
    showSecond <- (showRec second seen (depth + 1))
    return $ printf "Con first:%s second:%s" showFirst showSecond
  showRec (Alt lhs rhs) seen depth = do
    showLHS <- (showRec lhs seen (depth + 1))
    showRHS <- (showRec rhs seen (depth + 1))
    return $ printf "Alt lhs:%s rhs:%s" showLHS showRHS
  showRec (Epsilon nullMatches) _ _ = return $ printf "Epsilon nullParses:%s" (show nullMatches)
  showRec Empty _ _ = return $ "Empty"
  showRec (Reduction _ p) seen depth = do
    showP <- (showRec p seen (depth + 1))
    return $ printf "Reduction p:%s" showP

instance (Show a) => ShowRec s (IDParser s t a) where
  showRec (IDParser _ g) seen _ | elem g seen = return $ show g
  showRec (IDParser p g) seen depth | otherwise = do
    showP <- (showRec p (g:seen) depth)
    return $ printf "%s\n%sid:%s" showP (concat (replicate (depth + 1) "    ")) (show g)

instance (Show a) => ShowRec s (CachedParserRef s t a) where
  showRec cachedRef seen depth = do
    (CachedParser p i n e pn _) <- readCachedParserRefCached cachedRef
    showP <- (showRec p seen depth)
    return $ printf
      "\n%s(%s init: %s nullable: %s empty: %s parseNull: %s)"
      (concat (replicate depth "    "))
      showP (show i) (show n) (show e) (show pn)
