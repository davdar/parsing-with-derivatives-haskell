{-# LANGUAGE RecursiveDo, TemplateHaskell, RankNTypes, GeneralizedNewtypeDeriving, GADTs, MultiParamTypeClasses, FlexibleInstances #-}

-- Author: David Darais

module DerParser.Base
  ( Context, runContext, CachedParserRef
  , term, termEq
  , (.~), (~>), (<~), (<~>)
  , (.|), (|>), (<|), (<|>)
  , eps, emp, (==>), rep
  , parse, parseFull
  , isEmpty, der, derInner
  , dummyRef, link
  , showRec
  , weed
  ) where

import Text.Printf
import Control.Monad.State
import Data.STRef.Strict
import Control.Monad.ST.Strict
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Control.Applicative hiding ((<|>))

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
type Context s a = IDHandlerT (ST s) a
type ChangeContext s a = ChangeCellT (IDHandlerT (ST s)) a

runContext :: (forall s. Context s a) -> a
runContext context = runST $ evalIDT context

execChangeContext :: ChangeContext s a -> Context s Bool
execChangeContext = execChangeCellT

-- Parser: The main parser type. Note recursive structures contain references
-- to children (instead of values.)
--
-- Typing relies heavily on GADTs.
--
-- The (Show a) and (Ord a) class constraints are the only way I could figure
-- out how to get this to compile.
data Parser s t a where
  Terminal   :: (t -> Bool) -> Parser s t t

  Con        :: (Show a', Show b', Ord a', Ord b')
             => CachedParserRef s t a'
             -> CachedParserRef s t b'
             -> Parser s t (a', b')

  Alt        :: CachedParserRef s t a
             -> CachedParserRef s t a
             -> Parser s t a

  Epsilon    :: [a] -> Parser s t a

  Empty      :: Parser s t a

  Reduction  :: (Ord a', Show a')
             => CachedParserRef s t a'
             -> (a' -> b')
             -> Int
             -> Parser s t b'

  Repetition :: (Show a, Ord a)
             => CachedParserRef s t a
             -> b
             -> (a -> b -> b)
             -> Int
             -> Parser s t b

-- IDParser: Wraps around a parser and gives it an ID
data IDParser s t a = IDParser (Parser s t a) Int

idParserParser :: IDParser s t a -> Parser s t a
idParserParser (IDParser p _) = p

-- CachedParser: Wraps around an IDParser and caches things.
data CachedParser s t a =
  CachedParser
  { cachedParserIDParser          :: (IDParser s t a)
  , cachedParserIsInit            :: Bool
  , cachedParserIsNullable        :: Bool
  , cachedParserIsEmpty           :: Bool
  , cachedParserParseNull         :: Set.Set a
  , cachedParserCachedDerivatives :: Map.Map t (CachedParserRef s t a)
  }

-- CachedParserRef: A wrapper around a reference to a CachedParser.  This
-- newtype is necessary in order to typeclass with partially applied type args.
newtype CachedParserRef s t a =
  CachedParserRef 
  { getCachedRef :: STRef s (CachedParser s t a) }
  deriving (Eq)

-- Helpers for tying knots (currently unused thanks to RecursiveDo)
dummyRef :: Context s (CachedParserRef s t a)
dummyRef = lift $ CachedParserRef <$> newSTRef (error "access to dummy reference")

link :: CachedParserRef s t a -> CachedParserRef s t a -> Context s ()
link destination source = writeCachedParserRef destination =<< readCachedParserRefCached source

-- Recursive Operations on parser children
parserChildrenDo :: (Monad m, Ord a) => Parser s t a -> (forall b. (Ord b) => (CachedParserRef s t b -> m ())) -> m ()
parserChildrenDo (Terminal _) _ = return ()
parserChildrenDo (Epsilon _) _ = return ()
parserChildrenDo Empty _ = return ()
parserChildrenDo (Con first second) f = f first >> f second
parserChildrenDo (Alt lhs rhs) f = f lhs >> f rhs
parserChildrenDo (Reduction p _ _) f = f p
parserChildrenDo (Repetition p _ _ _) f = f p

-- Direct reading and writing reference
writeCachedParserRef :: CachedParserRef s t a -> CachedParser s t a -> Context s ()
writeCachedParserRef ref parser = lift $ writeSTRef (getCachedRef ref) parser

readCachedParserRefCached :: CachedParserRef s t a -> Context s (CachedParser s t a)
readCachedParserRefCached ref = lift $ readSTRef $ getCachedRef ref

readCachedParserRefID :: CachedParserRef s t a -> Context s (IDParser s t a)
readCachedParserRefID ref = cachedParserIDParser <$> readCachedParserRefCached ref

readCachedParserRef :: CachedParserRef s t a -> Context s (Parser s t a)
readCachedParserRef ref = idParserParser <$> readCachedParserRefID ref

-- Reference accessors
isInit :: CachedParserRef s t a -> Context s Bool
isInit = liftM cachedParserIsInit . readCachedParserRefCached

cachedDerivatives :: CachedParserRef s t a -> Context s (Map.Map t (CachedParserRef s t a))
cachedDerivatives = liftM cachedParserCachedDerivatives . readCachedParserRefCached

isEmpty :: (Ord a) => CachedParserRef s t a -> Context s Bool
isEmpty ref = do
  initialize ref 
  cachedParserIsEmpty <$> readCachedParserRefCached ref

isNullable :: (Ord a) => CachedParserRef s t a -> Context s Bool
isNullable ref = do
  initialize ref
  cachedParserIsNullable <$> readCachedParserRefCached ref

parseNull :: (Ord a) => CachedParserRef s t a -> Context s (Set.Set a)
parseNull ref = do
  initialize ref
  cachedParserParseNull <$> readCachedParserRefCached ref

-- Reference mutators
setInitialized :: CachedParserRef s t a -> Bool -> Context s ()
setInitialized ref iToSet = do
  cp <- readCachedParserRefCached ref
  if cachedParserIsInit cp /= iToSet
    then writeCachedParserRef ref cp { cachedParserIsInit = iToSet }
    else return ()

setCachedDerivatives :: (Eq t) => CachedParserRef s t a -> Map.Map t (CachedParserRef s t a) -> Context s ()
setCachedDerivatives ref dToSet = do
  cp <- readCachedParserRefCached ref
  if cachedParserCachedDerivatives cp /= dToSet
    then writeCachedParserRef ref cp { cachedParserCachedDerivatives = dToSet }
    else return ()

setNullableWithChange :: CachedParserRef s t a -> Bool -> ChangeContext s ()
setNullableWithChange ref nToSet = do
  cp <- lift $ readCachedParserRefCached ref
  if cachedParserIsNullable cp /= nToSet
    then do
      lift $ writeCachedParserRef ref cp { cachedParserIsNullable = nToSet }
      flagChanged
    else return ()

setEmptyWithChange :: CachedParserRef s t a -> Bool -> ChangeContext s ()
setEmptyWithChange ref eToSet = do
  cp <- lift $ readCachedParserRefCached ref
  if cachedParserIsEmpty cp /= eToSet
    then do
      lift $ writeCachedParserRef ref cp { cachedParserIsEmpty = eToSet }
      flagChanged
    else return ()

setParseNullWithChange :: (Eq a) => CachedParserRef s t a -> Set.Set a -> ChangeContext s ()
setParseNullWithChange ref pnToSet = do
  cp <- lift $ readCachedParserRefCached ref
  if cachedParserParseNull cp /= pnToSet
    then do
      lift $ writeCachedParserRef ref cp { cachedParserParseNull = pnToSet }
      flagChanged
    else return ()

-- Derivative cache manipulation
getCachedDerivative :: (Ord t) => t -> CachedParserRef s t a -> Context s (Maybe (CachedParserRef s t a))
getCachedDerivative token parserRef = Map.lookup token <$> cachedDerivatives parserRef

addDerivativeToCache :: (Ord t) => t -> CachedParserRef s t a -> CachedParserRef s t a -> Context s ()
addDerivativeToCache token valueRefToAdd parserRef =
  setCachedDerivatives parserRef =<< updated
  where
    updated = Map.insert token valueRefToAdd <$> cachedDerivatives parserRef

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
  if seen == False 
    then ctxt 
    else return ()

updateChildren :: (Ord a) => CachedParserRef s t a -> ChangeContext s ()
updateChildren ref = do
  (IDParser parser ident) <- lift $ readCachedParserRefID ref
  ifUnseen ident $ do
    flagSeen ident
    parserChildrenDo parser updateChildBasedAttributes

-- Fixed point algorithm for determining child-based attributes (a little messy)
updateChildBasedAttributes :: (Ord a) => CachedParserRef s t a -> ChangeContext s ()
updateChildBasedAttributes ref = do
  updateChildren ref
  parser <- lift $ idParserParser <$> readCachedParserRefID ref
  updateCBA' parser ref
  where
    updateCBA' :: (Ord a) => Parser s t a -> CachedParserRef s t a -> ChangeContext s ()
    updateCBA' (Terminal _) _ = return ()
    updateCBA' Empty _ = return ()
    updateCBA' (Repetition _ _ _ _) _ = return ()

    updateCBA' (Epsilon nullMatches) ref' = do
      let parseNullToSet = Set.fromList nullMatches
      setParseNullWithChange ref' parseNullToSet

    updateCBA' (Con first second) ref' = do
      parseNullToSet <- lift $ return combineParses <*> parseNull first <*> parseNull second
      setParseNullWithChange ref' parseNullToSet
      emptyToSet <- lift $ return (||) <*> isEmpty first <*> isEmpty second
      setEmptyWithChange ref' emptyToSet
      nullableToSet <- lift $ return (&&) <*> return (emptyToSet == False)
                                          <*> (return (&&) <*> isNullable first
                                                           <*> isNullable second)
      setNullableWithChange ref' nullableToSet
      where
        combineParses :: (Ord a', Ord b') => Set.Set a' -> Set.Set b' -> Set.Set (a', b')
        combineParses p1 p2 = Set.fromList [ (x,y)
                                           | x <- Set.toList p1
                                           , y <- Set.toList p2
                                           ]

    updateCBA' (Alt lhs rhs) ref' = do
      parseNullToSet <- lift $ return Set.union <*> parseNull lhs <*> parseNull rhs
      setParseNullWithChange ref' parseNullToSet
      emptyToSet <- lift $ return (&&) <*> isEmpty lhs <*> isEmpty rhs
      setEmptyWithChange ref' emptyToSet
      nullableToSet <- lift $ return (&&) <*> return (emptyToSet == False)
                                          <*> (return (||) <*> isNullable lhs
                                                           <*> isNullable rhs)
      setNullableWithChange ref' nullableToSet

    updateCBA' (Reduction p f _) ref' = do
      parseNullToSet <- lift $ Set.map f <$> parseNull p
      setParseNullWithChange ref' parseNullToSet
      emptyToSet <- lift $ isEmpty p
      setEmptyWithChange ref' emptyToSet
      nullableToSet <- lift $ isNullable p
      setNullableWithChange ref' nullableToSet

-- Building Parsers
mkCached :: Parser s t a -> Context s (CachedParserRef s t a)
mkCached p = do
  n <- nextID
  let cached = initialOf $ IDParser p n
  lift $ liftM CachedParserRef (newSTRef cached)

initialOf :: IDParser s t a -> CachedParser s t a
initialOf idp = case idParserParser idp of
  (Terminal _) -> base
  (Con _ _) -> base
  (Alt _ _) -> base
  (Epsilon _) -> base { cachedParserIsNullable = True }
  Empty -> base { cachedParserIsEmpty = True }
  (Reduction _ _ _) -> base
  (Repetition _ init' _ _) ->
    base 
    { cachedParserParseNull  = Set.singleton init'
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

-- Parser Constructors
term :: (t -> Bool) -> Context s (CachedParserRef s t t)
term = mkCached . Terminal
termEq :: (Eq t) => t -> Context s (CachedParserRef s t t)
termEq = term . (==)
(.~) :: (Show a, Ord a, Show b, Ord b)
    => CachedParserRef s t a
    -> CachedParserRef s t b
    -> Context s (CachedParserRef s t (a, b))
(.~) x y = mkCached $ Con x y
(~>) :: (Show a, Ord a, Show b, Ord b)
       => CachedParserRef s t a
       -> Context s (CachedParserRef s t b)
       -> Context s (CachedParserRef s t (a, b))
(~>) x ym = (x .~) =<< ym
(<~) :: (Show a, Ord a, Show b, Ord b)
       => Context s (CachedParserRef s t a)
       -> CachedParserRef s t b
       -> Context s (CachedParserRef s t (a, b))
(<~) xm y = (.~ y) =<< xm
(<~>) :: (Show a, Ord a, Show b, Ord b)
      => Context s (CachedParserRef s t a)
      -> Context s (CachedParserRef s t b)
      -> Context s (CachedParserRef s t (a, b))
(<~>) xm ym = uncurry (.~) =<< liftM2 (,) xm ym

(.|) :: CachedParserRef s t a
        -> CachedParserRef s t a
        -> Context s (CachedParserRef s t a)
(.|) x y = mkCached $ Alt x y
(|>) :: CachedParserRef s t a
       -> Context s (CachedParserRef s t a)
       -> Context s (CachedParserRef s t a)
(|>) x ym = (x .|) =<< ym
(<|) :: Context s (CachedParserRef s t a)
       -> CachedParserRef s t a
       -> Context s (CachedParserRef s t a)
(<|) xm y = (.| y) =<< xm
(<|>) :: Context s (CachedParserRef s t a)
      -> Context s (CachedParserRef s t a)
      -> Context s (CachedParserRef s t a)
(<|>) xm ym = uncurry (.|) =<< liftM2 (,) xm ym

infixl 3 .~, ~>, <~, <~>
infixl 1 .|, |>, <|, <|>

eps :: [a] -> Context s (CachedParserRef s t a)
eps = mkCached . Epsilon
emp :: Context s (CachedParserRef s t a)
emp = mkCached Empty
(==>|) :: (Show a, Ord a)
       => Context s (CachedParserRef s t a)
       -> (a -> b)
       -> Int
       -> Context s (CachedParserRef s t b)
(==>|) pm f fid =
  mkCached =<< return Reduction <*> pm <*> return f <*> return fid
(==>) :: (Show a, Ord a)
      => Context s (CachedParserRef s t a)
      -> (a -> b)
      -> Context s (CachedParserRef s t b)
(==>) pm f = (==>|) pm f =<< nextID

rep :: (Show a, Ord a) => CachedParserRef s t a -> b -> (a -> b -> b) -> Context s (CachedParserRef s t b)
rep p init' tallyf = mkCached =<< return . Repetition p init' tallyf =<< nextID

infix 2 ==>, ==>|

-- Weeding is intended to keep the data structure size of a parser small after
-- computing a derivative.
weed :: (Ord a) => CachedParserRef s t a -> Context s ()
weed ref = do
  execChangeContext $ weed' ref
  return ()

weed' :: (Ord a) => CachedParserRef s t a -> ChangeContext s ()
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
        handleSpecial parser ref
  where
    handleSpecial :: (Ord a) => Parser s t a -> CachedParserRef s t a -> ChangeContext s ()
    handleSpecial (Alt lhs rhs) ref' = do
      lhsEmpty <- lift $ isEmpty lhs
      rhsEmpty <- lift $ isEmpty rhs
      case (lhsEmpty, rhsEmpty) of
        (True, False) ->
          lift $ writeCachedParserRef ref' =<< readCachedParserRefCached rhs
        (False, True) ->
          lift $ writeCachedParserRef ref' =<< readCachedParserRefCached lhs
        (False, False) -> return ()
        (True, True) -> error "case should never happen"
    handleSpecial _ _ = return ()
  
-- Derivative
der :: (Ord t, Ord a, Show a) => t -> CachedParserRef s t a -> Context s (CachedParserRef s t a)
der token ref = do
  d <- derInner token ref
  weed d
  return d

derInner :: (Ord t, Ord a, Show a) => t -> CachedParserRef s t a -> Context s (CachedParserRef s t a)
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

derRaw :: (Ord t, Ord a, Show a) => t -> Parser s t a -> CachedParserRef s t a -> Context s (CachedParserRef s t a)
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
derRaw token (Repetition parser _ tallyf' fid) ref =
  der token parser <~ ref ==>| uncurry tallyf' $ fid

-- Parsing
parse :: (Ord t, Ord a, Show a) => CachedParserRef s t a -> [t] -> Context s [(a, [t])]
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
    (Repetition p init' f fid) -> doDerivParse parserRef input -- do a fixed point thing here

doDerivParse :: (Ord a, Ord t, Show a) => CachedParserRef s t a -> [t] -> Context s [(a, [t])]
doDerivParse parserRef [] = do
  nullParses <- parseNull parserRef
  return [(a,[]) | a <- Set.toList nullParses]
doDerivParse parserRef (x:xs) = do
  parseFullOnEmpty <- parseFull parserRef []
  derivParse <- der x parserRef >>= flip parse xs
  return $ combineEven derivParse [(a,(x:xs)) | a <- parseFullOnEmpty]

-- Parsing
parseFull :: (Ord a, Ord t, Show a) => CachedParserRef s t a -> [t] -> Context s [a]
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
    (Repetition _ _ _ _) -> doDerivParseFull parserRef input

doDerivParseFull :: (Ord a, Ord t, Show a) => CachedParserRef s t a -> [t] -> Context s [a]
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

-- A pretty print
showRec :: (Show a) => CachedParserRef s t a -> [Int] -> Int -> Context s String
showRec cachedRef seen depth = do
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

  if g `elem` seen then return seenResult
    else case p of
      (Terminal _) -> return $ printf "(Terminal %s)" cachMsg
      (Con first second) -> do
        let resultFormat = concat [   "(Con %s\n"
                                  , "%s first:\n"
                                  , "%s  %s\n"
                                  , "%s second:\n"
                                  , "%s  %s"
                                  ]
        showFirst <- showRec first seen' nextDepth
        showSecond <- showRec second seen' nextDepth
        return $ printf resultFormat 
                        cachMsg 
                        depthBuffer depthBuffer showFirst 
                        depthBuffer depthBuffer showSecond
      (Alt lhs rhs) -> do
        let resultFormat = concat [   "(Alt %s\n"
                                  , "%s lhs:\n"
                                  , "%s  %s\n"
                                  , "%s rhs:\n"
                                  , "%s  %s"
                                  ]
        showLhs <- showRec lhs seen' nextDepth
        showRhs <- showRec rhs seen' nextDepth
        return $ printf resultFormat
                        cachMsg
                        depthBuffer depthBuffer showLhs
                        depthBuffer depthBuffer showRhs
      (Epsilon nullMatches) -> 
        return $ printf "(Epsilon %s %s)" (show nullMatches) cachMsg
      Empty -> return $ printf "(Empty %s)" cachMsg
      (Reduction rp _ fid) -> do
        let resultFormat = concat [   "(Reduction %s [fid: %s]\n"
                                  , "%s p:\n"
                                  , "%s  %s"
                                  ]
        showRP <- showRec rp seen' nextDepth
        return $ printf resultFormat
                        cachMsg (show fid)
                        depthBuffer depthBuffer showRP
      (Repetition rp _ _ fid) -> do
        let resultFormat = concat [   "(Repetion %s [fid: %s]\n"
                                  , "%s p:\n"
                                  , "%s  %s"
                                  ]
        showRP <- showRec rp seen' nextDepth
        return $ printf resultFormat
                        cachMsg (show fid)
                        depthBuffer depthBuffer showRP
