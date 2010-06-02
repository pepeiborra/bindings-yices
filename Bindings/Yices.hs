{-# LANGUAGE EmptyDataDecls #-}

module Bindings.Yices (YDef(..), module Bindings.Yices) where

import Bindings.Yices.Internal
import Control.Applicative
import Control.Monad
import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import qualified Data.Traversable as T

type Context = Ptr YContext
type Expr    = Ptr YExpr
type Decl    = Ptr YDecl
type VarDecl = Ptr YVarDecl
type Type    = Ptr YType
type Model   = Ptr YModel
type VarIterator = Ptr YVarIterator

type YBool  = YDef Bool

data SAT a  = Sat a | Unknown a | Unsat

-- * Initialization

mkContext :: IO Context
mkContext = c_mk_context

delContext :: Context -> IO ()
delContext = c_del_context

withContext :: (Context -> IO a) -> IO a
withContext f = do {ctx <- c_mk_context; f ctx <* c_del_context ctx}

setVerbosity :: Int -> IO ()
setVerbosity = c_set_verbosity . fromIntegral

setLogFile :: FilePath -> IO ()
setLogFile fp = withCString fp c_enable_log_file

enableTypeChecker :: Bool -> IO ()
enableTypeChecker True  = c_enable_type_checker 1
enableTypeChecker False = c_enable_type_checker 0

isInconsistent :: Context -> IO Bool
isInconsistent = liftM (toEnum.fromIntegral) . c_inconsistent

interrupt :: Context -> IO ()
interrupt = c_interrupt

-- * Assertions

assert :: Context -> Expr -> IO ()
assert = c_assert

assertWeighted :: Context -> Expr -> Int -> IO AssertionId
assertWeighted ctx e = c_assert_weighted ctx e . fromIntegral

-- * Proof Search

check :: Context -> IO YBool
check = liftM eatYBool . c_check

maxSat :: Context -> IO YBool
maxSat = liftM eatYBool . c_max_sat

maxSatCost :: Context -> Int -> IO YBool
maxSatCost ctx = liftM eatYBool . c_max_sat_cost_leq ctx . fromIntegral

-- | Calls 'check' to ensure that the context is satisfiable and then returns a model
getModel :: Context -> IO (SAT [(String, YBool)])
getModel ctx = do
  sat <- check ctx
  case sat of
    YDef True  -> Sat <$> (getBoolModel ctx =<< c_get_model ctx)
    YUndef     -> Unknown <$> (getBoolModel ctx =<< c_get_model ctx)
    YDef False -> pure Unsat

getMaxSatModel :: Context -> Maybe Int -> IO (SAT [(String, YBool)])
getMaxSatModel ctx cost = do
  sat <- maybe (maxSat ctx) (maxSatCost ctx) cost
  case sat of
    YDef True  -> Sat <$> (getBoolModel ctx =<< c_get_model ctx)
    YUndef     -> Unknown <$> (getBoolModel ctx =<< c_get_model ctx)
    YDef False -> pure Unsat

-- | Returns a model
--   Assumes that you have evaluated the context already with 'check' or maxSat'.
--   The behaviour if you haven't done so is unspecified.
getModel' :: Context -> IO Model
getModel' = c_get_model

findWeightedModel :: Context -> Int -> IO YBool
findWeightedModel ctx = liftM eatYBool . c_find_weighted_model ctx . fromIntegral

-- Evaluation

getVarDecl :: Context -> String -> IO(Maybe VarDecl)
getVarDecl ctx name = do
  ptr <- withCString name $ c_get_var_decl_from_name ctx
  return $ if nullPtr == ptr then Nothing else Just ptr

getVarFromDecl :: Context -> VarDecl -> IO Expr
getVarFromDecl = c_mk_var_from_decl

getVar :: Context -> String -> IO (Maybe Expr)
getVar ctx name = getVarDecl ctx name >>= T.sequence . liftM (getVarFromDecl ctx)

class YEval a where getValue :: Model -> VarDecl -> IO (YDef a)
instance YEval Bool where getValue = getBoolValue
instance YEval Int  where getValue = getNatValue

getBoolValue :: Model -> VarDecl -> IO YBool
getBoolValue m = liftM eatYBool . c_get_value m

getNatValue :: Model -> VarDecl -> IO (YDef Int)
getNatValue m vd = alloca $ \ptr -> do
                              code <- c_get_int_value m vd ptr
                              case code of
                                0 -> return YUndef
                                1 -> YDef . fromIntegral <$> peek ptr

evaluateInModel :: Model -> Expr -> IO YBool
evaluateInModel m = liftM eatYBool . c_evaluate_in_model m

withVarIterator :: Context -> (VarIterator -> IO a) -> IO a
withVarIterator ctx f = do
  iter <- c_create_var_decl_iterator ctx
  f iter <* c_del_iterator iter

hasNext :: VarIterator -> IO Bool
hasNext = liftM (toEnum . fromIntegral) . c_iterator_has_next

-- * Expressions

mkTrue, mkFalse :: Context -> IO Expr
mkTrue  = c_mk_true
mkFalse = c_mk_false

mkVar :: Context -> String -> Type -> IO Expr
mkVar ctx name typ = withCString name (\n -> c_mk_var_decl ctx n typ) >>= c_mk_var_from_decl ctx

mkBoolVar :: Context -> String -> IO Expr
mkBoolVar ctx name = withCString name $ c_mk_bool_var ctx

mkNatVar  :: Context -> String -> IO Expr
mkNatVar  ctx name = mkVar ctx name =<< mkNatType ctx

mkBoolType :: Context -> IO Type
mkBoolType ctx = mkType ctx "bool"
mkNatType :: Context -> IO Type
mkNatType ctx = mkType ctx "nat"
mkType :: Context -> String -> IO Type
mkType ctx typ =  withCString typ $ c_mk_type ctx

mkFreshBoolVar :: Context -> IO Expr
mkFreshBoolVar = c_mk_fresh_bool_var

mkNot :: Context -> Expr -> IO Expr
mkNot = c_mk_not

mkAnd,mkOr :: Context -> [Expr] -> IO Expr
mkAnd ctx ee = withArray ee $ \ee_a -> c_mk_and ctx ee_a (fromIntegral $ length ee)
mkOr  ctx ee = withArray ee $ \ee_a -> c_mk_or  ctx ee_a (fromIntegral $ length ee)

mkAnd2,mkOr2 :: Context -> Expr -> Expr -> IO Expr
mkAnd2 ctx e1 e2 = withArray [e1,e2] $ \ee_a -> c_mk_and ctx ee_a 2
mkOr2  ctx e1 e2 = withArray [e1,e2] $ \ee_a -> c_mk_or  ctx ee_a 2

mkEq, mkDiseq :: Context -> Expr -> Expr -> IO Expr
mkEq = c_mk_eq
mkDiseq = c_mk_diseq

mkIte :: Context -> Expr -> Expr -> Expr -> IO Expr
mkIte = c_mk_ite

mkNum :: Context -> Int -> IO Expr
mkNum ctx = c_mk_num ctx . fromIntegral

mkGt, mkLt, mkGe, mkLe :: Context -> Expr -> Expr -> IO Expr
mkLe = c_mk_le
mkLt = c_mk_lt
mkGt = c_mk_gt
mkGe = c_mk_ge

-- * Types

-- -------------
-- Marshalling
-- -------------

getAllVariables :: Context -> Model -> IO [VarDecl]
getAllVariables ctx m = withVarIterator ctx $ \iter ->
  let go = do
        cont <- hasNext iter
        if cont
          then do
             v     <- c_iterator_next iter
             rest  <- go
             return (v : rest)

          else return []
  in go

getBoolModel :: Context -> Model -> IO [(String, YBool)]
getBoolModel ctx m = withVarIterator ctx $ \iter ->
  let go = do
        cont <- hasNext iter
        if cont
          then do
             v     <- c_iterator_next iter
             name  <- peekCString =<< c_get_var_decl_name v
             value <- getValue m v
             rest  <- go
             return ((name, value) : rest)

          else return []
  in go
