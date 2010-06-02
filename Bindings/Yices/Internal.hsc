{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Bindings.Yices.Internal where

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

data YContext
data YExpr
data YDecl
data YVarDecl
data YType
data YModel
data YVarIterator
newtype AssertionId = AssertionId Int deriving (Eq, Ord)
data YDef a = YDef !a | YUndef


#include "yices_c.h"


foreign import ccall "yices_c.h yices_interrupt"
  c_interrupt :: Ptr YContext -> IO (())

-- -------------
-- Marshalling
-- -------------

eatYBool l = case l of
    #const l_false
        -> YDef False
    #const l_true
        -> YDef True
    #const l_undef
        -> YUndef

-- -----------
-- Primitives
-- -----------

foreign import ccall "yices_c.h yices_set_verbosity"
  c_set_verbosity :: CInt -> IO (())

--

foreign import ccall "yices_c.h yices_version"
  c_version :: IO (Ptr (CChar))

--

foreign import ccall "yices_c.h yices_set_max_num_conflicts_in_maxsat_iteration"
  c_set_max_num_conflicts_in_maxsat_iteration :: CUInt -> IO (())

--

foreign import ccall "yices_c.h yices_enable_type_checker"
  c_enable_type_checker :: CInt -> IO (())

--

foreign import ccall "yices_c.h yices_set_max_num_iterations_in_maxsat"
  c_set_max_num_iterations_in_maxsat :: CUInt -> IO (())

--

foreign import ccall safe "yices_c.h yices_set_maxsat_initial_cost"
  c_set_maxsat_initial_cost :: CLLong -> IO (())

--

foreign import ccall "yices_c.h yices_set_arith_only"
  c_set_arith_only :: CInt -> IO (())

--

foreign import ccall "yices_c.h yices_enable_log_file"
  c_enable_log_file :: Ptr (CChar) -> IO (())

--

foreign import ccall "yices_c.h yices_mk_context"
  c_mk_context :: IO (Ptr YContext)

--

foreign import ccall "yices_c.h yices_del_context"
  c_del_context :: Ptr YContext -> IO (())

--

foreign import ccall "yices_c.h yices_reset"
  c_reset :: Ptr YContext -> IO (())

--

foreign import ccall "yices_c.h yices_dump_context"
  c_dump_context :: Ptr YContext -> IO (())

--

foreign import ccall "yices_c.h yices_push"
  c_push :: Ptr YContext -> IO (())

--

foreign import ccall "yices_c.h yices_pop"
  c_pop :: Ptr YContext -> IO (())

--

foreign import ccall "yices_c.h yices_assert"
  c_assert :: Ptr YContext -> Ptr YExpr -> IO (())

--

foreign import ccall "yices_c.h yices_assert_weighted"
  c_assert_weighted :: Ptr YContext -> Ptr YExpr -> CLLong -> IO AssertionId

--

foreign import ccall "yices_c.h yices_assert_retractable"
  c_assert_retractable :: Ptr YContext -> Ptr YExpr -> IO (CInt)

--

foreign import ccall "yices_c.h yices_retract"
  c_retract :: Ptr YContext -> CInt -> IO (())

--

foreign import ccall safe "yices_c.h yices_inconsistent"
  c_inconsistent :: Ptr YContext -> IO (CInt)

--

foreign import ccall safe "yices_c.h yices_check"
  c_check :: Ptr YContext -> IO (CInt)

--

foreign import ccall safe "yices_c.h yices_find_weighted_model"
  c_find_weighted_model :: Ptr YContext -> CInt -> IO (CInt)

--

foreign import ccall "yices_c.h yices_evaluate_in_model"
  c_evaluate_in_model :: Ptr YModel -> Ptr YExpr -> IO CInt

--

foreign import ccall safe "yices_c.h yices_max_sat"
  c_max_sat :: Ptr YContext -> IO (CInt)

--

foreign import ccall safe "yices_c.h yices_max_sat_cost_leq"
  c_max_sat_cost_leq :: Ptr YContext -> CLLong -> IO (CInt)

--

foreign import ccall safe "yices_c.h yices_get_model"
  c_get_model :: Ptr YContext -> IO (Ptr YModel)

--

foreign import ccall safe "yices_c.h yices_get_unsat_core_size"
  c_get_unsat_core_size :: Ptr YContext -> IO (CUInt)

--

foreign import ccall safe "yices_c.h yices_get_unsat_core"
  c_get_unsat_core :: Ptr YContext -> Ptr (CInt) -> IO (CUInt)

--

foreign import ccall "yices_c.h yices_get_value"
  c_get_value :: Ptr YModel -> Ptr YVarDecl -> IO CInt

--

foreign import ccall "yices_c.h yices_get_int_value"
  c_get_int_value :: Ptr YModel -> Ptr YVarDecl -> Ptr (CLong) -> IO CInt

--

foreign import ccall "yices_c.h yices_get_arith_value"
  c_get_arith_value :: Ptr YModel -> Ptr YVarDecl -> Ptr (CLong) -> Ptr (CLong) -> IO CInt

--

foreign import ccall "yices_c.h yices_get_double_value"
  c_get_double_value :: Ptr YModel -> Ptr YVarDecl -> Ptr (CDouble) -> IO CInt

--

foreign import ccall "yices_c.h yices_get_bitvector_value"
  c_get_bitvector_value :: Ptr YModel -> Ptr YVarDecl -> CUInt -> Ptr (CInt) -> IO CInt

--

foreign import ccall "yices_c.h yices_get_assertion_value"
  c_get_assertion_value :: Ptr YModel -> AssertionId -> IO CInt

--

foreign import ccall "yices_c.h yices_display_model"
  c_display_model :: Ptr YModel -> IO ()

--

foreign import ccall "yices_c.h yices_get_cost"
  c_get_cost :: Ptr YModel -> IO CLLong

--

foreign import ccall "yices_c.h yices_get_cost_as_double"
  c_get_cost_as_double :: Ptr YModel -> IO CDouble

--

foreign import ccall "yices_c.h yices_mk_true"
  c_mk_true :: Ptr YContext -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_false"
  c_mk_false :: Ptr YContext -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_bool_var"
  c_mk_bool_var :: Ptr YContext -> Ptr (CChar) -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_fresh_bool_var"
  c_mk_fresh_bool_var :: Ptr YContext -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_get_var_decl"
  c_get_var_decl :: Ptr YExpr -> IO (Ptr YVarDecl)

--

foreign import ccall "yices_c.h yices_mk_bool_var_decl"
  c_mk_bool_var_decl :: Ptr YContext -> Ptr CChar -> IO (Ptr YVarDecl)

--

foreign import ccall "yices_c.h yices_get_var_decl_name"
  c_get_var_decl_name :: Ptr YVarDecl -> IO (Ptr CChar)

--

foreign import ccall "yices_c.h yices_mk_bool_var_from_decl"
  c_mk_bool_var_from_decl :: Ptr YContext -> Ptr YDecl -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_or"
  c_mk_or :: Ptr YContext -> Ptr (Ptr YExpr) -> CUInt -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_and"
  c_mk_and :: Ptr YContext -> Ptr (Ptr YExpr) -> CUInt -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_eq"
  c_mk_eq :: Ptr YContext -> Ptr YExpr -> Ptr YExpr -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_diseq"
  c_mk_diseq :: Ptr YContext -> Ptr YExpr -> Ptr YExpr -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_ite"
  c_mk_ite :: Ptr YContext -> Ptr YExpr -> Ptr YExpr -> Ptr YExpr -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_not"
  c_mk_not :: Ptr YContext -> Ptr YExpr -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_create_var_decl_iterator"
  c_create_var_decl_iterator :: Ptr YContext -> IO (Ptr YVarIterator)

--

foreign import ccall "yices_c.h yices_iterator_has_next"
  c_iterator_has_next :: Ptr YVarIterator -> IO CInt

--

foreign import ccall "yices_c.h yices_iterator_next"
  c_iterator_next :: Ptr YVarIterator -> IO (Ptr YVarDecl)

--

foreign import ccall "yices_c.h yices_iterator_reset"
  c_iterator_reset :: Ptr YVarIterator -> IO ()

--

foreign import ccall "yices_c.h yices_del_iterator"
  c_del_iterator :: Ptr YVarIterator -> IO ()

--

foreign import ccall "yices_c.h yices_mk_type"
  c_mk_type :: Ptr YContext -> Ptr CChar -> IO (Ptr YType)

--

foreign import ccall "yices_c.h yices_mk_function_type"
  c_mk_function_type :: Ptr YContext -> Ptr (Ptr YType) -> CUInt -> Ptr YType -> IO (Ptr YType)

--

foreign import ccall "yices_c.h yices_mk_bitvector_type"
  c_mk_bitvector_type :: Ptr YContext -> CUInt -> IO (Ptr YType)

--

foreign import ccall "yices_c.h yices_mk_tuple_type"
  c_mk_tuple_type :: Ptr YContext -> Ptr (Ptr (Ptr YType)) -> CUInt -> IO (Ptr YType)

--

foreign import ccall "yices_c.h yices_mk_var_decl"
  c_mk_var_decl :: Ptr YContext -> Ptr CChar -> Ptr YType -> IO (Ptr YVarDecl)

--

foreign import ccall "yices_c.h yices_get_var_decl_from_name"
  c_get_var_decl_from_name :: Ptr YContext -> Ptr CChar -> IO (Ptr YVarDecl)

--

foreign import ccall "yices_c.h yices_mk_var_from_decl"
  c_mk_var_from_decl :: Ptr YContext -> Ptr YVarDecl -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_app"
  c_mk_app :: Ptr YContext -> Ptr YExpr -> Ptr (Ptr YExpr) -> CUInt -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_num"
  c_mk_num :: Ptr YContext -> CInt -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_num_from_string"
  c_mk_num_from_string :: Ptr YContext -> Ptr CChar -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_sum"
  c_mk_sum :: Ptr YContext -> Ptr (Ptr YExpr) -> CUInt -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_sub"
  c_mk_sub :: Ptr YContext -> Ptr (Ptr YExpr) -> CUInt -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_mul"
  c_mk_mul :: Ptr YContext -> Ptr (Ptr YExpr) -> CUInt -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_lt"
  c_mk_lt :: Ptr YContext -> Ptr YExpr -> Ptr YExpr -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_le"
  c_mk_le :: Ptr YContext -> Ptr YExpr -> Ptr YExpr -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_gt"
  c_mk_gt :: Ptr YContext -> Ptr YExpr -> Ptr YExpr -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_ge"
  c_mk_ge :: Ptr YContext -> Ptr YExpr -> Ptr YExpr -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_bv_constant"
  c_mk_bv_constant :: Ptr YContext -> CUInt -> CULong -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_bv_constant_from_array"
  c_mk_bv_constant_from_array :: Ptr YContext -> CUInt -> Ptr (CInt) -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_bv_add"
  c_mk_bv_add :: Ptr YContext -> Ptr YExpr -> Ptr YExpr -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_bv_sub"
  c_mk_bv_sub :: Ptr YContext -> Ptr YExpr -> Ptr YExpr -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_bv_mul"
  c_mk_bv_mul :: Ptr YContext -> Ptr YExpr -> Ptr YExpr -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_bv_minus"
  c_mk_bv_minus :: Ptr YContext -> Ptr YExpr -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_bv_concat"
  c_mk_bv_concat :: Ptr YContext -> Ptr YExpr -> Ptr YExpr -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_bv_and"
  c_mk_bv_and :: Ptr YContext -> Ptr YExpr -> Ptr YExpr -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_bv_or"
  c_mk_bv_or :: Ptr YContext -> Ptr YExpr -> Ptr YExpr -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_bv_xor"
  c_mk_bv_xor :: Ptr YContext -> Ptr YExpr -> Ptr YExpr -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_bv_not"
  c_mk_bv_not :: Ptr YContext -> Ptr YExpr -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_bv_extract"
  c_mk_bv_extract :: Ptr YContext -> CUInt -> CUInt -> Ptr YExpr -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_bv_sign_extend"
  c_mk_bv_sign_extend :: Ptr YContext -> Ptr YExpr -> CUInt -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_bv_shift_left0"
  c_mk_bv_shift_left0 :: Ptr YContext -> Ptr YExpr -> CUInt -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_bv_shift_left1"
  c_mk_bv_shift_left1 :: Ptr YContext -> Ptr YExpr -> CUInt -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_bv_shift_right0"
  c_mk_bv_shift_right0 :: Ptr YContext -> Ptr YExpr -> CUInt -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_bv_shift_right1"
  c_mk_bv_shift_right1 :: Ptr YContext -> Ptr YExpr -> CUInt -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_bv_lt"
  c_mk_bv_lt :: Ptr YContext -> Ptr YExpr -> Ptr YExpr -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_bv_le"
  c_mk_bv_le :: Ptr YContext -> Ptr YExpr -> Ptr YExpr -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_bv_gt"
  c_mk_bv_gt :: Ptr YContext -> Ptr YExpr -> Ptr YExpr -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_bv_ge"
  c_mk_bv_ge :: Ptr YContext -> Ptr YExpr -> Ptr YExpr -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_bv_slt"
  c_mk_bv_slt :: Ptr YContext -> Ptr YExpr -> Ptr YExpr -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_bv_sle"
  c_mk_bv_sle :: Ptr YContext -> Ptr YExpr -> Ptr YExpr -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_bv_sgt"
  c_mk_bv_sgt :: Ptr YContext -> Ptr YExpr -> Ptr YExpr -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_mk_bv_sge"
  c_mk_bv_sge :: Ptr YContext -> Ptr YExpr -> Ptr YExpr -> IO (Ptr YExpr)

--

foreign import ccall "yices_c.h yices_pp_expr"
  c_pp_expr :: Ptr YExpr -> IO ()

--

