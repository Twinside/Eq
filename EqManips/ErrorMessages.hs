{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- | This module should be imported as qualified
module EqManips.ErrorMessages where

--------------------------------------------------
----            Generic stuff
--------------------------------------------------
shouldnt_happen = (++ "Shouldn't happen")

--------------------------------------------------
----            Eval defs
--------------------------------------------------
def_diff_argcount = "Warning definition with different argument count"
def_not_lambda = (++ " already defined as not a function")
def_already = (++ " is already defined")

--------------------------------------------------
----            Eval errors
--------------------------------------------------
div_undefined_matrixes = "Division is not defined for matrixes"
div_by_0 = "This expression evaluate to 0, and is used in a division."

factorial_on_real = "Can't apply factorial to real number"
factorial_negative = "No factorial of negative numbers"
factorial_matrix = "No factorial of matrix"

add_matrix = "Addition between matrix and scalar is invalid"
sub_matrix = "Substraction between matrix and scalar is invalid"

empty_binop = (++ "Operator denormalized, no operand in it")
single_binop  = (++ "Operator denormalized, only one operand in it")

not_here = (++ "Shouldn't happen here")
app_no_applygindef = "No function definition match the parameters"


deriv_bad_var_spec = "Sorry your derivation doesn't have a good variable specification"
sum_wrong_bounds = "Sorry, your sum as wrong bounds, can't evaluate"
product_wrong_bounds = "Sorry, your product as wrong bounds, can't evaluate"
integration_no_eval = "No algorithm to integrate your function, sorry"
block_eval = "Block cannot be evaluated"

matrixScalar_badop = "matrixScalar - Should be impossible"
matrix_mul_bad_size = "Error can't multiply matrix, m2 has wrong height"
matrix_empty = "Matrixes are empty" 
matrix_diff_size = "Sorry can't apply this operation on matrix of different sizes"

--------------------------------------------------
----            MetaEval
--------------------------------------------------
wrong_lambda_format = "Your lambda definition doesn't have the good format"
