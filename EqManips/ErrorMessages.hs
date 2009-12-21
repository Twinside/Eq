{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- | This module should be imported as qualified
module EqManips.ErrorMessages where

--------------------------------------------------
----            Generic stuff
--------------------------------------------------
shouldnt_happen = (++ "Shouldn't happen")
reOp = "reOp Empty formula? WTF"
impossible = (++ " It's impossible. Really.")

--------------------------------------------------
----            Eval defs
--------------------------------------------------
def_diff_argcount = "Warning definition with different argument count"
def_not_lambda = (++ " already defined as not a function")
def_already = (++ " is already defined")

--------------------------------------------------
----            Eval errors
--------------------------------------------------
attrib_in_expr = "You can't attribute a value in an expression"
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

--------------------------------------------------
----            Derivative
--------------------------------------------------
deriv_no_multi_app = "Ok, now solution for app with multi argument"
deriv_no_eq_expr = "Can't derivate expression with a '='"
deriv_no_attrib_expr = "Can't derivate an assignation ':='"
deriv_no_sum = "Sum differentiation is not defined"
deriv_no_product = "Product differentiation is not defined"
deriv_floor_not_continuous = "The floor function is not continuous"
deriv_ceil_not_continuous = "The ceil function in not continuous"
deriv_frac_not_continuous = "I don't know how to derivate the fractional part"
deriv_in_deriv = "No nested differentiation allowed"
deriv_no_integration = "No integration allowed in differentiation"
deriv_no_matrix = "No matrix allowed in differentiation"
deriv_no_bool = "No Boolean value allowed in differentiation"
deriv_lambda = "Differentiation of lambdas"
deriv_block = "An error as previously occured during evaluation, can't differentiate"
deriv_no_factorial = "Differentiation of factorials is undefined"
deriv_no_abs = "Absolute value is not derivable"
deriv_no_log = "No position for Log for now"

--------------------------------------------------
----            C output
--------------------------------------------------
c_out_lambda = "We can't output lambda function in C"
c_out_integrate = "We can't output integrals function in C"
c_out_derivate = "We can't output derivative function in C"
c_out_block = "We can't output evaluation errors in C"
c_out_matrix = "We can't output matrix in C for now (maybe in the future)"
c_out_bad_iteration = "We can't translate product or sum to a meaningfull loop"
c_out_bad_binop = "The binary operator has a wrong internal form"
c_out_complex = "Complex is not yet decided for C/C++ output"

--------------------------------------------------
----            Polynome
--------------------------------------------------
polynom_bad_casting = "Error, coefficients are not compatible, casting error"
polynom_emptyCoeffPack = "Error, empty coeff, big bug!!"
ill_formed_polynomial = "Error the polynome is ill formed, no element in it"
polynom_coeff_notascalar = "Error, you're trying to create a polynome coefficient from a non-scalar element"
polynome_empty = "Error, the polynomial is empty, which is not allowed"
polynome_no_coeff_substitution = "Error, the polynomial coefficient shouldn't be substitued by formula"
