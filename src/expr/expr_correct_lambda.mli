open Commons
open Expr_ds
open Util

class correct_lambda : object
  inherit [term_db * user_defined_op_ IntMap.t] Expr_map2.expr_map
end

val correct_lambda_context : context -> context
