open Commons
open Expr_ds

type anyExpr =
  | Nothing
  | Any_location of location
  | Any_level of level option
  | Any_name of string
  | Any_reference of int
  | Any_node of node
  | Any_expr of expr
  | Any_expr_or_op_arg of expr_or_op_arg
  | Any_ap_subst_in of ap_subst_in
  | Any_subst_in of subst_in
  | Any_instance of instance
  | Any_subst of subst
  | Any_assume of assume
  | Any_assume_ of assume_
  | Any_theorem of theorem
  | Any_theorem_ of theorem_
  | Any_assume_prove of assume_prove
  | Any_new_symb of new_symb
  | Any_op_def of op_def
  | Any_op_def_ of op_def_
  | Any_module_instance of module_instance
  | Any_module_instance_ of module_instance_
  | Any_user_defined_op of user_defined_op
  | Any_user_defined_op_ of user_defined_op_
  | Any_builtin_op of builtin_op
  | Any_op_arg of op_arg
  | Any_formal_param of formal_param
  | Any_formal_param_ of formal_param_
  | Any_op_decl of op_decl
  | Any_op_decl_ of op_decl_
  | Any_proof of proof
  | Any_omitted of omitted
  | Any_obvious of obvious
  | Any_expr_or_module_or_module_instance of expr_or_module_or_module_instance
  | Any_defined_expr of defined_expr
  | Any_by of by
  | Any_steps of steps
  | Any_step of step
  | Any_def_step of  def_step
  | Any_use_or_hide of use_or_hide
  | Any_at of at
  | Any_decimal of decimal
  | Any_label of label
  | Any_op_def_or_theorem_or_assume of op_def_or_theorem_or_assume
  | Any_let_in of let_in
  | Any_numeral of numeral
  | Any_strng of strng
  | Any_operator of operator
  | Any_op_appl of op_appl
  | Any_op_appl_or_binder of op_appl_or_binder
  | Any_binder of binder
  | Any_lambda of lambda
  | Any_bound_symbol of bound_symbol
  | Any_unbounded_bound_symbol of unbounded_bound_symbol
  | Any_bounded_bound_symbol of bounded_bound_symbol
  | Any_mule of mule
  | Any_mule_ of  mule_
  | Any_context of context
  | Any_entry of (int * entry)

type 'a macc =  anyExpr * 'a


let format_anyexpr = function
  | Nothing -> "nothing"
  | Any_location _ -> "location"
  | Any_level _ -> "level option"
  | Any_name _ -> "string"
  | Any_reference _ -> "int"
  | Any_node _ -> "node"
  | Any_expr _ -> "expr"
  | Any_expr_or_op_arg _ -> "expr_or_op_arg"
  | Any_ap_subst_in _ -> "ap_subst_in"
  | Any_subst_in _ -> "subst_in"
  | Any_instance _ -> "instance"
  | Any_subst _ -> "subst"
  | Any_assume _ -> "assume"
  | Any_assume_ _ -> "assume_"
  | Any_theorem _ -> "theorem"
  | Any_theorem_ _ -> "theorem_"
  | Any_assume_prove _ -> "assume_prove"
  | Any_new_symb _ -> "new_symb"
  | Any_op_def _ -> "op_def"
  | Any_op_def_ _ -> "op_def_"
  | Any_module_instance _ -> "module_instance"
  | Any_module_instance_ _ -> "module_instance_"
  | Any_user_defined_op _ -> "user_defined_op"
  | Any_user_defined_op_ _ -> "user_defined_op_"
  | Any_builtin_op _ -> "builtin_op"
  | Any_op_arg _ -> "op_arg"
  | Any_formal_param _ -> "formal_param"
  | Any_formal_param_ _ -> "formal_param_"
  | Any_op_decl _ -> "op_decl"
  | Any_op_decl_ _ -> "op_decl_"
  | Any_proof _ -> "proof"
  | Any_omitted _ -> "omitted"
  | Any_obvious _ -> "obvious"
  | Any_expr_or_module_or_module_instance _ ->
     "expr_or_module_or_module_instance"
  | Any_defined_expr _ -> "defined_expr"
  | Any_by _ -> "by"
  | Any_steps _ -> "steps"
  | Any_step _ -> "step"
  | Any_def_step _ -> " def_step"
  | Any_use_or_hide _ -> "use_or_hide"
  | Any_at _ -> "at"
  | Any_decimal _ -> "decimal"
  | Any_label _ -> "label"
  | Any_op_def_or_theorem_or_assume _ -> "op_def_or_theorem_or_assume"
  | Any_let_in _ -> "let_in"
  | Any_numeral _ -> "numeral"
  | Any_strng _ -> "strng"
  | Any_operator _ -> "operator"
  | Any_op_appl _ -> "op_appl"
  | Any_op_appl_or_binder _ -> "op_appl_or_binder"
  | Any_binder _ -> "binder"
  | Any_lambda _ -> "lambda"
  | Any_bound_symbol _ -> "bound_symbol"
  | Any_unbounded_bound_symbol _ -> "unbounded_bound_symbol"
  | Any_bounded_bound_symbol _ -> "bounded_bound_symbol"
  | Any_mule _ -> "mule"
  | Any_mule_ _ -> " mule_"
  | Any_context _ -> "context"
  | Any_entry _ -> "(int * entry)"



class ['a] any_extractor = object(self)
  method extract: ('a -> anyExpr) = fun x -> Nothing

  method private fmt x =
    "Unexpected match of: " ^ (format_anyexpr (self#extract x))

  method ap_subst_in acc =
    match self#extract acc with Any_ap_subst_in x -> x
                              | _ -> failwith (self#fmt acc)
  method assume acc =
    match self#extract acc with Any_assume x -> x
                              | _ -> failwith (self#fmt acc)
  method assume_prove acc =
    match self#extract acc with Any_assume_prove x -> x
                              | _ -> failwith (self#fmt acc)
  method at acc =
    match self#extract acc with Any_at x -> x
                              | _ -> failwith (self#fmt acc)
  method binder acc =
    match self#extract acc with Any_binder x -> x
                              | _ -> failwith (self#fmt acc)
  method bound_symbol acc =
    match self#extract acc with Any_bound_symbol x -> x
                              | _ -> failwith (self#fmt acc)
  method bounded_bound_symbol acc =
    match self#extract acc with Any_bounded_bound_symbol x -> x
                              | _ -> failwith (self#fmt acc)
  method builtin_op acc =
    match self#extract acc with Any_builtin_op x -> x
                              | _ -> failwith (self#fmt acc)
  method decimal acc =
    match self#extract acc with Any_decimal x -> x
                              | _ -> failwith (self#fmt acc)
  method def_step acc =
    match self#extract acc with Any_def_step x -> x
                              | _ -> failwith (self#fmt acc)
  method defined_expr acc =
    match self#extract acc with Any_defined_expr x -> x
                              | _ -> failwith (self#fmt acc)
  method entry acc =
    match self#extract acc with Any_entry x -> x
                              | _ -> failwith (self#fmt acc)
  method expr acc =
    match self#extract acc with Any_expr x -> x
                              | _ -> failwith (self#fmt acc)
  method expr_or_op_arg acc =
    match self#extract acc with Any_expr_or_op_arg x -> x
                              | _ -> failwith (self#fmt acc)
  method expr_or_module_or_module_instance acc =
    match self#extract acc with Any_expr_or_module_or_module_instance x -> x

  method formal_param acc =
    match self#extract acc with Any_formal_param x -> x
                              | _ -> failwith (self#fmt acc)
  method instance acc =
    match self#extract acc with Any_instance x -> x
                              | _ -> failwith (self#fmt acc)
  method label acc =
    match self#extract acc with Any_label x -> x
                              | _ -> failwith (self#fmt acc)
  method lambda acc =
    match self#extract acc with Any_lambda x -> x
                              | _ -> failwith (self#fmt acc)
  method let_in acc =
    match self#extract acc with Any_let_in x -> x
                              | _ -> failwith (self#fmt acc)
  method level acc =
    match self#extract acc with Any_level x -> x
                              | _ -> failwith (self#fmt acc)
  method location acc =
    match self#extract acc with Any_location x -> x
                              | _ -> failwith (self#fmt acc)
  method module_instance acc =
    match self#extract acc with Any_module_instance x -> x
                              | _ -> failwith (self#fmt acc)
  method mule acc =
    match self#extract acc with Any_mule x -> x
                              | _ -> failwith (self#fmt acc)
  method name acc =
    match self#extract acc with Any_name x -> x
                              | _ -> failwith (self#fmt acc)
  method node acc =
    match self#extract acc with Any_node x -> x
                              | _ -> failwith (self#fmt acc)
  method numeral acc =
    match self#extract acc with Any_numeral x -> x
                              | _ -> failwith (self#fmt acc)
  method new_symb acc =
    match self#extract acc with Any_new_symb x -> x
                              | _ -> failwith (self#fmt acc)
  method op_appl_or_binder acc =
    match self#extract acc with Any_op_appl_or_binder x -> x
                              | _ -> failwith (self#fmt acc)
  method op_arg acc =
    match self#extract acc with Any_op_arg x -> x
                              | _ -> failwith (self#fmt acc)
  method op_appl acc =
    match self#extract acc with Any_op_appl x -> x
                              | _ -> failwith (self#fmt acc)
  method op_decl acc =
    match self#extract acc with Any_op_decl x -> x
                              | _ -> failwith (self#fmt acc)
  method op_def acc =
    match self#extract acc with Any_op_def x -> x
                              | _ -> failwith (self#fmt acc)
  method op_def_or_theorem_or_assume acc =
    match self#extract acc with Any_op_def_or_theorem_or_assume x -> x
                              | _ -> failwith (self#fmt acc)
  method operator acc =
    match self#extract acc with Any_operator x -> x
                              | _ -> failwith (self#fmt acc)
  method proof acc =
    match self#extract acc with Any_proof x -> x
                              | _ -> failwith (self#fmt acc)
  method reference acc =
    match self#extract acc with Any_reference x -> x
                              | _ -> failwith (self#fmt acc)
  method step acc =
    match self#extract acc with Any_step x -> x

  method strng acc =
    match self#extract acc with Any_strng x -> x
                              | _ -> failwith (self#fmt acc)
  method subst_in acc =
    match self#extract acc with Any_subst_in x -> x
                              | _ -> failwith (self#fmt acc)
  method subst acc =
    match self#extract acc with Any_subst x -> x
                              | _ -> failwith (self#fmt acc)
  method theorem acc =
    match self#extract acc with Any_theorem x -> x
                              | _ -> failwith (self#fmt acc)
  method unbounded_bound_symbol acc =
    match self#extract acc with Any_unbounded_bound_symbol x -> x
                              | _ -> failwith (self#fmt acc)
  method use_or_hide acc =
    match self#extract acc with Any_use_or_hide x -> x
                              | _ -> failwith (self#fmt acc)
  method user_defined_op acc =
    match self#extract acc with Any_user_defined_op x -> x
                              | _ -> failwith (self#fmt acc)
end
