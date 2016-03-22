open Commons
open Simple_expr_ds

type anySimpleExpr =
  | Nothing
  | Any_location of location
  | Any_level of level option
  | Any_name of string
  | Any_reference of int
  | Any_expr of expr
  | Any_expr_or_op_arg of expr_or_op_arg
  | Any_assume_prove of assume_prove
  | Any_new_symb of new_symb
  | Any_op_def of op_def
  | Any_user_defined_op of user_defined_op
  | Any_user_defined_op_ of user_defined_op_
  | Any_builtin_op of builtin_op
  | Any_op_arg of op_arg
  | Any_formal_param of formal_param
  | Any_formal_param_ of formal_param_
  | Any_op_decl of op_decl
  | Any_op_decl_ of op_decl_
  | Any_expr_or_module_or_module_instance of expr_or_module_or_module_instance
  | Any_defined_expr of defined_expr
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
  | Any_mule_entry of mule_entry
  | Any_entry of (int * entry)


type 'a macc =  anySimpleExpr * 'a


let format_anysimple = function
  | Nothing -> "nothing"
  | Any_location _ -> "location"
  | Any_level _ -> "level option"
  | Any_name _ -> "string"
  | Any_reference _ -> "int"
  | Any_expr _ -> "expr"
  | Any_expr_or_op_arg _ -> "expr_or_op_arg"
  | Any_assume_prove _ -> "assume_prove"
  | Any_new_symb _ -> "new_symb"
  | Any_op_def _ -> "op_def"
  | Any_user_defined_op _ -> "user_defined_op"
  | Any_user_defined_op_ _ -> "user_defined_op_"
  | Any_builtin_op _ -> "builtin_op"
  | Any_op_arg _ -> "op_arg"
  | Any_formal_param _ -> "formal_param"
  | Any_formal_param_ _ -> "formal_param_"
  | Any_op_decl _ -> "op_decl"
  | Any_op_decl_ _ -> "op_decl_"
  | Any_expr_or_module_or_module_instance _ ->
     "expr_or_module_or_module_instance"
  | Any_defined_expr _ -> "defined_expr"
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
  | Any_entry _ -> "(int * entry)"


class ['a] any_extractor = object(self)
  method extract: ('a -> anySimpleExpr) = fun x -> Nothing

  method private fmt x =
    "Unexpected match of: " ^ (format_anysimple (self#extract x))

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
                              | _ -> failwith (self#fmt acc)
  method formal_param acc =
    match self#extract acc with Any_formal_param x -> x
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
  method mule_entry acc =
    match self#extract acc with Any_mule_entry x -> x
                              | _ -> failwith (self#fmt acc)
  method name acc =
    match self#extract acc with Any_name x -> x
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
  method reference acc =
    match self#extract acc with Any_reference x -> x
                              | _ -> failwith (self#fmt acc)
  method strng acc =
    match self#extract acc with Any_strng x -> x
                              | _ -> failwith (self#fmt acc)
  method unbounded_bound_symbol acc =
    match self#extract acc with Any_unbounded_bound_symbol x -> x
                              | _ -> failwith (self#fmt acc)
  method user_defined_op acc =
    match self#extract acc with Any_user_defined_op x -> x
                              | _ -> failwith (self#fmt acc)
end
