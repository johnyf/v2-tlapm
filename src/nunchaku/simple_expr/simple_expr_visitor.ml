open Commons
open Simple_expr_ds

class ['a] visitor :
  object
    method expr            : 'a -> simple_expr -> 'a
    method name            : 'a -> string -> 'a
    method location        : 'a -> location -> 'a
    method level           : 'a -> level option -> 'a
    method decimal         : 'a -> simple_decimal -> 'a
    method numeral         : 'a -> simple_numeral -> 'a
    method strng           : 'a -> simple_strng -> 'a
    method at              : 'a -> simple_at -> 'a
    method op_appl         : 'a -> simple_op_appl -> 'a
    method binder          : 'a -> simple_binder -> 'a
    method lambda          : 'a -> simple_lambda -> 'a
    method op_arg          : 'a -> simple_op_arg -> 'a
    method operator        : 'a -> simple_operator -> 'a
    method expr_or_op_arg  : 'a -> simple_expr_or_op_arg -> 'a
    method bound_symbol    : 'a -> simple_bound_symbol -> 'a
    method bounded_bound_symbol   : 'a -> simple_bounded_bound_symbol -> 'a
    method unbounded_bound_symbol : 'a -> simple_unbounded_bound_symbol -> 'a
    method formal_param    : 'a -> simple_formal_param -> 'a
    method op_decl         : 'a -> simple_op_decl -> 'a
    method op_def          : 'a -> simple_op_def -> 'a
    method assume_prove    : 'a -> simple_assume_prove -> 'a
    method new_symb        : 'a -> simple_new_symb -> 'a
    method builtin_op      : 'a -> simple_builtin_op -> 'a
    method user_defined_op : 'a -> simple_user_defined_op -> 'a
    method label           : 'a -> simple_label -> 'a
    method let_in          : 'a -> simple_let_in -> 'a
    method reference       : 'a -> int -> 'a

    method entry           : 'a -> (int * simple_entry) -> 'a

    method op_appl_or_binder : 'a -> simple_op_appl_or_binder -> 'a
    method expr_or_module_or_module_instance :
      'a -> simple_expr_or_module_or_module_instance -> 'a

    method defined_expr : 'a -> simple_defined_expr -> 'a

    method op_def_or_theorem_or_assume       :
      'a -> simple_op_def_or_theorem_or_assume -> 'a

  end
  = object(self)

    (* parts of expressions *)
    method location acc l : 'a = acc
    method level acc l : 'a = acc

    (* non-recursive expressions *)
    method decimal acc d = acc
    method numeral acc n = acc
    method strng acc s = acc

    (* recursive expressions *)
    method at acc0 {location; level; except; except_component} =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = self#op_appl_or_binder acc2 except in
      let acc = self#op_appl_or_binder acc3 except_component in
      acc

    method op_appl acc0 ({location; level; operator; operands} : simple_op_appl) =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = self#operator acc2 operator in
      let acc = List.fold_left self#expr_or_op_arg acc3 operands in
      acc

    method binder acc0 {location; level; operator; operand; bound_symbols} =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = self#operator acc2 operator in
      let acc4 = self#expr_or_op_arg acc3 operand in
      let acc = List.fold_left self#bound_symbol acc4 bound_symbols in
      acc

    method lambda acc ({level; arity; body; params; } : simple_lambda) =
      let acc1 = self#level acc level in
      (* arity *)
      let acc2 = self#expr acc1 body in
      let acc = List.fold_left
          (fun x (fp,_) -> self#formal_param x fp) acc2 params in
      acc

    method bound_symbol acc = function
      | B_bounded_bound_symbol s -> self#bounded_bound_symbol acc s
      | B_unbounded_bound_symbol s -> self#unbounded_bound_symbol acc s

    method bounded_bound_symbol acc x = acc
    method unbounded_bound_symbol acc x = acc


    method formal_param acc0 = function
      | FP_ref i -> self#reference acc0 i
      | FP { location; level; name; arity; } ->
        let acc1 = self#location acc0 location in
        let acc2 = self#level acc1 level in
        let acc3 = self#name acc2 name in
        (* arity skipped *)
        acc3

    method op_arg acc0 {location; level; argument } =
      (* terminal node *)
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      (*skip arity *)
      let acc3 = self#operator acc2 argument in
      acc3

    method op_decl acc0 = function
      | OPD_ref x -> self#reference acc0 x
      | OPD  { location ; level ; name ; arity ; kind ; } ->
        (* terminal node *)
        let acc1 = self#location acc0 location in
        let acc2 = self#level acc1 level in
        let acc3 = self#name acc2 name in
        (* skip arity and kind *)
        acc3

    method op_def acc = function
      | O_builtin_op x      -> self#builtin_op acc x
      | O_user_defined_op x -> self#user_defined_op acc x

    method assume_prove acc0 ({ location; level; new_symbols; assumes;
                                prove;  } : simple_assume_prove) =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = List.fold_left
          self#new_symb acc2 new_symbols in
      let acc4 = List.fold_left
          self#assume_prove acc3 assumes in
      let acc = self#expr acc4 prove in
      (* suffices and boxed are boolean flags*)
      acc						 

    method new_symb acc0 { location; level; op_decl; set } =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = self#op_decl acc2 op_decl in
      let acc = match set with
        | None -> acc3
        | Some e -> self#expr acc3 e
      in acc

    method let_in acc0 {location; level; body; op_defs } =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = self#expr acc2 body in
      let acc = List.fold_left self#op_def_or_theorem_or_assume acc3 op_defs in
      acc

    method label acc0 ({location; level; name; arity; body; params } : simple_label) =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = self#name acc2 name in
      (* skip arity *)
      let acc4 = self#assume_prove acc3 body in
      let acc = List.fold_left self#formal_param acc4 params in
      acc

    method builtin_op acc0 = function
      | { level; name; arity; params } ->
        let acc1 = self#level acc0 level in
        let acc2 = self#name acc1 name in
        (* skip arity *)
        let acc = List.fold_left
            (fun x (fp,_) -> self#formal_param x fp) acc2 params
        in acc

    method user_defined_op acc0 = function
      | UOP_ref x -> self#reference acc0 x
      | UOP { location; level ; name ; arity ;
              body ; params ;  } ->
        let acc1 = self#location acc0 location in
        let acc2 = self#level acc1 level in
        let acc3 = self#name acc2 name in
        (* arity *)
        let acc4 = self#expr acc3 body in
        let acc = List.fold_left
            (fun x fp -> self#formal_param x fp) acc4 params in
        (* skip recursive flag *)
        acc

    method name acc x = acc

    method reference acc x = acc

    method entry acc (id, e) = match e with
      | FP_entry x -> self#formal_param acc (FP x)
      | OPDef_entry x -> self#op_def acc x
      | OPDec_entry x -> self#op_decl acc (OPD x)

    (* pure disjunction types *)
    method expr acc = function
      | E_at x        -> self#at acc x
      | E_decimal x   -> self#decimal acc x
      | E_label x     -> self#label acc x
      | E_let_in x    -> self#let_in acc x
      | E_numeral x   -> self#numeral acc x
      | E_op_appl x   -> self#op_appl acc x
      | E_string x    -> self#strng acc x
      | E_binder x    -> self#binder acc x

    method op_appl_or_binder acc0 = function
      | OB_op_appl x -> self#op_appl acc0 x
      | OB_binder x -> self#binder acc0 x

    method expr_or_module_or_module_instance acc = function
      | EMM_expr x            -> self#expr acc x

    method defined_expr acc = function
      | UMTA_user_defined_op x -> self#user_defined_op acc x

    method op_def_or_theorem_or_assume acc = function
      | OTA_op_def x -> self#op_def acc x

    method expr_or_op_arg acc = function
      | EO_op_arg oa -> self#op_arg acc oa
      | EO_expr e -> self#expr acc e

    method operator acc = function
      | FMOTA_formal_param x -> self#formal_param acc x
      | FMOTA_op_decl x -> self#op_decl acc x
      | FMOTA_op_def  x -> self#op_def acc x
      | FMOTA_lambda x  -> self#lambda acc x
  end
