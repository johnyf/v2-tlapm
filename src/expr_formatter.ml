open Commons
open Expr_ds
open Expr_visitor
open Format

class ['a] formatter =
object(self)
  inherit [Format.formatter] visitor as self


  (* parts of expressions *)
   method location ppf l : 'a = ppf
   method level ppf l : 'a = ppf

  (* non-recursive expressions *)
   method decimal ppf { location; level; mantissa; exponent;  } =
     let value =
       (float_of_int mantissa) /. ( 10.0 ** (float_of_int exponent)) in
     fprintf ppf "%se%s" (string_of_float value);
     ppf

   method numeral ppf {location; level; value } =
     fprintf ppf "%s" (string_of_int value);
     ppf

   method strng ppf {location; level; value} =
     fprintf ppf "%s" value;
     ppf

(* recursive expressions *)
   method at ppf0 {location; level; except; except_component} =
     let ppf1 = self#location ppf0 location in
     let ppf2 = self#level ppf1 level in
     let ppf3 = self#op_appl ppf2 except in
     let ppf = self#op_appl ppf3 except_component in
     ppf

   method op_appl ppf0 {location; level; operator; operands; bound_symbols} =
     let ppf1 = self#location ppf0 location in
     let ppf2 = self#level ppf1 level in
     let ppf3 = self#operator ppf2 operator in
     let ppf4 = List.fold_left self#expr_or_op_arg ppf3 operands in
     let ppf = List.fold_left self#bound_symbol ppf4 bound_symbols in
     ppf

   method bound_symbol ppf = function
   | B_bounded_bound_symbol s -> self#bounded_bound_symbol ppf s
   | B_unbounded_bound_symbol s -> self#unbounded_bound_symbol ppf s

   method bounded_bound_symbol ppf x = ppf
   method unbounded_bound_symbol ppf x = ppf


   method formal_param ppf0 = function
   | FP_ref i -> self#reference ppf0 i
   | FP { location; level; name; arity; } ->
     let ppf1 = self#location ppf0 location in
     let ppf2 = self#level ppf1 level in
     let ppf3 = self#name ppf2 name in
     (* arity skipped *)
     ppf3

   method mule ppf0 = function
   | MOD_ref i -> self#reference ppf0 i
   | MOD {name; location; constants; variables;
          definitions; assumptions; theorems; } ->
     let ppf0a = self#name ppf0 name in
     let ppf1 = self#location ppf0a location in
     let ppf2 = List.fold_left self#op_decl ppf1 constants in
     let ppf3 = List.fold_left self#op_decl ppf2 variables in
     let ppf4 = List.fold_left self#op_def ppf3 definitions in
     let ppf5 = List.fold_left self#assume ppf4 assumptions in
     let ppf = List.fold_left self#theorem ppf5 theorems in
     ppf

   method op_arg ppf0 {location; level; name; arity } =
     (* terminal node *)
     let ppf1 = self#location ppf0 location in
     let ppf2 = self#level ppf1 level in
     let ppf3 = self#name ppf2 name in
   (*skip arity *)
     ppf3

   method op_decl ppf0 = function
   | OPD_ref x -> self#reference ppf0 x
   | OPD  { location ; level ; name ; arity ; kind ; } ->
   (* terminal node *)
     let ppf1 = self#location ppf0 location in
     let ppf2 = self#level ppf1 level in
     let ppf3 = self#name ppf2 name in
   (* skip arity and kind *)
     ppf3

   method op_def ppf = function
   | OPDef_ref x -> self#reference ppf x
   | OPDef (O_module_instance x) -> self#module_instance ppf x
   | OPDef (O_builtin_op x)      -> self#builtin_op ppf x
   | OPDef (O_user_defined_op x) -> self#user_defined_op ppf x

   method theorem ppf0 = function
   | THM_ref x -> self#reference ppf0 x
   | THM { location; level; expr; proof; suffices } ->
     let ppf1 = self#location ppf0 location in
     let ppf2 = self#level ppf1 level in
     let ppf3 = self#assume_prove ppf2 expr in
     let ppf4 = self#proof ppf3 proof  in
     (* skip suffices *)
     ppf4

   method assume ppf0  = function
   | ASSUME_ref x -> self#reference ppf0 x
   | ASSUME {location; level; expr; } ->
     let ppf1 = self#location ppf0 location in
     let ppf2 = self#level ppf1 level in
     let ppf = self#expr ppf2 expr in
     ppf

   method proof ppf0 = function
   | P_omitted location -> ppf0
   | P_obvious location -> ppf0
   | P_by { location; level; facts; defs; only} ->
     let ppf1 = self#location ppf0 location in
     let ppf2 = self#level ppf1 level in
     let ppf3 = List.fold_left
       self#expr_or_module_or_module_instance ppf2 facts in
     let ppf = List.fold_left
       self#defined_expr ppf3 defs in
     (* skip the only tag *)
     ppf
   | P_steps { location; level; steps; } ->
     List.fold_left self#step ppf0 steps
   | P_noproof -> ppf0

   method step ppf0 = function
   | S_def_step x -> self#def_step ppf0 x
   | S_use_or_hide x -> self#use_or_hide ppf0 x
   | S_instance i -> self#instance ppf0 i
   | S_theorem t -> self#theorem ppf0 t

   method use_or_hide ppf0 {  location; level; facts; defs; only; hide } =
     let ppf1 = self#location ppf0 location in
     let ppf2 = self#level ppf1 level in
     let ppf3 = List.fold_left
       self#expr_or_module_or_module_instance ppf2 facts in
     let ppf = List.fold_left
       self#defined_expr ppf3 defs in
     ppf

   method instance ppf0 {location; level; name; substs; params; } =
     let ppf1 = self#location ppf0 location in
     let ppf2 = self#level ppf1 level in
     let ppf3 = self#name ppf2 name in
     let ppf4 = List.fold_left self#subst ppf3 substs in
     let ppf = List.fold_left self#formal_param ppf4 params in
     ppf

   method subst ppf0 { op; expr } =
     let ppf1 = self#op_decl ppf0 op in
     let ppf = self#expr_or_op_arg ppf1 expr in
     ppf

   method assume_prove ppf0 { location; level; new_symbols; assumes;
                              prove; suffices; boxed; } =
     let ppf1 = self#location ppf0 location in
     let ppf2 = self#level ppf1 level in
     let ppf3 = List.fold_left
                  self#new_symb ppf2 new_symbols in
     let ppf4 = List.fold_left
                  self#assume_prove ppf3 assumes in
     let ppf = self#expr ppf4 prove in
     (* suffices and boxed are boolean flags*)
     ppf

   method new_symb ppf0 { location; level; op_decl; set } =
     let ppf1 = self#location ppf0 location in
     let ppf2 = self#level ppf1 level in
     let ppf3 = self#op_decl ppf2 op_decl in
     let ppf = match set with
       | None -> ppf3
       | Some e -> self#expr ppf3 e
     in ppf

   method let_in ppf0 {location; level; body; op_defs } =
     let ppf1 = self#location ppf0 location in
     let ppf2 = self#level ppf1 level in
     let ppf3 = self#expr ppf2 body in
     let ppf = List.fold_left self#op_def_or_theorem_or_assume ppf3 op_defs in
     ppf

   method subst_in ppf0 ({ location; level; substs; body } : subst_in) =
     let ppf1 = self#location ppf0 location in
     let ppf2 = self#level ppf1 level in
     let ppf3 = List.fold_left self#subst ppf2 substs in
     let ppf = self#expr ppf3 body in
     ppf

   method label ppf0 ({location; level; name; arity; body; params } : label) =
     let ppf1 = self#location ppf0 location in
     let ppf2 = self#level ppf1 level in
     let ppf3 = self#name ppf2 name in
     (* skip arity *)
     let ppf4 = self#assume_prove ppf3 body in
     let ppf = List.fold_left self#formal_param ppf4 params in
     ppf

   method ap_subst_in ppf0 ({ location; level; substs; body } : ap_subst_in) =
     let ppf1 = self#location ppf0 location in
     let ppf2 = self#level ppf1 level in
     let ppf3 = List.fold_left self#subst ppf2 substs in
     let ppf = self#node ppf3 body in
     ppf

   method def_step ppf0 { location; level; defs } =
     let ppf1 = self#location ppf0 location in
     let ppf2 = self#level ppf1 level in
     let ppf = List.fold_left self#op_def ppf2 defs in
     ppf

   method module_instance ppf0 = function
   | MI_ref x -> self#reference ppf0 x
   | MI {location; level; name} ->
     let ppf1 = self#location ppf0 location in
     let ppf2 = self#level ppf1 level in
     let ppf = self#name ppf2 name in
     ppf

   method builtin_op ppf0 = function
   | { level; name; arity; params } ->
     let ppf1 = self#level ppf0 level in
     let ppf2 = self#name ppf1 name in
   (* skip arity *)
     let ppf = List.fold_left
       (fun x (fp,_) -> self#formal_param x fp) ppf2 params
     in ppf

   method user_defined_op ppf0 = function
   | UOP_ref x -> self#reference ppf0 x
   | UOP { location; level ; name ; arity ;
           body ; params ; recursive ; } ->
     let ppf1 = self#location ppf0 location in
     let ppf2 = self#level ppf1 level in
     let ppf3 = self#name ppf2 name in
   (* arity *)
     let ppf4 = self#expr ppf3 body in
     let ppf = List.fold_left
       (fun x (fp,_) -> self#formal_param x fp) ppf4 params in
   (* skip recursive flag *)
     ppf

   method name ppf x = ppf

   method reference ppf x = ppf

   method context ppf { fp_entries; mod_entries; opdec_entries;
                        opdef_entries; theorem_entries; assume_entries;
                        apsubst_entries; modules } =
     let strip pack list = List.map (fun x -> pack (snd x)) list in
     let fp_strip = strip (fun x -> FP x) in
     let mod_strip = strip (fun x -> MOD x) in
     let opdef_strip = strip (fun x -> OPDef x) in
     let opdec_strip = strip (fun x -> OPD x) in
     let theorem_strip = strip (fun x -> THM x) in
     let assume_strip = strip (fun x -> ASSUME x) in
     let ap_strip = strip (fun x ->  x) in
     let ppf1 = List.fold_left self#formal_param ppf (fp_strip fp_entries) in
     let ppf2 = List.fold_left self#mule ppf1 (mod_strip mod_entries) in
     let ppf3 = List.fold_left self#op_decl ppf2 (opdec_strip opdec_entries) in
     let ppf4 = List.fold_left self#op_def ppf3 (opdef_strip opdef_entries) in
     let ppf5 =
       List.fold_left self#theorem ppf4 (theorem_strip theorem_entries) in
     let ppf6 = List.fold_left self#assume ppf5 (assume_strip assume_entries) in
     let ppf7 =
       List.fold_left self#ap_subst_in ppf6 (ap_strip apsubst_entries) in
     let ppf8 = List.fold_left self#mule ppf7 modules in
     ppf8

end
