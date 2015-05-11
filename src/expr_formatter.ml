open Commons
open Expr_ds
open Expr_visitor
open Format
open List

type fc = Format.formatter * context

(* these are extractors for the accumulator type - could be defined as fst and
   snd, but if we extend the accumulator, we need to change it anyway  *)
let ppf (p, context) = p
let con (p, context) = context

(* folds the function f into the given list, but extracts the formatter from
   the accumulator and prints the string s after all but the last elements.  *)
let rec ppf_fold_with ?str:(s=",") f acc = function
  | [x] -> f acc x
  | x::xs ->
     let y = f acc x in
     fprintf (ppf acc) "%s" s;
     ppf_fold_with f y xs
  | [] -> acc

(** encloses the given string with parenthesis. can be used as a %a
    argument in a formatter.  *)
let ppf_parens ppf x = fprintf ppf "(%s)" x
(** encloses the given string with a formatting box. can be used as a %a
    argument in a formatter.  *)
let ppf_box ppf x = fprintf ppf "@[%s@]" x
(** prints the given string as it is. can be used as a %a
    argument in a formatter.  *)
let ppf_ident ppf x = fprintf ppf "%s" x

(** extracts the ppf from the given accumulator and outputs a newline *)
let ppf_newline acc = fprintf (ppf acc) "@\n"

class formatter =
object(self)
  inherit [fc] visitor as self


  (* parts of expressions *)
   method location acc l : 'a = acc
   method level acc l : 'a = acc

  (* non-recursive expressions *)
   method decimal acc { location; level; mantissa; exponent;  } =
     let value =
       (float_of_int mantissa) /. ( 10.0 ** (float_of_int exponent)) in
     fprintf (ppf acc) "%se%s" (string_of_float value);
     acc

   method numeral acc {location; level; value } =
     fprintf (ppf acc) "%s" (string_of_int value);
     acc

   method strng acc {location; level; value} =
     fprintf (ppf acc) "%s" value;
     acc

   method op_arg acc {location; level; name; arity } =
     fprintf (ppf acc) "%s" name;
     acc

(* recursive expressions *)
   method at acc0 {location; level; except; except_component} =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = self#op_appl acc2 except in
     let acc = self#op_appl acc3 except_component in
     (* todo make this better or manually remove the @ operators? *)
     fprintf (ppf acc) "@@" ;
     acc

   method op_appl acc0 {location; level; operator; operands; bound_symbols} =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = self#operator acc2 operator in
     let oparens, cparens = if (operands <> []) then ("(",")") else ("","") in
     fprintf (ppf acc3) "%s" oparens;
     let acc4 = ppf_fold_with self#expr_or_op_arg acc3 operands in
     fprintf (ppf acc4) "%s" cparens;
     fprintf (ppf acc4) "(* bound symbols: ";
     let acc = List.fold_left self#bound_symbol acc4 bound_symbols in
     fprintf (ppf acc) " *)";
     acc

   method bounded_bound_symbol acc { params; tuple; domain; } =
     match params with
     | [] ->
        failwith "Trying to process empty tuple of bound symbols with domain!"
     | [param] ->
        if tuple then fprintf (ppf acc) "<<";
        let acc1 = ppf_fold_with self#formal_param acc params in
        if tuple then fprintf (ppf acc1) ">> \in ";
        let acc2 = self#expr acc domain in
        acc2

   method unbounded_bound_symbol acc { param; tuple } =
     if tuple then fprintf (ppf acc) "<<";
     let acc1 = self#formal_param acc param in
     if tuple then fprintf (ppf acc1) ">> \in ";
     acc1

   method formal_param acc0 = function
     | FP_ref i ->
        let fps = (con acc0).fp_entries in
        let fp = assoc i fps in
        self#formal_param acc0 (FP fp)
     | FP { location; level; name; arity; } ->
        let acc1 = self#location acc0 location in
        let acc2 = self#level acc1 level in
        let acc3 = self#name acc0 name in
        fprintf (ppf acc3) "%s" name;
        (* arity skipped *)
        acc3

   method mule acc0 = function
     | MOD_ref i ->
        failwith "Implementation error: cannot dereference modules!"
     | MOD {name; location; constants; variables;
            definitions; assumptions; theorems; } ->
        fprintf (ppf acc0) "@[";
        fprintf (ppf acc0) "==== %s ====" name;
        (* let acc0a = self#name acc0 name in
           let acc1 = self#location acc0a location in *)
        fprintf (ppf acc0) "CONSTANTS ";
        let acc2 = ppf_fold_with self#op_decl acc0 constants in
        fprintf (ppf acc2) "@\n";
        fprintf (ppf acc2) "VARIABLES ";
        let acc3 = ppf_fold_with self#op_decl acc2 variables in
        fprintf (ppf acc2) "@\n";
        let acc4 = List.fold_left self#op_def acc3 definitions in
        let acc5 = List.fold_left self#assume acc4 assumptions in
        let acc = List.fold_left self#theorem acc5 theorems in
        fprintf (ppf acc2) "------------";
        fprintf (ppf acc2) "@]";
        acc


   method op_decl acc0 = function
     | OPD_ref x ->
        let opdefs = (con acc0).opdec_entries in
        let opd = assoc x opdefs in
        self#op_decl acc0 (OPD opd)
     | OPD  { location ; level ; name ; arity ; kind ; } ->
        let new_decl = match kind with
          (* the new is added in new_decl *)
          | NewConstant -> "CONSTANT "
          | NewVariable -> "VARIABLE "
          | NewState -> "STATE "
          | NewAction -> "ACTION "
          | NewTemporal -> "TEMPORAL "
          | _ -> ""
        in
        (* terminal node *)
        let acc1 = self#location acc0 location in
        let acc2 = self#level acc1 level in
        let acc3 = self#name acc2 name in
        fprintf (ppf acc3) "%s %s" new_decl name ;
        acc3

   method op_def acc = function
     | OPDef_ref x ->
        let ops = (con acc).opdef_entries  in
        let op = assoc x ops in
        self#op_def acc (OPDef op)
     | OPDef (O_module_instance x) -> self#module_instance acc x
     | OPDef (O_builtin_op x)      -> self#builtin_op acc x
     | OPDef (O_user_defined_op x) -> self#user_defined_op acc x

   method theorem acc0 = function
     | THM_ref x ->
        let thms = (con acc0).theorem_entries  in
        let thm = assoc x thms in
        self#theorem acc0 (THM thm)
     | THM { location; level; expr; proof; suffices } ->
        let acc1 = self#location acc0 location in
        let acc2 = self#level acc1 level in
        let s = if suffices then "SUFFICES " else "" in
        fprintf (ppf acc2) "%s" s;
        let acc3 = self#assume_prove acc2 expr in
        let acc4 = self#proof acc3 proof  in
        ppf_newline;
        acc4

   method assume acc0  = function
   | ASSUME_ref x -> self#reference acc0 x
   | ASSUME {location; level; expr; } ->
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     fprintf (ppf acc2) "ASSUME " ;
     let acc = self#expr acc2 expr in
     fprintf (ppf acc) "@\n" ;
     acc

   method proof acc0 = function
     | P_omitted location ->
        fprintf (ppf acc0) "OMITTED@\n";
        acc0
     | P_obvious location ->
        fprintf (ppf acc0) "OBVIOUS@\n";
        acc0
     | P_by { location; level; facts; defs; only } ->
        let acc1 = self#location acc0 location in
        let acc2 = self#level acc1 level in
        let by_only = if only then "BY ONLY " else "BY " in
        fprintf (ppf acc2) "%s" by_only;
        let acc3 = ppf_fold_with
                     self#expr_or_module_or_module_instance acc2 facts in
        let bydef = if (defs <> []) then "DEF " else "" in
        fprintf (ppf acc3) " %s" bydef;
        let acc = ppf_fold_with
                    self#defined_expr acc3 defs in
        ppf_newline acc;
        acc
     | P_steps { location; level; steps; } ->
        let acc = ppf_fold_with ~str:"@\n" self#step acc0 steps in
        ppf_newline acc;
        acc
     | P_noproof ->
        ppf_newline acc0;
        acc0

   method use_or_hide acc0 {  location; level; facts; defs; only; hide } =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let uoh = if hide then "HIDE " else "USE " in
     fprintf (ppf acc2) "%s" uoh;
     let acc3 = ppf_fold_with
                  self#expr_or_module_or_module_instance acc2 facts in
     let bydef = if (defs <> []) then "DEF " else "" in
     fprintf (ppf acc3) "%s" bydef;
     let acc = ppf_fold_with
                 self#defined_expr acc3 defs in
     ppf_newline acc;
     acc

   method instance acc0 {location; level; name; substs; params; } =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     fprintf (ppf acc2) "INSTANCE ";
     let acc3 = self#name acc2 name in
     fprintf (ppf acc2) " == ";
     let acc4 = List.fold_left self#subst acc3 substs in
     let acc = List.fold_left self#formal_param acc4 params in
     ppf_newline acc;
     acc

   method subst acc0 { op; expr } =
     let acc1 = self#op_decl acc0 op in
     fprintf (ppf acc1) " <- ";
     let acc = self#expr_or_op_arg acc1 expr in
     acc

   method assume_prove acc0 { location; level; new_symbols; assumes;
                              prove; suffices; boxed; } =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let s_suffices = if (suffices) then "SUFFICES ASSUME " else "ASSUME " in
     fprintf (ppf acc2) "%s" s_suffices;
     let acc3 = ppf_fold_with
                  self#new_symb acc2 new_symbols in
     fprintf (ppf acc2) " PROVE ";
     let acc4 = ppf_fold_with
                  self#assume_prove acc3 assumes in
     let acc = self#expr acc4 prove in
     ppf_newline acc;
     acc

   method new_symb acc0 { location; level; op_decl; set } =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     fprintf (ppf acc2) "NEW ";
     let acc3 = self#op_decl acc2 op_decl in
     let acc = match set with
       | None -> acc3
       | Some e ->
          fprintf (ppf acc3) " \in ";
          self#expr acc3 e
     in acc

   method let_in acc0 {location; level; body; op_defs } =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = self#expr acc2 body in
     let acc = List.fold_left self#op_def_or_theorem_or_assume acc3 op_defs in
     acc

   method subst_in acc0 ({ location; level; substs; body } : subst_in) =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = List.fold_left self#subst acc2 substs in
     let acc = self#expr acc3 body in
     acc

   method label acc0 ({location; level; name; arity; body; params } : label) =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = self#name acc2 name in
     (* skip arity *)
     let acc4 = self#assume_prove acc3 body in
     let acc = List.fold_left self#formal_param acc4 params in
     acc

   method ap_subst_in acc0 ({ location; level; substs; body } : ap_subst_in) =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = List.fold_left self#subst acc2 substs in
     let acc = self#node acc3 body in
     acc

   method def_step acc0 { location; level; defs } =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc = List.fold_left self#op_def acc2 defs in
     acc

   method module_instance acc0 = function
   | MI_ref x -> self#reference acc0 x
   | MI {location; level; name} ->
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc = self#name acc2 name in
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
           body ; params ; recursive ; } ->
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = self#name acc2 name in
   (* arity *)
     let acc4 = self#expr acc3 body in
     let acc = List.fold_left
       (fun x (fp,_) -> self#formal_param x fp) acc4 params in
   (* skip recursive flag *)
     acc

   method name acc x = acc

   method reference acc x = acc

   method context acc { fp_entries; mod_entries; opdec_entries;
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
     let acc1 = List.fold_left self#formal_param acc (fp_strip fp_entries) in
     let acc2 = List.fold_left self#mule acc1 (mod_strip mod_entries) in
     let acc3 = List.fold_left self#op_decl acc2 (opdec_strip opdec_entries) in
     let acc4 = List.fold_left self#op_def acc3 (opdef_strip opdef_entries) in
     let acc5 =
       List.fold_left self#theorem acc4 (theorem_strip theorem_entries) in
     let acc6 = List.fold_left self#assume acc5 (assume_strip assume_entries) in
     let acc7 =
       List.fold_left self#ap_subst_in acc6 (ap_strip apsubst_entries) in
     let acc8 = List.fold_left self#mule acc7 modules in
     acc8

end

let expr_formatter = new formatter
