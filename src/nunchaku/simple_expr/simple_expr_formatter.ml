open Commons
open Simple_expr_ds
open Simple_expr_visitor
open Simple_expr_utils
open Simple_expr_dereference
open Util
open List
open Format
open Simple_expr_prover_parser

type nesting = Module | Expression | ProofStep of int | By
(* We need to pass on the formatter, the contect for unfolding references and a
   flag if to unfold *)
type fc = Format.formatter * simple_term_db * bool * nesting * int

(* these are extractors for the accumulator type  *)
let ppf     ( ppf, _, _, _, _ ) = ppf
let tdb     ( _, db, _, _, _ ) = db
let undef   ( _, _, expand, _, _ ) = expand
let nesting ( _, _, _, n, _ ) = n
let ndepth  ( _, _, _,  _, n ) = n


(* sets the expand flag of the accumulator *)
let set_expand (x,y,_,n, d) v = (x,y,v,n,d)

(* sets the expand flag of the first accumulator the the expand flag of the
   second accumulator *)
let reset_expand (x,y,_,n,d) (_,_,v,_,_) = (x,y,v,n,d)

(* modifies the accumulator to turn on definition expansion *)
let enable_expand x = set_expand x true

(* modifies the accumulator to turn off definition expansion *)
let disable_expand x = set_expand x false


let set_nesting (x,y,z,_,u) n = (x,y,z,n,u)
let reset_nesting x (_,_,_,n,_) = set_nesting x n
let nest_module x = set_nesting x Module
let nest_expr x = set_nesting x Expression
let nest_proof x n = set_nesting x (ProofStep n)
let nest_by x = set_nesting x By

let set_ndepth (x,y,z,n,d) depth = (x,y,z,n,depth)
let reset_ndepth x (_,_,_,_,d) = set_ndepth x d
let inc_ndepth x = set_ndepth x ((ndepth x) + 1)


let comma_formatter channel _ =
  fprintf channel ", ";
  ()
let newline_formatter channel _ =
  fprintf channel "@, ";
  ()
let empty_formatter channel _ =
  fprintf channel "";
  ()

(* folds the function f into the given list, but extracts the formatter from
   the accumulator and prints the string s after all but the last elements.  *)
let rec ppf_fold_with ?str:(s=comma_formatter) f acc = function
  | [x] ->
    f acc x
  | x::xs ->
    let y = f acc x in
    fprintf (ppf y) "%a" s ();
    ppf_fold_with ~str:s f y xs
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


(** checks if a user defined operator is defined in a standard module
    (TLAPS, Naturals etc.) *)
let is_standard_location location =
  match location.filename with
  | "--TLA+ BUILTINS--" -> true
  | "TLAPS" -> true
  | "TLC" -> true
  | "Naturals" -> true
  | _ -> false

class formatter =
  object(self)
    inherit [fc] visitor as super

    (* parts of expressions *)
    method location acc { column; line; filename } : 'a =
    (*
    fprintf (ppf acc) "(%s:l%d-%d c%d-%d)"
            filename line.rbegin line.rend
            column.rbegin column.rend;
     *)
      acc
    method level acc l : 'a =
    (*
    let lstr = match l with
    | None -> "(no level)"
    | Some ConstantLevel -> "(Constant)"
    | Some VariableLevel -> "(Variable)"
    | Some ActionLevel -> "(Action)"
    | Some TemporalLevel -> "(Temporal)"
    in
    fprintf (ppf acc) "%s" lstr;
     *)
      acc

    (* non-recursive expressions *)
    method decimal acc { location; level; mantissa; exponent;  } =
      let value =
        (float_of_int mantissa) /. ( 10.0 ** (float_of_int exponent)) in
      fprintf (ppf acc) "%s" (string_of_float value);
      acc

    method numeral acc {location; level; value } =
      fprintf (ppf acc) "%s" (string_of_int value);
      acc

    method strng acc {location; level; value} =
      fprintf (ppf acc) "\"%s\"" value;
      acc

    method op_arg acc {location; level; argument } =
      (* fprintf (ppf acc) "%s" name;
         acc *)
      self#operator acc argument

    (* recursive expressions *)
    method at acc0 {location; level; except; except_component} =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = self#op_appl_or_binder acc2 except in
      let acc = self#op_appl_or_binder acc3 except_component in
      (* todo make this better or manually remove the @ operators? *)
      fprintf (ppf acc) "@@" ;
      acc

    method op_appl acc0 {location; level; operator; operands} =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let match_fixity_op term_db =  function
        | FMOTA_op_def (O_user_defined_op uop) ->
          let uopi = dereference_user_defined_op term_db uop in
          Expr_prover_parser.extract_fixity uopi.name uopi.params
        | FMOTA_op_def (O_builtin_op op) ->
          Expr_prover_parser.extract_fixity op.name op.params
        | FMOTA_formal_param _
        | FMOTA_op_decl _
        | FMOTA_lambda _ -> Expr_prover_parser.Standard
      in
      match match_fixity_op (tdb acc2) operator with
      | Expr_prover_parser.BinaryInfix ->
        (* infix binary operators *)
        fprintf (ppf acc2) "(";
        let left, right = match operands with
          | [l;r] -> l,r
          | _ -> failwith "Binary operator does not have 2 arguments!"
        in
        let acc3 = self#expr_or_op_arg acc2 left in
        fprintf (ppf acc3) " ";
        let acc4 = self#operator acc3 operator in
        fprintf (ppf acc4) " ";
        let acc5 = self#expr_or_op_arg acc4 right in
        fprintf (ppf acc5) ")";
        acc5
      | Expr_prover_parser.Special Expr_builtins.Builtin.IF_THEN_ELSE ->
        fprintf (ppf acc2) "(";
        let op1, op2, op3 = match operands with
          | [o1;o2;o3] -> o1,o2,o3
          | _ -> failwith "if-then-else operator does not have 3 arguments!"
        in
        fprintf (ppf acc2) "(IF ";
        let acc3 = self#expr_or_op_arg acc2 op1 in
        fprintf (ppf acc3) " THEN ";
        let acc4 = self#expr_or_op_arg acc3 op2 in
        fprintf (ppf acc4) " ELSE ";
        let acc5 = self#expr_or_op_arg acc4 op3 in
        fprintf (ppf acc5) ")";
        acc5
      | Expr_prover_parser.Special Expr_builtins.Builtin.FUN_APP ->
        (* TODO: not sure if the args need to be wrapped in prenthesis sometimes *)
        let f, args = match operands with
          | x::y::xs -> x, y::xs
          | _ -> failwith "Function application needs at least 2 arguments!"
        in
        let acc3 = self#expr_or_op_arg acc2 f in
        fprintf (ppf acc3) "[";
        let acc4 = ppf_fold_with self#expr_or_op_arg acc3 args in
        fprintf (ppf acc4) "]";
        acc4
      | Expr_prover_parser.Special Expr_builtins.Builtin.SET_ENUM ->
        fprintf (ppf acc2) "{";
        (* TODO: not sure if the args need to be wrapped in prenthesis sometimes *)
        let acc3 = ppf_fold_with self#expr_or_op_arg acc2 operands in
        fprintf (ppf acc3) "}";
        acc3
      | Expr_prover_parser.Special _
      | Expr_prover_parser.UnaryPrefix|Expr_prover_parser.UnaryPostfix
      | Expr_prover_parser.Standard ->
        (* other operators *)
        let acc3 = self#operator acc2 operator in
        let oparens, cparens =
          if (operands <> []) then ("(",")") else ("","") in
        fprintf (ppf acc3) "%s" oparens;
        let acc4 = ppf_fold_with self#expr_or_op_arg acc3 operands in
        fprintf (ppf acc4) "%s" cparens;
        acc4

    method lambda acc0 {level; arity; body; params} =
      let acc1 = self#level acc0 level in
      fprintf (ppf acc1) "LAMBDA ";
      let acc2 = ppf_fold_with
          (fun x (fp,_) -> self#formal_param x fp) acc1 params in
      fprintf (ppf acc1) " : (";
      let acc3 = self#expr acc2 body in
      fprintf (ppf acc1) ")";
      acc3

    method binder acc0 {location; level; operator; operand; bound_symbols} =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = self#operator acc2 operator in
      fprintf (ppf acc3) " ";
      let acc4 = ppf_fold_with self#bound_symbol acc3 bound_symbols in
      fprintf (ppf acc4) " : ";
      let oparens, cparens = "(",")" in
      fprintf (ppf acc4) "%s" oparens;
      let acc5 = self#expr_or_op_arg acc3 operand in
      fprintf (ppf acc5) "%s" cparens;
      acc4


    method bounded_bound_symbol acc { params; tuple; domain; } =
      match params with
      | [] ->
        failwith "Trying to process empty tuple of bound symbols with domain!"
      | [param] ->
        if tuple then fprintf (ppf acc) "<<";
        let acc1 = self#formal_param acc param in
        if tuple then fprintf (ppf acc1) ">> ";
        fprintf (ppf acc1) " \\in ";
        let acc2 = self#expr acc domain in
        acc2
      | _ ->
        fprintf (ppf acc) "<<";
        let acc1 = ppf_fold_with self#formal_param acc params in
        fprintf (ppf acc1) ">> \\in ";
        let acc2 = self#expr acc domain in
        acc2

    method unbounded_bound_symbol acc { param; tuple } =
      if tuple then fprintf (ppf acc) "<<";
      let acc1 = self#formal_param acc param in
      if tuple then fprintf (ppf acc1) ">> ";
      acc1

    method formal_param acc0 fp =
      let { location; level; name; arity; } : simple_formal_param_ =
        dereference_formal_param (tdb acc0) fp in
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = self#name acc2 name in
      fprintf (ppf acc3) "%s" name;
      (* arity skipped *)
      acc3

    method op_decl acc0 opdec =
      let { location ; level ; name ; arity ; kind ; } =
        dereference_op_decl (tdb acc0) opdec in
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      (* let acc3 = self#name acc2 name in *)
      let acc3 = match nesting acc0 with
        | Module ->
          (* terminal node *)
          let declaration_string = match kind with
            | ConstantDecl -> "CONSTANT"
            | VariableDecl -> "VARIABLE"
            | _ ->
              failwith "Global declaration can only be CONSTANT or VARIABLE."
          in
          fprintf (ppf acc2) "%s %s" declaration_string name ;
          ppf_newline acc2;
          acc2
        | Expression ->
          (* the kind is only relevant in the new_symb rule *)
          (* terminal node *)
          fprintf (ppf acc2) "%s" name ;
          acc2
        | ProofStep _ ->
          failwith "Operator declarations don't happen as a proof step!"
        | By ->
          failwith "Operator declarations don't happen in a BY statement!"
      in acc3

    method op_def acc = function
      | O_builtin_op x      ->
        self#builtin_op acc x
      | O_user_defined_op x ->
        let op = dereference_user_defined_op (tdb acc) x
        in
        match nesting acc, undef acc, is_standard_location op.location with
        | Module, _, true ->
          (* skip standard libraries *)
          acc
        | Module, _, _ ->
          let acc0 = disable_expand acc in
          let acc0a = self#user_defined_op acc0 x in
          let acc0b = enable_expand acc0a in
          fprintf (ppf acc0b) " == ";
          let acc1 = nest_expr acc0b in
          let acc2 = self#user_defined_op acc1 x in
          let acc3 = reset_nesting acc2 acc in
          let acc4 = reset_expand acc3 acc in
          ppf_newline acc4;
          acc4
        | Expression, true, _ ->
          let acc1 = nest_expr acc in
          let acc2 = self#user_defined_op acc1 x in
          let acc3 = reset_nesting acc2 acc in
          acc3
        | Expression, false, _ ->
          fprintf (ppf acc) "%s" op.name;
          acc
        | By, _, _ ->
          let acc0 = disable_expand acc in
          let acc0a = self#user_defined_op acc0 x in
          let acc0b = reset_expand acc0a acc in
          acc0b
        | ProofStep depth, _, _ ->
          (* TODO: check if this is ok *)
          let acc0 = disable_expand acc in
          let acc0a = self#user_defined_op acc0 x in
          let acc0b = reset_expand acc0a acc in
          acc0b
  (*
          failwith ("TODO: implement printing of op definitions in " ^
                    "proof step environments.")
   *)


    method assume_prove acc0 { location; level; new_symbols; assumes;
                               prove; } =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let s_suffices, s_prove = match (new_symbols, assumes) with
        | ( [], []) -> "", ""  (* empty antecedent *)
        | ( _,  _) ->  "ASSUME ", " PROVE "
      in
      fprintf (ppf acc2) "%s" s_suffices;
      let acc3 = ppf_fold_with
          self#new_symb acc2 new_symbols in
      let sep = if (new_symbols <> []) then ", " else "" in
      fprintf (ppf acc3) "%s" sep;
      let acc4 = ppf_fold_with
          self#assume_prove acc3 assumes in
      fprintf (ppf acc2) "%s" s_prove;
      let acc = self#expr acc4 prove in
      (* ppf_newline acc; *)
      acc

    (*TODO: new_decl not used *)
    method new_symb acc0 { location; level; op_decl; set } =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let od = dereference_op_decl (tdb acc0) op_decl in
      let new_decl = match od.kind with
        | NewConstant -> "" (* default is constant "CONSTANT " *)
        | NewVariable -> "VARIABLE "
        | NewState -> "STATE "
        | NewAction -> "ACTION "
        | NewTemporal -> "TEMPORAL "
        | _ -> failwith "declared new symbol with a non-new kind."
      in
      fprintf (ppf acc2) "NEW %s" new_decl;
      let acc3 = self#op_decl acc2 op_decl in
      let acc = match set with
        | None -> acc3
        | Some e ->
          fprintf (ppf acc3) " \\in ";
          self#expr acc3 e
      in acc

    (* TODO *)
    method let_in acc0 {location; level; body; op_defs } =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = self#expr acc2 body in
      let acc = List.fold_left self#op_def_or_theorem_or_assume acc3 op_defs in
      acc

    (* TODO *)
    method label acc0 ({location; level; name; arity; body; params } : simple_label) =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = self#name acc2 name in
      (* skip arity *)
      fprintf (ppf acc3) "(label)";
      let acc4 = self#assume_prove acc3 body in
      let acc = List.fold_left self#formal_param acc4 params in
      acc

    method builtin_op acc0 = function
      | { level; name; arity; params } ->
        let acc1 = self#level acc0 level in
        let acc2 = self#name acc1 name in
        fprintf (ppf acc0) "%s" (self#translate_builtin_name name);
        acc2

    method user_defined_op acc0 op =
      let { location; level ; name ; arity ;
            body ; params ; recursive } =
        dereference_user_defined_op (tdb acc0) op in
      match nesting acc0, undef acc0, recursive with
      | _, true, false -> (* expand the definition *)
        let acc1 = self#location acc0 location in
        let acc2 = self#level acc1 level in
        let acc4 = self#expr acc2 body in
        acc4
      | By, _, _ ->
        let acc1 = self#location acc0 location in
        let acc2 = self#level acc1 level in
        fprintf (ppf acc2) "%s" name;
        let acc3 = self#name acc2 name in
        acc3
      | _ -> (* don't expand the definition  *)
        (* TODO: recursive definitions are never unfolded at the moment *)
        let acc1 = self#location acc0 location in
        let acc2 = self#level acc1 level in
        fprintf (ppf acc2) "%s" name;
        let acc3 = self#name acc2 name in
        let fp_open, fp_close = if (params <> []) then "(",")" else "","" in
        fprintf (ppf acc3) "%s" fp_open;
        let acc = ppf_fold_with
            (fun x fp -> self#formal_param x fp) acc3 params in
        fprintf (ppf acc) "%s" fp_close;
        acc


    method name acc x = acc

    method reference acc x = acc

    method translate_builtin_name = function
      (*    | "$AngleAct"  as x -> failwith ("Unknown operator " ^ x ^"!") *)
      | "$BoundedChoose" -> "CHOOSE"
      | "$BoundedExists" -> "\\E"
      | "$BoundedForall" -> "\\A"
      | "$Case" -> "CASE"
      | "$CartesianProd" -> "\times"
      | "$ConjList" -> "/\\"
      | "$DisjList" -> "\\/"
      | "$Except" -> "EXCEPT"
      (*    | "$FcnApply" as x -> failwith ("Unknown operator " ^ x ^"!") *)
      (*    | "$FcnConstructor"  as x -> failwith ("Unknown operator " ^ x ^"!") *)
      | "$IfThenElse" -> "IFTHENELSE"
      (*    | "$NonRecursiveFcnSpec"  as x -> failwith ("Unknown operator " ^ x ^"!")
            | "$Pair"  as x -> failwith ("Unknown operator " ^ x ^"!")
            | "$RcdConstructor"  as x -> failwith ("Unknown operator " ^ x ^"!")
            | "$RcdSelect"  as x -> failwith ("Unknown operator " ^ x ^"!")
            | "$RecursiveFcnSpec"  as x -> failwith ("Unknown operator " ^ x ^"!")
            | "$Seq"  as x -> failwith ("Unknown operator " ^ x ^"!")
            | "$SquareAct"  as x -> failwith ("Unknown operator " ^ x ^"!")
            | "$SetEnumerate"  as x -> failwith ("Unknown operator " ^ x ^"!")
            | "$SF"  as x -> failwith ("Unknown operator " ^ x ^"!")
            | "$SetOfAll" as x  -> failwith ("Unknown operator " ^ x ^"!")
            | "$SetOfRcds" as x  -> failwith ("Unknown operator " ^ x ^"!")
            | "$SetOfFcns" as x  -> failwith ("Unknown operator " ^ x ^"!")
            | "$SubsetOf" as x  -> failwith ("Unknown operator " ^ x ^"!")
            | "$Tuple" as x  -> failwith ("Unknown operator " ^ x ^"!") *)
      | "$TemporalExists" -> "\\EE"
      | "$TemporalForall" -> "\\AA"
      | "$UnboundedChoose" -> "CHOOSE"
      | "$UnboundedExists" -> "\\E"
      | "$UnboundedForall" -> "\\A"
      (*    | "$WF" as x  -> failwith ("Unknown operator " ^ x ^"!")
            | "$Nop" as x -> failwith ("Unknown operator " ^ x ^"!") *)
      (*    | "$Qed" -> "QED" *)
      (*    | "$Pfcase" -> "CASE" *)
      (*    | "$Have" -> "HAVE" *)
      (*    | "$Take" -> "TAKE" *)
      (*    | "$Pick" -> "PICK" *)
      (*    | "$Witness" -> "WITNESS" *)
      (*    | "$Suffices" -> "SUFFICES" *)
      (* manual additions *)
      | "\\land" -> "/\\"
      | "\\lor" -> "\\/"
      | x -> x (* catchall case *)
  end

let expr_formatter = new formatter

let mk_fmt (f : fc -> 'a -> fc) term_db channel (expr : 'a) =
  let fmt = formatter_of_out_channel channel in
  let acc = (fmt, term_db, true, Expression, 0) in
  ignore (f acc expr);
  fprintf fmt "@."

let fmt_expr = mk_fmt (expr_formatter#expr)

let fmt_assume_prove = mk_fmt (expr_formatter#assume_prove)
