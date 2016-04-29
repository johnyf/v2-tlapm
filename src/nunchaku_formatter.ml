open Commons
open Simple_expr_ds
open Simple_expr_visitor
open Simple_expr_utils
open Simple_expr_dereference
open Format
open Simple_expr_prover_parser
open Nunchaku_ast
       
type fc = statement list * simple_term_db * string
	    
(* these are extractors for the accumulator type  *)
let sta     ( sta, _, _) = sta
let tdb     ( _, tdb, _) = tdb
let str     ( _, _, str) = str
			     
let add_axiom l (sta, tdb, str) =
  let x = axiom l in
  (x::sta, tdb, str)

let add_decl v t (sta, tdb, str) =
  let x = decl [] v t in
  (x::sta, tdb, str)

let add_comm s (sta, tdb, str) =
  let x = comm s in
  (x::sta, tdb, str)

let add_include f (sta, tdb, str) =
  let x = include_ f in
  (x::sta, tdb, str)

let add_goal t (sta, tdb, str) =
  let x = goal t in
  (x::sta, tdb, str)
    
let set_string str (sta, tdb, _) = (sta, tdb, str)
				     
let add_to_string x (sta, tdb, str) = (sta, tdb, str^x)
				     
let acc_fold function_to_apply list_of_args acc =
   List.fold_right (fun arg_temp -> fun acc_temp -> function_to_apply acc_temp arg_temp) list_of_args acc
		      
class formatter =
object(self)
  inherit [fc] visitor as super

  (* parts of expressions *)
  method location acc { column; line; filename } : 'a =
    add_comm "Crossed location" acc

  method level acc l : 'a =
    add_comm "Crossed level" acc

  (* non-recursive expressions *)
  method decimal acc { location; level; mantissa; exponent;  } =
    let value =
      (float_of_int mantissa) /. ( 10.0 ** (float_of_int exponent)) in
    add_comm ("Crossed decimal = "^(string_of_float value)) acc

  method numeral acc {location; level; value } =
    add_comm ("Crossed numeral = "^(string_of_int value)) acc

  method strng acc {location; level; value} =
    add_comm ("Crossed string = "^value) acc

  method op_arg acc {location; level; argument } =
    self#operator acc argument

  (* recursive expressions *)
  method at acc {location; level; except; except_component} =
    add_comm "Crossed AT" acc
 
  method op_appl acc0 {location; level; operator; operands} =
    let acc1 = set_string "" acc0 in
    let acc = match match_infix_op (tdb acc1) operator with
      | true ->
         (* infix binary operators *)
         let left, right = match operands with
           | [l;r] -> l,r
           | _ -> failwith "Binary operator does not have 2 arguments!"
         in
	 let acc2 = add_to_string "(" acc1 in
         let acc3 = self#expr_or_op_arg acc2 left in
	 let acc4 = add_to_string " " acc3 in
         let acc5 = self#operator acc4 operator in
	 let acc6 = add_to_string " " acc5 in
         let acc7 = self#expr_or_op_arg acc6 right in
	 let acc8 = add_to_string ")" acc7 in
         acc8
      | false -> (
        match match_ternary_op (tdb acc1) operator with
        | Some name ->
           (* ternary binary operators *)
           let str_begin,str_middle,str_end = expand_ternary_name name in
           let op1, op2, op3 = match operands with
             | [o1;o2;o3] -> o1,o2,o3
             | _ -> failwith "Binary operator does not have 2 arguments!"
           in
      	   let acc2 = add_to_string ("( "^str_begin) acc1 in
           let acc3 = self#expr_or_op_arg acc2 op1 in
	   let acc4 = add_to_string (" "^str_middle^" ") acc3 in
           let acc5 = self#expr_or_op_arg acc4 op2 in
	   let acc6 = add_to_string (" "^str_end^" ") acc5 in
           let acc7 = self#expr_or_op_arg acc6 op3 in
	   let acc8 = add_to_string ")" acc7 in
           acc8
        | _ ->
           (* other operators *)
           let acc2 = self#operator acc1 operator in
           let oparens, cparens =
             if (operands <> []) then ("(",")") else ("","")
	   in
  	   let acc3= add_to_string oparens acc2 in
           let acc4 = acc_fold self#expr_or_op_arg operands acc3 in
  	   let acc5= add_to_string cparens acc4 in
           acc5
      )
    in
    add_comm ("Crossed "^(str acc)) acc

  method lambda acc {level; arity; body; params} =
    add_comm "Crossed LAMBDA" acc

  method binder acc0 {location; level; operator; operand; bound_symbols} =
    (* let acc1 = self#operator acc0 operator in *)
    (* fprintf (ppf acc3) " "; *)
    (* let acc4 = ppf_fold_with self#bound_symbol acc3 bound_symbols in *)
    (* fprintf (ppf acc4) " : "; *)
    (* let oparens, cparens = "(",")" in *)
    (* fprintf (ppf acc4) "%s" oparens; *)
    (* let acc5 = self#expr_or_op_arg acc3 operand in *)
    (* fprintf (ppf acc5) "%s" cparens; *)
    let acc1 = self#operator acc0 operator in
    add_comm ("Crossed "^(str acc1)) acc1

  method bounded_bound_symbol acc { params; tuple; domain; } =
    (* match params with *)
    (* | [] -> *)
    (*    failwith "Trying to process empty tuple of bound symbols with domain!" *)
    (* | [param] -> *)
    (*    if tuple then fprintf (ppf acc) "<<"; *)
    (*    let acc1 = self#formal_param acc param in *)
    (*    if tuple then fprintf (ppf acc1) ">> "; *)
    (*    fprintf (ppf acc1) " \\in "; *)
    (*    let acc2 = self#expr acc domain in *)
    (*    acc2 *)
    (* | _ -> *)
    (*    fprintf (ppf acc) "<<"; *)
    (*    let acc1 = ppf_fold_with self#formal_param acc params in *)
    (*    fprintf (ppf acc1) ">> \\in "; *)
    (*    let acc2 = self#expr acc domain in *)
    (*    acc2 *)
    acc
      
  method unbounded_bound_symbol acc { param; tuple } =
    (* if tuple then fprintf (ppf acc) "<<"; *)
    (* let acc1 = self#formal_param acc param in *)
    (* if tuple then fprintf (ppf acc1) ">> "; *)
    acc

  method formal_param acc fp =
    let { location; level; name; arity; } : simple_formal_param_ =
      dereference_formal_param (tdb acc) fp in
    add_comm ("Crossed formal_param"^name) acc

  method op_decl acc0 opdec =
    let { location ; level ; name ; arity ; kind ; } =
      dereference_op_decl (tdb acc0) opdec in
    let new_decl = match kind with
      | NewConstant -> "u" (* default is constant "CONSTANT " *)
      (* | NewVariable -> "VARIABLE " *)
      (* | NewState -> "STATE " *)
      (* | NewAction -> "ACTION " *)
      (* | NewTemporal -> "TEMPORAL " *)
      | _ -> failwith "declared new symbol with a non-new kind."
    in
    add_decl name (Unknown new_decl) acc0
    

  method op_def acc = function
     | O_builtin_op x      -> 
       self#builtin_op acc x
    | O_user_defined_op x ->
       let op = dereference_user_defined_op (tdb acc) x
       in
       add_comm op.name acc
	   
  method assume_prove acc0 { location; level; new_symbols; assumes;
                             prove; } =
    let acc1 = add_comm "the NEW SYMB part" acc0 in
    let acc2 = acc_fold self#new_symb new_symbols acc1 in 
    let acc3 = add_comm "the ASSUME part" acc2 in
    let acc4 = acc_fold self#assume_prove assumes acc3 in 
    let acc5 = add_comm "the PROVE part" acc4 in
    let acc6 = self#expr acc5 prove in
    let acc7 = add_goal (Unknown (str acc6)) acc6 in
    acc7

    
  (*TODO: new_decl not used *)
  method new_symb acc0 { location; level; op_decl; set } =
    (* let acc1 = self#location acc0 location in *)
    (* let acc2 = self#level acc1 level in *)
    let od = dereference_op_decl (tdb acc0) op_decl in
    let new_decl = match od.kind with
      | NewConstant -> "u" (* default is constant "CONSTANT " *)
      (* | NewVariable -> "VARIABLE " *)
      (* | NewState -> "STATE " *)
      (* | NewAction -> "ACTION " *)
      (* | NewTemporal -> "TEMPORAL " *)
      | _ -> failwith "declared new symbol with a non-new kind."
    in
    (* let acc3 = self#op_decl acc2 op_decl in *)
    let acc1 = match set with
      | None -> acc0
      | Some e -> acc0 (* TODO add axiom : mem x s *)
    in
    let acc2 = add_decl od.name (Unknown new_decl) acc1 in
    acc2

  method let_in acc0 {location; level; body; op_defs } =
    failwith "remove -let_in- during preprocessing"

  (* TODO *)
  method label acc0 ({location; level; name; arity; body; params } : simple_label) =
    (* let acc1 = self#location acc0 location in *)
    (* let acc2 = self#level acc1 level in *)
    (* let acc3 = self#name acc2 name in *)
    (* (\* skip arity *\) *)
    (* fprintf (ppf acc3) "(label)"; *)
    (* let acc4 = self#assume_prove acc3 body in *)
    (* let acc = List.fold_left self#formal_param acc4 params in *)
    acc0

  method builtin_op acc0 { level; name; arity; params } =
    (*    let acc1 = self#level acc0 level in *)
    (*    let acc2 = self#name acc1 name in *)
    (*    fprintf (ppf acc0) "%s" (self#translate_builtin_name name); *)
    add_to_string (self#translate_builtin_name name) acc0

  method user_defined_op acc0 op =
    (* let { location; level ; name ; arity ; *)
    (*       body ; params ; recursive } = *)
    (*   dereference_user_defined_op (tdb acc0) op in *)
    (* match nesting acc0, undef acc0, recursive with *)
    (* | _, true, false -> (\* expand the definition *\) *)
    (*    let acc1 = self#location acc0 location in *)
    (*    let acc2 = self#level acc1 level in *)
    (*    let acc4 = self#expr acc2 body in *)
    (*    acc4 *)
    (* | By, _, _ -> *)
    (*    let acc1 = self#location acc0 location in *)
    (*    let acc2 = self#level acc1 level in *)
    (*    fprintf (ppf acc2) "%s" name; *)
    (*    let acc3 = self#name acc2 name in *)
    (*    acc3 *)
    (* | _ -> (\* don't expand the definition  *\) *)
    (*    (\* TODO: recursive definitions are never unfolded at the moment *\) *)
    (*    let acc1 = self#location acc0 location in *)
    (*    let acc2 = self#level acc1 level in *)
    (*    fprintf (ppf acc2) "%s" name; *)
    (*    let acc3 = self#name acc2 name in *)
    (*    let fp_open, fp_close = if (params <> []) then "(",")" else "","" in *)
    (*    fprintf (ppf acc3) "%s" fp_open; *)
    (*    let acc = ppf_fold_with *)
    (*                (fun x fp -> self#formal_param x fp) acc3 params in *)
    (*    fprintf (ppf acc) "%s" fp_close; *)
       acc0

  method expr acc x = match x with
    | E_binder b -> self#binder acc b
    | E_op_appl b -> self#op_appl acc b
    | _ -> add_comm "Crossed unknown expression" acc

  method name acc x = acc

  method reference acc x = acc

  method translate_builtin_name = function
    (*    | "$AngleAct"  as x -> failwith ("Unknown operator " ^ x ^"!") *)
    (* | "$BoundedChoose" -> "CHOOSE" *)
    | "$BoundedExists" -> "exists"
    | "$BoundedForall" -> "forall"
    (* | "$Case" -> "CASE" *)
    (* | "$CartesianProd" -> "\times" *)
    | "$ConjList" -> "conj"
    | "$DisjList" -> "disj"
    (* | "$Except" -> "EXCEPT" *)
    (*    | "$FcnApply" as x -> failwith ("Unknown operator " ^ x ^"!") *)
    (*    | "$FcnConstructor"  as x -> failwith ("Unknown operator " ^ x ^"!") *)
    (* | "$IfThenElse" -> "IFTHENELSE" *)
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
    (* | "$TemporalExists" -> "\\EE" *)
    (* | "$TemporalForall" -> "\\AA" *)
    (* | "$UnboundedChoose" -> "CHOOSE" *)
    | "$UnboundedExists" -> "exists"
    | "$UnboundedForall" -> "forall"
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
    (* | "\\land" -> "/\\" *)
    (* | "\\lor" -> "\\/" *)
    | "TRUE" -> "true"
    | "FALSE" -> "false"
    | x -> x (* catchall case *)
end

let expr_formatter = new formatter

let mk_fmt (f : fc -> 'a -> fc) term_db (goal : 'a) =
  let acc = ([include_ "prelude.nun"], term_db, "") in
  let (l,_,_) = (f acc goal) in
  l
      
let fmt_expr = mk_fmt (expr_formatter#expr)

let fmt_assume_prove = mk_fmt (expr_formatter#assume_prove)

			    

			    
			    
