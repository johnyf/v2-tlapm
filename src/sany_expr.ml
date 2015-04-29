(** Provides a conversion from the xml datastructures which directly mirror
    the sany datastructures to the internal ones defined in Expr_ds.mli

    The AnyExpr type is a disjunction on all types defined in sany_ds and
    is used as the accumulator type in the converter visitor. The interface
    unwraps the results of converter for a few particular types.
 *)
open Expr_ds
open Commons


type anyExpr =
  | Nothing
  | Any_location of location
  | Any_level of level option
  | Any_name of string
  | Any_reference of int
  | Any_node of node
  | Any_expr of expr
  | Any_expr_or_oparg of expr_or_op_arg
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
  | Any_obcious of obvious
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
  | Any_bound_symbol of bound_symbol
  | Any_unbounded_bound_symbol of unbounded_bound_symbol
  | Any_bounded_bound_symbol of bounded_bound_symbol
  | Any_mule of mule
  | Any_mule_ of  mule_
  | Any_context of context
  | Any_entry of dentry

and dentry = {
	fp    : (int * formal_param_) list;
	m     : (int * mule_)  list;
	opd   : (int * op_def_) list;
	opdec : (int * op_decl_) list;
	t     : (int * theorem_) list;
	a     : (int * assume_) list;
	aps   : (int * ap_subst_in) list;
      }

let unfold_assumption (Any_assume x) = x
let unfold_assume_prove (Any_assume_prove x) = x
let unfold_bs (Any_bound_symbol s) = s
let unfold_def  (Any_defined_expr x) = x
let unfold_entry (Any_entry x) = x
let unfold_expr (Any_expr x) = x
let unfold_e_o (Any_expr_or_oparg e) = e
let unfold_fact  (Any_expr_or_module_or_module_instance x) = x
let unfold_formal_param (Any_formal_param x) = x
let unfold_module (Any_mule x) = x
let unfold_new_symb (Any_new_symb x) = x
let unfold_od_t_a (Any_op_def_or_theorem_or_assume x) = x
let unfold_op_decl (Any_op_decl x) = x
let unfold_op_def (Any_op_def x) = x
let unfold_theorem (Any_theorem x) = x
let unfold_steps  (Any_step x) = x
let unfold_subst (Any_subst x) = x



type builtin_store = (int * builtin_op) list

(**
  Applies the function f to the element y with the accumulator extracted from x.
  Returns a pair of the function result prepended to the list of results
  extracted from x together with the last accumulator state.

  The idea is to lift f to lists of any expressions instead of expressions only.
  Then it can be used to lift a fold.
 *)
let lift (f : ('b * 'c) -> 'a -> ('b * 'c))
	 (x : ('b list * 'c))
	 (y : 'a) = match x with
  | list, racc ->
     let ne, nacc = f (Nothing, racc) y in
     (ne::list, nacc)

(** Defines a fold_left with a folded function of type
    ('a * 'b) -> ('a list *'b). The function f of type
    ('a * 'b) -> 'c -> ('a * 'b) is lifted, such that the result 'a is
    collected in a list and 'b is passed through as an argument.
*)
let fold f (anyexpr, acc0) l unwrap =
  let (anylist, acc) =
    List.fold_left (lift f) ([],acc0) l  in
  (* lift produces a reversed list, turn it around before unwrapping *)
  let unwrapped_list = List.map unwrap (List.rev anylist) in
  (unwrapped_list, acc)

let extract_location (expr : Sany_ds.expr) = match expr with
  | Sany_ds.E_at { Sany_ds.location; level } -> location
  | Sany_ds.E_decimal { Sany_ds.location; level; } -> location
  | Sany_ds.E_label { Sany_ds.location; level; } -> location
  | Sany_ds.E_let_in { Sany_ds.location; level; } -> location
  | Sany_ds.E_numeral { Sany_ds.location; level; } -> location
  | Sany_ds.E_op_appl { Sany_ds.location; level; } -> location
  | Sany_ds.E_string { Sany_ds.location; level; } -> location
  | Sany_ds.E_subst_in { Sany_ds.location; level; } -> location

(** wraps an expression into an assume-prove with empty assumptions *)
let assume_proves_from_expr (expr : Sany_ds.expr) =  {
    Sany_ds.location = extract_location expr;
    level = None;
    assumes = [];
    prove = expr;
    suffices = false;
    boxed = false; (* TODO: check if this is true *)
  }


(**
 This visitor converts sany_ds datastructures into expr_ds datastructures. Since
 builtin operators or unfolded, we pass an association list builtin_store in the
 accumulator. As a return value, we are actually interested in an expression,
 so we need to use the anyExpr data type to have a uniform type.

 Apart from the methods: builtin_op, expr_or_assume_prove, context, entry
 (todo: fill in), the methods are just recreating the sany datastructure as
 internal datastructure.
 *)
class converter = object(self)
  inherit [anyExpr * builtin_store] Sany_visitor.visitor as super

  method node acc = function
    | Sany_ds.N_ap_subst_in x  ->
       let (Any_ap_subst_in x, acc0) = self#ap_subst_in acc x in
       (Any_node (N_ap_subst_in x),acc0)
    | Sany_ds.N_assume_prove x ->
       let (Any_assume_prove x, acc0) = self#assume_prove acc x in
       (Any_node (N_assume_prove x), acc0)
    | Sany_ds.N_def_step x     ->
       let (Any_def_step x, acc0) = self#def_step acc x in
       (Any_node (N_def_step x), acc0)
    | Sany_ds.N_expr x         ->
       let (Any_expr x, acc0) = self#expr acc x in
       (Any_node (N_expr x), acc0)
    | Sany_ds.N_op_arg x       ->
       let (Any_op_arg x, acc0) = self#op_arg acc x in
       (Any_node (N_op_arg x), acc0)
    | Sany_ds.N_instance x     ->
       let (Any_instance x, acc0) = self#instance acc x in
       (Any_node (N_instance x), acc0)
    | Sany_ds.N_new_symb x     ->
       let (Any_new_symb x, acc0) = self#new_symb acc x in
       (Any_node (N_new_symb x), acc0)
    | Sany_ds.N_proof x        ->
       let (Any_proof x, acc0) = self#proof acc x in
       (Any_node (N_proof x), acc0)
    | Sany_ds.N_formal_param x ->
       let (Any_formal_param x, acc0) = self#formal_param acc x in
       (Any_node (N_formal_param x), acc0)
    | Sany_ds.N_module x       ->
       let (Any_mule x, acc0) = self#mule acc x in
       (Any_node (N_module x), acc0)
    | Sany_ds.N_op_decl x      ->
       let (Any_op_decl x, acc0) = self#op_decl acc x in
       (Any_node (N_op_decl x), acc0)
    | Sany_ds.N_op_def x       ->
       let (Any_op_def x, acc0) = self#op_def acc x in
       (Any_node (N_op_def x), acc0)
    | Sany_ds.N_assume x       ->
       let (Any_assume x, acc0) = self#assume acc x in
       (Any_node (N_assume x), acc0)
    | Sany_ds.N_theorem x      ->
       let (Any_theorem x, acc0) = self#theorem acc x in
       (Any_node (N_theorem x), acc0)
    | Sany_ds.N_use_or_hide x  ->
       let (Any_use_or_hide x, acc0) = self#use_or_hide acc x in
       (Any_node (N_use_or_hide x), acc0)

  (* parts of expressions *)
  method location (_, acc) = function
    | None -> (Any_location mkDummyLocation, acc)
    | Some l -> (Any_location l, acc)

  method level (_, acc) l = (Any_level l, acc)

  (* non-recursive expressions *)
  method decimal acc0 {Sany_ds.location; level; mantissa; exponent} =
    let Any_location location, acc = self#location acc0 location in
    let d = {
	location ;
	level ;
	mantissa ;
	exponent ;
      } in
    (Any_decimal d, acc)

  method numeral acc0 ({Sany_ds.location; level; value} : Sany_ds.numeral) =
    let Any_location location, acc = self#location acc0 location in
    let n:numeral = {
	location ;
	level ;
	value ;
      } in
    (Any_numeral n, acc)

  method strng acc0 ({Sany_ds.location; level; value} : Sany_ds.strng) =
    let Any_location location, acc = self#location acc0 location in
    let s:strng = {
	location ;
	level ;
	value ;
      } in
    (Any_strng s, acc)

  (* recursive expressions *)
   method at acc0 {Sany_ds.location; level; except; except_component} =
     let Any_location location, acc1 = self#location acc0 location in
     let Any_level level      , acc2 = self#level (Nothing, acc1) level in
     let Any_op_appl except   , acc3 = self#op_appl (Nothing, acc2) except in
     let Any_op_appl except_component, acc  =
       self#op_appl (Nothing, acc3) except_component in
     let at = {
	 location ;
	 level ;
	 except ;
	 except_component ;
       } in
     (Any_at at, acc)

   method op_appl acc0 ({Sany_ds.location; level; operator;
			      operands; bound_symbols} : Sany_ds.op_appl) =
     let Any_location location, acc1 = self#location acc0 location in
     let Any_level level,       acc2 = self#level (Nothing, acc1) level in
     let Any_operator operator, acc3 = self#fmota (Nothing, acc2) operator in
     let operands, acc4 =
       fold self#expr_or_op_arg (Nothing, acc3) operands unfold_e_o in
     let bound_symbols, acc =
       fold self#bound_symbol (Nothing, acc4) bound_symbols unfold_bs in
     let op_appl = {
	 location ;
	 level ;
	 operator ;
	 operands ;
	 bound_symbols ;
       } in
     (Any_op_appl op_appl, acc)

   method bound_symbol acc0 = function
     | Sany_ds.B_bounded_bound_symbol s ->
	let Any_bounded_bound_symbol b,acc =
	  self#bounded_bound_symbol acc0 s in
	(Any_bound_symbol (B_bounded_bound_symbol b), acc)
     | Sany_ds.B_unbounded_bound_symbol s ->
	let Any_unbounded_bound_symbol b, acc =
	  self#unbounded_bound_symbol acc0 s in
	(Any_bound_symbol (B_unbounded_bound_symbol b), acc)

   method bounded_bound_symbol acc0 { Sany_ds.params; tuple; domain }  =
     let params, acc1 =
       fold self#formal_param acc0 params unfold_formal_param in
     let Any_expr domain, acc = self#expr (Nothing, acc1) domain in
     let b = {
	 params ;
	 tuple ;
	 domain ;
       } in
     (Any_bounded_bound_symbol b, acc)

   method unbounded_bound_symbol acc0 { Sany_ds.param; tuple }  =
     let Any_formal_param param, acc = self#formal_param acc0 param  in
     let b = {
	 param ;
	 tuple ;
       } in
     (Any_unbounded_bound_symbol b, acc)


   method formal_param acc0 = function
     | Sany_ds.FP_ref i ->
	(Any_formal_param (FP_ref i), snd acc0)
     | Sany_ds.FP { Sany_ds.location; level; name; arity; } ->
	let Any_location location, acc1 = self#location acc0 location in
	let Any_level level,       acc2 = self#level (Nothing, acc1) level in
	let fp = {
	    location ;
	    level ;
	    name ;
	    arity ;
	  }
	in (Any_formal_param (FP fp), acc2)

   method mule acc0 = function
     | Sany_ds.MOD_ref i ->
	(Any_mule (MOD_ref i), snd acc0)
     | Sany_ds.MOD { Sany_ds.name; location; constants; variables;
		     definitions; assumptions; theorems; } ->
	let Any_location location, acc1 = self#location acc0 location in
	let constants, acc2 =
	  fold self#op_decl (Nothing, acc1) constants unfold_op_decl in
	let variables, acc3 =
	  fold self#op_decl (Nothing, acc2) variables unfold_op_decl in
	let definitions, acc4 =
	  fold self#op_def (Nothing, acc3) definitions unfold_op_def in
	let assumptions, acc5 =
	  fold self#assume (Nothing, acc4) assumptions unfold_assumption in
	let theorems, acc =
	  fold self#theorem (Nothing, acc5) theorems unfold_theorem in
	let m = MOD {
	    name ;
	    location ;
	    constants ;
	    variables ;
	    definitions ;
	    assumptions ;
	    theorems ;
	  } in
	(Any_mule m, acc)

   method op_arg acc0 { Sany_ds.location; level; name; arity } =
     let Any_location location, acc1 = self#location acc0 location in
     let Any_level level,       acc2 = self#level (Nothing, acc1) level in
     let oparg = ({
	 location ;
	 level ;
	 name ;
	 arity ;
       } : op_arg)  in
     (Any_op_arg oparg, acc2)


   method op_decl acc0 = function
     | Sany_ds.OPD_ref x ->
	(Any_op_decl (OPD_ref x), snd acc0)
   | Sany_ds.OPD  { Sany_ds.location ; level ; name ; arity ; kind ; } ->
   (* terminal node *)
     let Any_location location, acc1 = self#location acc0 location in
     let Any_level level,       acc2 = self#level (Nothing, acc1) level in
     let opdec = OPD {
	 location ;
	 level ;
	 name ;
	 arity ;
	 kind ;
       } in
     (Any_op_decl opdec, acc2)

   method op_def acc0 = function
     | Sany_ds.OPDef_ref x -> (Any_op_def (OPDef_ref x), snd acc0)
     | Sany_ds.OPDef (Sany_ds.O_module_instance x) ->
	let Any_module_instance mi, acc = self#module_instance acc0 x in
	(Any_op_def (OPDef (O_module_instance mi)), acc)
     | Sany_ds.OPDef (Sany_ds.O_builtin_op x)      ->
	let Any_builtin_op bi, acc = self#builtin_op acc0 x in
	(Any_op_def (OPDef (O_builtin_op bi)), acc)
     | Sany_ds.OPDef (Sany_ds.O_user_defined_op x) ->
	let Any_user_defined_op op, acc = self#user_defined_op acc0 x in
	(Any_op_def (OPDef (O_user_defined_op op)), acc)

   method theorem acc0 = function
     | Sany_ds.THM_ref x -> (Any_theorem (THM_ref x), snd acc0)
     | Sany_ds.THM { Sany_ds.location; level; expr; proof; suffices } ->
	let Any_location location, acc1 = self#location acc0 location in
	let Any_level level,       acc2 = self#level (Nothing, acc1) level in
	let ap = match expr with
	  | EA_expr e -> assume_proves_from_expr e
	  | EA_assume_prove ap -> ap in
	let Any_assume_prove expr, acc3 =
	  self#assume_prove (Nothing,acc2) ap in
	let Any_proof proof, acc4 = self#proof (Nothing, acc3) proof  in
	let t = {
	    location;
	    level;
	    expr;
	    proof;
	    suffices;
	  } in
	(Any_theorem (THM t), acc4)

   method assume acc0  = function
   | Sany_ds.ASSUME_ref x -> (Any_assume (ASSUME_ref x), snd acc0)
   | Sany_ds.ASSUME {Sany_ds.location; level; expr; } ->
     let Any_location location, acc1 = self#location acc0 location in
     let Any_level level,       acc2 = self#level (Nothing, acc1) level in
     let Any_expr expr,         acc = self#expr (Nothing, acc2) expr in
     let a = { location; level; expr;  } in
     (Any_assume (ASSUME a), acc)

   method proof acc0 = function
     | Sany_ds.P_omitted { Sany_ds.location; level } ->
	let Any_location location, acc1 = self#location acc0 location in
	let Any_level level,       acc  = self#level (Nothing, acc1) level in
	let o = ({
	    location ;
	    level ;
	  } : omitted) in
	(Any_proof (P_omitted o), snd acc0)
     | Sany_ds.P_obvious { Sany_ds.location; level } ->
	let Any_location location, acc1 = self#location acc0 location in
	let Any_level level,       acc  = self#level (Nothing, acc1) level in
	let o = ({
	    location ;
	    level ;
	  } : obvious) in
	(Any_proof (P_obvious o), snd acc0)
     | Sany_ds.P_by { Sany_ds.location; level; facts; defs; only } ->
	let Any_location location, acc1 = self#location acc0 location in
	let Any_level level, acc2 = self#level (Nothing, acc1) level in
	let facts, acc3 =
	  fold self#expr_or_module_or_module_instance
	       (Nothing, acc2) facts unfold_fact in
	let defs, acc =
	  fold self#user_defined_op_or_module_instance_or_theorem_or_assume
	       (Nothing, acc3) defs unfold_def in
	let by = {
	    location ;
	    level ;
	    facts ;
	    defs ;
	    only ;
	  } in
	(Any_proof (P_by by), acc)
     | Sany_ds.P_steps { Sany_ds.location; level; steps; } ->
	let Any_location location, acc1 = self#location acc0 location in
	let Any_level level, acc2 = self#level (Nothing, acc1) level in
	let steps, acc3 =
	  fold self#step (Nothing, acc2) steps unfold_steps in
	let s = {
	    location = location;
	    level = level;
	    steps = steps;
	  } in
	(Any_proof (P_steps s), acc3)

     | Sany_ds.P_noproof -> ( Any_proof P_noproof, snd acc0)

   method step acc0 = function
     | Sany_ds.S_def_step x ->
	let Any_def_step def_step, acc = self#def_step acc0 x in
	(Any_step (S_def_step def_step), acc)
     | Sany_ds.S_use_or_hide x ->
	let Any_use_or_hide use_or_hide, acc = self#use_or_hide acc0 x in
	(Any_step (S_use_or_hide use_or_hide), acc)
     | Sany_ds.S_instance i ->
	let Any_instance i, acc = self#instance acc0 i in
	(Any_step (S_instance i), acc)
     | Sany_ds.S_theorem t ->
	let Any_theorem t, acc = self#theorem acc0 t in
	(Any_step (S_theorem t), acc)

   method use_or_hide acc0 {  Sany_ds.location; level;
			      facts; defs; only; hide } =
     let Any_location location, acc1 = self#location acc0 location in
     let Any_level level, acc2 = self#level (Nothing, acc1) level in
     let facts, acc3 = fold self#expr_or_module_or_module_instance
			    (Nothing, acc2) facts  unfold_fact in
     let defs, acc =
       fold self#user_defined_op_or_module_instance_or_theorem_or_assume
	    (Nothing, acc3) defs unfold_def in
     let uoh = { location ; level ; facts ; defs ; only ; hide ; } in
     (Any_use_or_hide uoh, acc)

   method instance acc0 {Sany_ds.location; level; name; substs; params; } =
     let Any_location location, acc1 = self#location acc0 location in
     let Any_level level, acc2 = self#level (Nothing, acc1) level in
     let substs, acc3 = fold self#subst (Nothing, acc2) substs unfold_subst in
     let params, acc =
       fold self#formal_param (Nothing, acc3) params unfold_formal_param in
     let i = {	 location ; level ; name ; substs ; params ; }  in
     (Any_instance i, acc)

   method subst acc0 { Sany_ds.op; expr } =
     let Any_op_decl op, acc1 = self#op_decl acc0 op in
     let Any_expr_or_oparg expr, acc =
       self#expr_or_op_arg (Nothing, acc1) expr in
     let s = { op = op; expr = expr; } in
     (Any_subst s, acc)

   method assume_prove acc0 { Sany_ds.location; level; assumes;
			      prove; suffices; boxed; } =
     let Any_location location, acc1 = self#location acc0 location in
     let Any_level level, acc2 = self#level (Nothing, acc1) level in
     (* convert expr to assume_prove *)
     let sany_assumes2 =
       List.map (function
		  | Sany_ds.NEA_expr expr ->
		     Sany_ds.NEA_assume_prove (assume_proves_from_expr expr)
		  | x -> x (* rest is unchanged *)
		) assumes in
     (* divide into new_symb and assume-proves *)
     let (sany_ns, sany_assumes3) =
       List.fold_right (fun nea pair ->
			let (ns,aps) = pair in
			match nea with
			| Sany_ds.NEA_new_symb n -> (n::ns, aps)
			| Sany_ds.NEA_assume_prove ap -> (ns, ap::aps)
			| Sany_ds.NEA_expr _ -> failwith "Implementation error!"
		       ) sany_assumes2 ([],[]) in
     let assumes, acc3 =
       fold self#assume_prove (Nothing, acc2)
	    sany_assumes3 unfold_assume_prove in
     let new_symbols, acc4 =
       fold self#new_symb (Nothing, acc3) sany_ns unfold_new_symb
     in
     let Any_expr prove, acc = self#expr (Nothing, acc3) prove in
     let ap = { location; level; new_symbols; assumes;
		prove; suffices; boxed; } in
     (Any_assume_prove ap, acc)

   method new_symb acc0 { Sany_ds.location; level; op_decl; set } =
     let Any_location location, acc1 = self#location acc0 location in
     let Any_level level,       acc2 = self#level (Nothing, acc1) level in
     let Any_op_decl op_decl,   acc3 = self#op_decl (Nothing, acc2) op_decl in
     let set, acc = match set with
       | None -> (None, acc3)
       | Some e -> let Any_expr e, acc4 = self#expr (Nothing, acc3) e in
		   (Some e,acc4)
     in
     let ns = { location; level; op_decl; set; } in
     (Any_new_symb ns, acc)

   method let_in acc0 {Sany_ds.location; level; body; op_defs } =
     let Any_location location, acc1 = self#location acc0 location in
     let Any_level level,       acc2 = self#level (Nothing, acc1) level in
     let Any_expr body,         acc3 = self#expr (Nothing, acc2) body in
     let op_defs, acc = fold self#op_def_or_theorem_or_assume
			     (Nothing, acc3) op_defs unfold_od_t_a in
     let l = { location; level; body; op_defs } in
     (Any_let_in l, acc)

   method subst_in acc0 ({ Sany_ds.location; level;
			   substs; body } : Sany_ds.subst_in) =
     let Any_location location, acc1 = self#location acc0 location in
     let Any_level level,       acc2 = self#level (Nothing, acc1) level in
     let substs, acc3 = fold self#subst (Nothing, acc2) substs unfold_subst in
     let Any_expr body,         acc = self#expr (Nothing, acc3) body in
     let s = ({ location; level; substs; body } : subst_in) in
     (Any_subst_in s, acc)

   method label acc0 ({Sany_ds.location; level; name;
		       arity; body; params } : Sany_ds.label) =
     let Any_location location, acc1 = self#location acc0 location in
     let Any_level level,       acc2 = self#level (Nothing, acc1) level in
     let ap = match body with
       | EA_expr e -> assume_proves_from_expr e
       | EA_assume_prove ap -> ap in
     let Any_assume_prove body, acc3 = self#assume_prove (Nothing, acc2) ap in
     let params, acc =
       fold self#formal_param (Nothing, acc3) params unfold_formal_param  in
     let l = ({location; level; name; arity; body; params } : label) in
     (Any_label l, acc)

   method ap_subst_in acc0 ({ Sany_ds.location;
			      level; substs; body } : Sany_ds.ap_subst_in) =
     let Any_location location, acc1 = self#location acc0 location in
     let Any_level level,       acc2 = self#level (Nothing, acc1) level in
     let substs, acc3 = fold self#subst (Nothing, acc2) substs unfold_subst in
     let Any_node body, acc = self#node (Nothing, acc3) body in
     let ap = ({ location; level; substs; body } : ap_subst_in) in
     (Any_ap_subst_in ap, acc)

   method def_step acc0 { Sany_ds.location; level; defs } =
     let Any_location location, acc1 = self#location acc0 location in
     let Any_level level,       acc2 = self#level (Nothing, acc1) level in
     let defs, acc = fold self#op_def (Nothing, acc2) defs unfold_op_def in
     let ds = { location; level; defs } in
     (Any_def_step ds, acc)

   method module_instance acc0 = function
   | Sany_ds.MI_ref x -> (Any_module_instance (MI_ref x), snd acc0)
   | Sany_ds.MI { Sany_ds.location; level; name } ->
     let Any_location location, acc1 = self#location acc0 location in
     let Any_level level,       acc2 = self#level (Nothing, acc1) level in
     let mi = { location; level; name } in
     (Any_module_instance (MI mi), acc2)

   (* TODO: proper replacement *)
   method builtin_op (_, acc0) = function
     | Sany_ds.BOP_ref x ->
	let op = List.assoc x acc0 in
	(Any_builtin_op op, acc0)
   | Sany_ds.BOP {Sany_ds.location; level; name; arity; params } ->
      failwith "Implementation error: builtins shouldn't be converted anymore!"

   method user_defined_op acc0 = function
     | Sany_ds.UOP_ref x ->
	(Any_user_defined_op (UOP_ref x), snd acc0)
     | Sany_ds.UOP { Sany_ds.location; level ; name ; arity ;
		     body ; params ; recursive ; } ->
	let (Any_location location, acc1) = self#location acc0 location in
	let (Any_level level, acc2) = self#level (Nothing, acc1) level in
	let (Any_expr body, acc3) = self#expr (Nothing, acc2) body in
	let handle_arg x (fp,_) = self#formal_param x fp in
	let (args, acc) = fold handle_arg (Nothing, acc3)
			       params unfold_formal_param in
	let leibniz = List.map snd params in
	let params = List.combine args leibniz in
	let op = UOP {
		     location; level; name; arity; body; params; recursive;
		   } in
	(Any_user_defined_op op, acc)

   method name (_,acc) x = (Any_name x, acc)

   method reference (_, acc) x =
     failwith "Implementation error: references should be handled implicitly!"

   method context acc { Sany_ds.entries; modules } =
     let es,  acc0 = fold self#entry acc entries unfold_entry in
     let empty_entry = { fp = []; m = []; opd = [];
			 opdec = []; t = []; a = []; aps = [] } in
     let e = List.fold_right
	       (fun x y ->
		{
		  fp = List.append x.fp y.fp;
		  m = List.append x.m y.m;
		  opd = List.append x.opd y.opd;
		  opdec = List.append x.opdec y.opdec;
		  t = List.append x.t y.t;
		  a = List.append x.a y.a;
		  aps = List.append x.aps y.aps;
		}
	       ) es empty_entry in
     let modules, acc1 = fold self#mule (Nothing, acc0) modules unfold_module in
     let c = {
	 fp_entries = e.fp;
	 mod_entries = e.m;
	 opdef_entries = e.opd;
	 opdec_entries = e.opdec;
	 theorem_entries = e.t;
	 assume_entries = e.a;
	 apsubst_entries = e.aps;
	 modules;
       } in
     (Any_context c, acc1)

   method entry acc0 { Sany_ds.uid; reference } =
     (* since the FMOTA datatype is not present, we have a local record dentry
        instead, which collects the seperated lists. To be more concise, we
        put empty default entries for each field into the scope. In each branch,
        we only overwrite the definition of the matched element.
      *)
     let (fp, m, opd, opdec, t, a, aps) = ([], [], [], [], [], [], []) in
     match reference with
     (* builtin operators need to be taken from the builtin store *)
     | Sany_ds.FMOTA_op_def (Sany_ds.OPDef (Sany_ds.O_builtin_op _)) ->
	let _, acc = acc0 in
	let x = List.assoc uid acc in
	let opd = [(uid, O_builtin_op x)] in
	(Any_entry { fp; m; opd; opdec; t; a; aps }, acc)
     (* remaining cases *)
     | Sany_ds.FMOTA_formal_param x ->
	let Any_formal_param (FP x), acc = self#formal_param acc0 x in
	let fp = [(uid, x)] in
	(Any_entry { fp; m; opd; opdec; t; a; aps }, acc)
     | Sany_ds.FMOTA_module x ->
	let Any_mule (MOD x), acc = self#mule acc0 x in
	let m = [(uid, x)] in
	(Any_entry { fp; m; opd; opdec; t; a; aps }, acc)
     | Sany_ds.FMOTA_op_def x ->
	let Any_op_def (OPDef x), acc = self#op_def acc0 x in
	let opd = [(uid, x)] in
	(Any_entry { fp; m; opd; opdec; t; a; aps }, acc)
     | Sany_ds.FMOTA_op_decl x ->
	let Any_op_decl (OPD x), acc = self#op_decl acc0 x in
	let opdec = [(uid, x)] in
	(Any_entry { fp; m; opd; opdec; t; a; aps }, acc)
     | Sany_ds.FMOTA_theorem x ->
	let Any_theorem (THM x), acc = self#theorem acc0 x in
	let t = [(uid, x)] in
	(Any_entry { fp; m; opd; opdec; t; a; aps }, acc)
     | Sany_ds.FMOTA_assume x ->
	let Any_assume (ASSUME x), acc = self#assume acc0 x in
	let a = [(uid, x)] in
	(Any_entry { fp; m; opd; opdec; t; a; aps }, acc)
     | Sany_ds.FMOTA_ap_subst_in x ->
	let Any_ap_subst_in x, acc = self#ap_subst_in acc0 x in
	let aps = [(uid, x)] in
	(Any_entry { fp; m; opd; opdec; t; a; aps }, acc)


   (* pure disjunction types *)
   method expr acc = function
     | Sany_ds.E_at x        ->
	let Any_at y, acc0 = self#at acc x in
	(Any_expr (E_at y), acc0)
     | Sany_ds.E_decimal x   ->
	let Any_decimal y, acc0 = self#decimal acc x in
	(Any_expr (E_decimal y), acc0)
     | Sany_ds.E_label x     ->
	let Any_label y, acc0 = self#label acc x in
	(Any_expr (E_label y), acc0)
     | Sany_ds.E_let_in x    ->
	let Any_let_in y, acc0 = self#let_in acc x in
	(Any_expr (E_let_in y), acc0)
     | Sany_ds.E_numeral x   ->
	let Any_numeral y, acc0 = self#numeral acc x in
	(Any_expr (E_numeral y), acc0)
     | Sany_ds.E_op_appl x   ->
	let Any_op_appl y, acc0 = self#op_appl acc x in
	(Any_expr (E_op_appl y), acc0)
     | Sany_ds.E_string x    ->
	let Any_strng y, acc0 = self#strng acc x in
	(Any_expr (E_string y), acc0)
     | Sany_ds.E_subst_in x  ->
	let Any_subst_in y, acc0 = self#subst_in acc x in
	(Any_expr (E_subst_in y), acc0)

   method expr_or_module_or_module_instance acc = function
     | Sany_ds.EMM_expr x            ->
	let Any_expr y, acc0 = self#expr acc x in
	(Any_expr_or_module_or_module_instance (EMM_expr y), acc0)
     | Sany_ds.EMM_module_instance x ->
	let Any_mule y, acc0 = self#module_instance acc x in
	(Any_expr_or_module_or_module_instance (EMM_module y), acc0)
     | Sany_ds.EMM_module x          ->
	let Any_module_instance y, acc0 = self#mule acc x in
	(Any_expr_or_module_or_module_instance (EMM_module_instance y), acc0)

   method user_defined_op_or_module_instance_or_theorem_or_assume acc = function
     | Sany_ds.UMTA_user_defined_op x ->
	let Any_user_defined_op y, acc0 = self#user_defined_op acc x in
	(Any_defined_expr ( UMTA_user_defined_op y), acc0)
     | Sany_ds.UMTA_module_instance x ->
	let Any_module_instance y, acc0 = self#module_instance acc x in
	(Any_defined_expr ( UMTA_module_instance y), acc0)
     | Sany_ds.UMTA_theorem x         ->
	let Any_theorem y, acc0 = self#theorem acc x in
	(Any_defined_expr ( UMTA_theorem y), acc0)
     | Sany_ds.UMTA_assume x          ->
	let Any_assume y, acc0 = self#assume acc x in
	(Any_defined_expr ( UMTA_assume y), acc0)

   method new_symb_or_expr_or_assume_prove acc =
     failwith "Implementation eror! This code should be unreachable!"

   method op_def_or_theorem_or_assume acc = function
     | Sany_ds.OTA_op_def x ->
	let Any_op_def y, acc0 = self#op_def acc x in
	(Any_op_def_or_theorem_or_assume (OTA_op_def y), acc0)
     | Sany_ds.OTA_theorem x ->
	let Any_theorem y, acc0 = self#theorem acc x in
	(Any_op_def_or_theorem_or_assume (OTA_theorem y), acc0)
     | Sany_ds.OTA_assume x ->
	let Any_assume y, acc0 = self#assume acc x in
	(Any_op_def_or_theorem_or_assume (OTA_assume y), acc0)

   method expr_or_assume_prove acc =
          failwith "Implementation eror! This code should be unreachable!"

   method expr_or_op_arg acc = function
     | Sany_ds.EO_op_arg oa ->
	let Any_op_arg y, acc0 = self#op_arg acc oa in
	(Any_expr_or_oparg (EO_op_arg y), acc0)
     | Sany_ds.EO_expr e ->
	let Any_expr y, acc0 = self#expr acc e in
	(Any_expr_or_oparg (EO_expr y), acc0)

   method fmota acc = function
     | Sany_ds.FMOTA_formal_param x ->
	let Any_formal_param y, acc0 = self#formal_param acc x in
	(Any_operator (FMOTA_formal_param y), acc0)
     | Sany_ds.FMOTA_module  x ->
	let Any_mule y, acc0 = self#mule acc x in
	(Any_operator (FMOTA_module y), acc0)
     | Sany_ds.FMOTA_op_decl x ->
	let Any_op_decl y, acc0 = self#op_decl acc x in
	(Any_operator (FMOTA_op_decl y), acc0)
     | Sany_ds.FMOTA_op_def  x ->
	let Any_op_def y, acc0 = self#op_def acc x in
	(Any_operator (FMOTA_op_def y), acc0)
     | Sany_ds.FMOTA_theorem x ->
	let Any_theorem y, acc0 = self#theorem acc x in
	(Any_operator (FMOTA_theorem y), acc0)
     | Sany_ds.FMOTA_assume  x ->
	let Any_assume y, acc0 = self#assume acc x in
	(Any_operator (FMOTA_assume y), acc0)
     | Sany_ds.FMOTA_ap_subst_in x ->
	let Any_ap_subst_in y, acc0 = self#ap_subst_in acc x in
	(Any_operator (FMOTA_ap_subst_in y), acc0)
end

(* The implementation of the public interface *)

let converter_instance = new converter

let convert_expr ?builtins:(b=[]) x =
  match converter_instance#expr ( Nothing, b ) x with
  | Any_expr e, _ -> e
  | _ -> failwith "Implementation error in sany -> internal term conversion."
let convert_context ?builtins:(b=[]) x =
  match converter_instance#context ( Nothing, b ) x with
  | Any_context e, _ -> e
  | _ -> failwith "Implementation error in sany -> internal term conversion."
let convert_module ?builtins:(b=[]) x =
  match converter_instance#mule (Nothing, b) x with
  | Any_mule e, _ -> e
  | _ -> failwith "Implementation error in sany -> internal term conversion."
let convert_formal_param ?builtins:(b=[]) x =
  match converter_instance#formal_param (Nothing, b) x with
  | Any_formal_param e, _ -> e
  | _ -> failwith "Implementation error in sany -> internal term conversion."