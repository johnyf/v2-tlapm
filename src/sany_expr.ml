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
  | Any_def_tep of  def_step
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

type builtin_store = (Sany_ds.builtin_op * builtin_op) list

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


(**
 This visitor converts sany_ds datastructures into expr_ds datastructures. Since
 builtin operators or unfolded, we pass an association list builtin_store in the
 accumulator. As a return value, we are actually interested in an expression,
 so we need to use the anyExpr data type to have a uniform type.
 *)
class converter = object(self)
  inherit [anyExpr * builtin_store] Sany_visitor.visitor as super

  method node acc = function
  | Sany_ds.N_ap_subst_in x  -> self#ap_subst_in acc x
  | Sany_ds.N_assume_prove x -> self#assume_prove acc x
  | Sany_ds.N_def_step x     -> self#def_step acc x
  | Sany_ds.N_expr x         -> self#expr acc x
  | Sany_ds.N_op_arg x       -> self#op_arg acc x
  | Sany_ds.N_instance x     -> self#instance acc x
  | Sany_ds.N_new_symb x     -> self#new_symb acc x
  | Sany_ds.N_proof x        -> self#proof acc x
  | Sany_ds.N_formal_param x -> self#formal_param acc x
  | Sany_ds.N_module x       -> self#mule acc x
  | Sany_ds.N_op_decl x      -> self#op_decl acc x
  | Sany_ds.N_op_def x       -> self#op_def acc x
  | Sany_ds.N_assume x       -> self#assume acc x
  | Sany_ds.N_theorem x      -> self#theorem acc x
  | Sany_ds.N_use_or_hide x  -> self#use_or_hide acc x

  (* parts of expressions *)
  method location (_, acc) = function
    | None -> (Any_location mkDummyLocation, acc)
    | Some l -> (Any_location l, acc)

  method level (_, acc) l = (Any_level l, acc)

  (* non-recursive expressions *)
  method decimal acc0 {Sany_ds.location; level; mantissa; exponent} =
    let Any_location location, acc = self#location acc0 location in
    let d = {
	location = location;
	level = level;
	mantissa = mantissa;
	exponent = exponent;
      } in
    (Any_decimal d, acc)

  method numeral acc0 ({Sany_ds.location; level; value} : Sany_ds.numeral) =
    let Any_location location, acc = self#location acc0 location in
    let n:numeral = {
	location = location;
	level = level;
	value = value;
      } in
    (Any_numeral n, acc)

  method strng acc0 ({Sany_ds.location; level; value} : Sany_ds.strng) =
    let Any_location location, acc = self#location acc0 location in
    let s:strng = {
	location = location;
	level = level;
	value = value;
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
	 location = location;
	 level = level;
	 except = except;
	 except_component = except_component;
       } in
     (Any_at at, acc)

   method op_appl acc0 ({Sany_ds.location; level; operator;
			      operands; bound_symbols} : Sany_ds.op_appl) =
     let Any_location location, acc1 = self#location acc0 location in
     let Any_level level,       acc2 = self#level (Nothing, acc1) level in
     let Any_operator operator, acc3 = self#fmota (Nothing, acc2) operator in
     let unwrap_expr (Any_expr_or_oparg e) = e in
     let unwrap_bs (Any_bound_symbol s) = s in
     let operands, acc4 =
       fold self#expr_or_op_arg (Nothing, acc3) operands unwrap_expr in
     let bound_symbols, acc =
       fold self#bound_symbol (Nothing, acc4) bound_symbols unwrap_bs in
     let op_appl = {
	 location = location;
	 level = level;
	 operator = operator;
	 operands = operands;
	 bound_symbols = bound_symbols;
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
     let unfold_formal_param (Any_formal_param x) = x in
     let params, acc1 =
       fold self#formal_param acc0 params unfold_formal_param in
     let Any_expr domain, acc = self#expr (Nothing, acc1) domain in
     let b = {
	 params = params;
	 tuple = tuple;
	 domain = domain;
       } in
     (Any_bounded_bound_symbol b, acc)

   method unbounded_bound_symbol acc0 { Sany_ds.param; tuple }  =
     let Any_formal_param param, acc = self#formal_param acc0 param  in
     let b = {
	 param = param;
	 tuple = tuple;
       } in
     (Any_unbounded_bound_symbol b, acc)


   method formal_param acc0 = function
     | Sany_ds.FP_ref i ->
	(Any_formal_param (FP_ref i), snd acc0)
     | Sany_ds.FP { Sany_ds.location; level; name; arity; } ->
	let Any_location location, acc1 = self#location acc0 location in
	let Any_level level,       acc2 = self#level (Nothing, acc1) level in
	let fp = {
	    location = location;
	    level = level;
	    name = name;
	    arity = arity;
	  }
	in (Any_formal_param (FP fp), acc2)

   method mule acc0 = function
     | Sany_ds.MOD_ref i ->
	(Any_mule (MOD_ref i), snd acc0)
     | Sany_ds.MOD { Sany_ds.name; location; constants; variables;
		     definitions; assumptions; theorems; } ->
	let Any_location location, acc1 = self#location acc0 location in
	let unfold_op_decl (Any_op_decl x) = x in
	let unfold_op_def (Any_op_def x) = x in
	let unfold_assumption (Any_assume x) = x in
	let unfold_theorem (Any_theorem x) = x in
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
	    name = name;
	    location = location;
	    constants = constants;
	    variables = variables;
	    definitions = definitions;
	    assumptions = assumptions;
	    theorems = theorems;
	  } in
	(Any_mule m, acc)

   method op_arg acc0 { Sany_ds.location; level; name; arity } =
     let Any_location location, acc1 = self#location acc0 location in
     let Any_level level,       acc2 = self#level (Nothing, acc1) level in
     let oparg = ({
	 location = location;
	 level = level;
	 name = name;
	 arity = arity;
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
	 location = location;
	 level = level;
	 name = name;
	 arity = arity;
	 kind = kind;
       } in
     (Any_op_decl opdec, acc2)

   method op_def acc = function
   | Sany_ds.OPDef_ref x -> self#reference acc x
   | Sany_ds.OPDef (Sany_ds.O_module_instance x) -> self#module_instance acc x
   | Sany_ds.OPDef (Sany_ds.O_builtin_op x)      -> self#builtin_op acc x
   | Sany_ds.OPDef (Sany_ds.O_user_defined_op x) -> self#user_defined_op acc x

   method theorem acc0 = function
   | Sany_ds.THM_ref x -> self#reference acc0 x
   | Sany_ds.THM { Sany_ds.location; level; expr; proof; suffices } ->
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = self#expr_or_assume_prove acc2 expr in
     let acc4 = self#proof acc3 proof  in
     (* todo: finish*)
     acc4

   method assume acc0  = function
   | Sany_ds.ASSUME_ref x -> self#reference acc0 x
   | Sany_ds.ASSUME {Sany_ds.location; level; expr; } ->
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc = self#expr acc2 expr in
     acc

   method proof acc0 = function
   | Sany_ds.P_omitted location -> acc0
   | Sany_ds.P_obvious location -> acc0
   | Sany_ds.P_by { Sany_ds.location; level; facts; defs; only} ->
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = List.fold_left
       self#expr_or_module_or_module_instance acc2 facts in
     let acc = List.fold_left
       self#user_defined_op_or_module_instance_or_theorem_or_assume acc3 defs in
     (* skip the only tag *)
     acc
   | Sany_ds.P_steps { Sany_ds.location; level; steps; } ->
     List.fold_left self#step acc0 steps
   | Sany_ds.P_noproof -> acc0

   method step acc0 = function
   | Sany_ds.S_def_step x -> self#def_step acc0 x
   | Sany_ds.S_use_or_hide x -> self#use_or_hide acc0 x
   | Sany_ds.S_instance i -> self#instance acc0 i
   | Sany_ds.S_theorem t -> self#theorem acc0 t

   method use_or_hide acc0 {  Sany_ds.location; level; facts; defs; only; hide } =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = List.fold_left
       self#expr_or_module_or_module_instance acc2 facts in
     let acc = List.fold_left
       self#user_defined_op_or_module_instance_or_theorem_or_assume acc3 defs in
     acc

   method instance acc0 {Sany_ds.location; level; name; substs; params; } =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = self#name acc2 name in
     let acc4 = List.fold_left self#subst acc3 substs in
     let acc = List.fold_left self#formal_param acc4 params in
     acc

   method subst acc0 { Sany_ds.op; expr } =
     let acc1 = self#op_decl acc0 op in
     let acc = self#expr_or_op_arg acc1 expr in
     acc

   method assume_prove acc0 { Sany_ds.location; level; assumes;
			      prove; suffices; boxed; } =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = List.fold_left
       self#new_symb_or_expr_or_assume_prove acc2 assumes in
     let acc = self#expr acc3 prove in
     (* suffices and boxed are boolean flags*)
     acc

   method new_symb acc0 { Sany_ds.location; level; op_decl; set } =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = self#op_decl acc2 op_decl in
     let acc = match set with
       | None -> acc3
       | Some e -> self#expr acc3 e
     in acc

   method let_in acc0 {Sany_ds.location; level; body; op_defs } =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = self#expr acc2 body in
     let acc = List.fold_left self#op_def_or_theorem_or_assume acc3 op_defs in
     acc

   method subst_in acc0 ({ Sany_ds.location; level; substs; body } : Sany_ds.subst_in) =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = List.fold_left self#subst acc2 substs in
     let acc = self#expr acc3 body in
     acc

   method label acc0 ({Sany_ds.location; level; name; arity; body; params } : Sany_ds.label) =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = self#name acc2 name in
     (* skip arity *)
     let acc4 = self#expr_or_assume_prove acc3 body in
     let acc = List.fold_left self#formal_param acc4 params in
     acc

   method ap_subst_in acc0 ({ Sany_ds.location; level; substs; body } : Sany_ds.ap_subst_in) =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = List.fold_left self#subst acc2 substs in
     let acc = self#node acc3 body in
     acc

   method def_step acc0 { Sany_ds.location; level; defs } =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc = List.fold_left self#op_def acc2 defs in
     acc

   method module_instance acc0 = function
   | Sany_ds.MI_ref x -> self#reference acc0 x
   | Sany_ds.MI {Sany_ds.location; level; name} ->
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc = self#name acc2 name in
     acc

   method builtin_op acc0 = function
   | Sany_ds.BOP_ref x -> self#reference acc0 x
   | Sany_ds.BOP {Sany_ds.location; level; name; arity; params } ->
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = self#name acc2 name in
   (* skip arity *)
     let acc = List.fold_left
       (fun x (fp,_) -> self#formal_param x fp) acc3 params
     in acc

   method user_defined_op acc0 = function
   | Sany_ds.UOP_ref x -> self#reference acc0 x
   | Sany_ds.UOP { Sany_ds.location; level ; name ; arity ;
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

   method context acc { Sany_ds.entries; modules } =
     let acc0 = List.fold_left self#entry acc entries in
     let acc1 = List.fold_left self#mule acc0 modules in
     acc1

   method entry acc { Sany_ds.uid; reference } =
     (* skipping uid *)
     self#fmota acc reference

   (* pure disjunction types *)
   method expr acc = function
   | Sany_ds.E_at x        -> self#at acc x
   | Sany_ds.E_decimal x   -> self#decimal acc x
   | Sany_ds.E_label x     -> self#label acc x
   | Sany_ds.E_let_in x    -> self#let_in acc x
   | Sany_ds.E_numeral x   -> self#numeral acc x
   | Sany_ds.E_op_appl x   -> self#op_appl acc x
   | Sany_ds.E_string x    -> self#strng acc x
   | Sany_ds.E_subst_in x  -> self#subst_in acc x

   method expr_or_module_or_module_instance acc = function
   | Sany_ds.EMM_expr x            -> self#expr acc x
   | Sany_ds.EMM_module_instance x -> self#module_instance acc x
   | Sany_ds.EMM_module x          -> self#mule acc x

   method user_defined_op_or_module_instance_or_theorem_or_assume acc = function
   | Sany_ds.UMTA_user_defined_op x -> self#user_defined_op acc x
   | Sany_ds.UMTA_module_instance x -> self#module_instance acc x
   | Sany_ds.UMTA_theorem x         -> self#theorem acc x
   | Sany_ds.UMTA_assume x          -> self#assume acc x

   method new_symb_or_expr_or_assume_prove acc = function
   | Sany_ds.NEA_new_symb s      -> self#new_symb acc s
   | Sany_ds.NEA_expr e          -> self#expr acc e
   | Sany_ds.NEA_assume_prove ap -> self#assume_prove acc ap

   method op_def_or_theorem_or_assume acc = function
   | Sany_ds.OTA_op_def x -> self#op_def acc x
   | Sany_ds.OTA_theorem x -> self#theorem acc x
   | Sany_ds.OTA_assume x -> self#assume acc x

   method expr_or_assume_prove acc = function
   | Sany_ds.EA_assume_prove ap -> self#assume_prove acc ap
   | Sany_ds.EA_expr e          -> self#expr acc e

   method expr_or_op_arg acc = function
   | Sany_ds.EO_op_arg oa -> self#op_arg acc oa
   | Sany_ds.EO_expr e -> self#expr acc e

   method fmota acc = function
   | Sany_ds.FMOTA_formal_param x -> self#formal_param acc x
   | Sany_ds.FMOTA_module  x -> self#mule acc x
   | Sany_ds.FMOTA_op_decl x -> self#op_decl acc x
   | Sany_ds.FMOTA_op_def  x -> self#op_def acc x
   | Sany_ds.FMOTA_theorem x -> self#theorem acc x
   | Sany_ds.FMOTA_assume  x -> self#assume acc x
   | Sany_ds.FMOTA_ap_subst_in x -> self#ap_subst_in acc x



end

let converter_instance = new converter

let convert_expr x = match converter_instance#expr ( Nothing, [] ) x with
  | Any_expr e, _ -> e
  | _ -> failwith "Implementation error in sany -> internal term conversion."
let convert_context x = match converter_instance#context ( Nothing, [] ) x with
  | Any_context e, _ -> e
  | _ -> failwith "Implementation error in sany -> internal term conversion."
let convert_module x = match converter_instance#mule (Nothing, []) x with
  | Any_mule e, _ -> e
  | _ -> failwith "Implementation error in sany -> internal term conversion."
