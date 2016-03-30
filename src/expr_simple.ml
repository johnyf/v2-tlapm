open Expr_visitor
open Simple_expr_ds
open Any_simple_expr

type esacc = Expr_ds.term_db * simple_term_db * anySimpleExpr
									      
let get_term_db (term_db,_,_) = term_db
let get_simple_term_db (_,simple_term_db,_) = simple_term_db
let get_any (_,_,any) = any

let set_term_db (_,simple_term_db,any) term_db = (term_db,simple_term_db,any)
let set_simple_term_db (term_db,_,any) simple_term_db = (term_db,simple_term_db,any)
let set_any (term_db,simple_term_db,_) any = (term_db,simple_term_db,any)
		       
class extractor = object
  inherit [anySimpleExpr] any_extractor
  method extract x = x
end
		       
class expr_to_simple_expr = object(self)

  val unany = new extractor
				    
  inherit [esacc] visitor as super

  (* failsafe against using this on the module / proof level *)

  method context _ _ = failwith "Can not convert full contexts!"
  method mule _ _ = failwith "Can not convert modules!"
  method entry _ _ = failwith "Can not convert entries!"

  (** failsafe: those operators should be removed during preprocessing **)
			      
  method at acc x = failwith "Remove at first."
  method label acc x = failwith "Remove first."
  method let_in acc x = failwith "Remove first."

  (** non recursive expressions **)				 

  method decimal acc x =
    let sx = {
	location          = x.location;
	level             = x.level;
	mantissa          = x.mantissa;
	exponent          = x.exponent
    }
    in set_any acc (Any_decimal sx)

  method numeral acc x = 
    let sx:simple_numeral = {
	location          = x.location;
	level             = x.level;
	value             = x.value;
    }
    in set_any acc (Any_numeral sx)

  method strng acc x =
    let sx = {
	location          = x.location;
	level             = x.level;
	value             = x.value;
    }
    in set_any acc (Any_strng sx)

  method op_arg acc x = self#operator acc x.argument

  (** recursive expressions **)			   
				      
  method op_appl acc x =
    let soperator:simple_operator =
      unany#operator
	(get_any (self#operator acc x.operator))
    and soperands:(simple_expr_or_op_arg list) =
      List.map
	(fun eooa -> unany#expr_or_op_arg
	   (get_any (self#expr_or_op_arg acc eooa))
	)
	(x.operands)
    in
   let sx:simple_op_appl = {
	location          = x.location;
	level             = x.level;
        operator          = soperator;
	operands          = soperands
    }
    in set_any acc (Any_op_appl sx)

  method expr_or_op_arg acc x =
    match x with
    | EO_expr expr ->
       let sexpr = EO_expr (unany#expr (get_any (self#expr acc expr)))
       in
       set_any acc (Any_expr_or_op_arg sexpr)
    | EO_op_arg op_arg ->
       let sop_arg = EO_op_arg (unany#op_arg (get_any (self#op_arg acc op_arg)))
       in
       set_any acc (Any_expr_or_op_arg sop_arg)

  method binder acc x =
    let soperator:simple_operator =
      unany#operator (get_any (self#operator acc x.operator))
    and soperand:simple_expr_or_op_arg =
      unany#expr_or_op_arg (get_any (self#expr_or_op_arg acc x.operand))
    and sbs = []
 (** TODO : Bound_symbols **)
      (*
      List.map
		(fun x -> (unany#bound_symbol (get_any (self#bound_symbol acc x)))
		x.bound_symbols
       *)
    in
    let sx:simple_binder = {
	location          = x.location;
	level             = x.level;
	operator          = soperator; 
	operand           = soperand; 
	bound_symbols     = sbs
    }
    in set_any acc (Any_binder sx)
			  
  method lambda acc x =
    let sparams = List.map
		    (fun (fp,b) -> (unany#formal_param (get_any ( self#formal_param acc fp)),b))
		    x.params
    and sbody = unany#expr (get_any (self#expr acc x.body))
    in
    let sx:simple_lambda = {
	location          = x.location;
	level             = x.level;
	arity             = x.arity; 
	body              = sbody; 
	params            = sparams
    }
    in set_any acc (Any_lambda sx)


  method assume_prove acc x =
    let str = {
	location          = x.location;
	level             = x.level;
	value             = "TEST - assume_prove";
    }
    in
    let sx = {
	location          = x.location;
	level             = x.level;
	new_symbols       = []; (*TODO*)
	assumes           = []; (*TODO*)
	prove             = E_string (str)
    }
    in set_any acc (Any_assume_prove sx)

  method new_symb acc x =
    let sset = match x.set with	
      | None -> None
      | Some e ->
	 let se = unany#expr (get_any (self#expr acc e))
	 in Some se
    and sop_decl = unany#op_decl (get_any  (self#op_decl acc x.op_decl)) in
    let sx = {
	location          = x.location;
	level             = x.level;
	op_decl           = sop_decl; 
	set               = sset
    }
    in set_any acc (Any_new_symb sx)

			    
  method op_def acc x = match x with
   | O_module_instance _ -> failwith "Can not convert modules!"
   | O_user_defined_op udo -> 
      let sudo = unany#user_defined_op (get_any (self#user_defined_op acc udo))
      in
      let sx = O_user_defined_op sudo
      in set_any acc (Any_op_def sx) 
   | O_builtin_op bo ->
      let sbo = unany#builtin_op (get_any (self#builtin_op acc bo))
      in
      let sx = O_builtin_op sbo
      in set_any acc (Any_op_def sx)  

  method builtin_op acc x =
    let sparams = List.map
		    (fun (fp,b) -> (unany#formal_param (get_any ( self#formal_param acc fp)),b))
		    x.params
    in
    let sx:simple_builtin_op = {
	level             = x.level;
	name              = x.name;
	arity             = x.arity; 
	params            = sparams
    }
    in set_any acc (Any_builtin_op sx)

  method expr_or_module_or_module_instance acc x = match x with
   | EMM_expr e -> 
      let se = unany#expr (get_any (self#expr acc e))
      in
      let sx = EMM_expr se
      in set_any acc (Any_expr_or_module_or_module_instance sx)
   | _ -> failwith "Can not convert modules!"

		   
  (** recursive expressions with reference **)
		   
  method user_defined_op acc x = acc (*TODO*)
  method formal_param acc x = acc (*TODO*)
  method op_decl acc x = acc (*TODO*)

			   
  (** global expr method **)
			   
  method expr acc x = acc                              (*TODO*)
                         
end


let parse_expr termdb assume_prove =
  let converter = new expr_to_simple_expr in
  let acc = (termdb, [], Nothing ) in
  let (_, stermdb, pre_ap) =
    converter#assume_prove acc assume_prove
  in
  match pre_ap with
  | Any_assume_prove ap -> (stermdb, ap)
  | _ -> failwith "Expr_to_simple_expr failed returning an assume_prove."
