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

  (* failsafe: those operators should be removed during preprocessing *)
			      
  method at acc x = failwith "Remove at first."
  method label acc x = failwith "Remove first."
  method let_in acc x = failwith "Remove first."

  (* non recursive expressions *)				 

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

  (* recursive expressions *)			   
				      
  method op_appl acc x =
    let soperator:simple_operator =
      unany#operator
	(get_any (self#operator acc x.operator))
    in
    let soperands:(simple_expr_or_op_arg list) =
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

  method binder acc x = acc (* TODO *)

  method lambda acc x = acc (* TODO *)

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
    in
    let sop_decl = unany#op_decl (get_any  (self#op_decl acc x.op_decl)) in
    let sx = {
	location          = x.location;
	level             = x.level;
	op_decl           = sop_decl; 
	set               = sset
    }
    in set_any acc (Any_new_symb sx)

			    
  method op_def acc x = acc
  method user_defined_op acc x = acc

  method builtin_op acc x = acc
  method formal_param acc x = acc
  method op_decl acc x = acc

  method expr_or_module_or_module_instance acc x = acc
  method expr acc x = acc                              
                         
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
