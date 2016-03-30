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

			
class expr_to_simple_expr = object(self)
  inherit [esacc] visitor as super

  (* failsafe against using this on the module / proof level *)
  method context _ _ = failwith "Can not convert full contexts!"
  method mule _ _ = failwith "Can not convert modules!"
  method entry _ _ = failwith "Can not convert entries!"
  method at acc x = failwith "Remove at first."
  method label acc x = failwith "Remove first."
  method let_in acc x = failwith "Remove first."

  method decimal acc x = acc
(*			  
    let sx = {
	location          = x.location;
	level             = x.level;
	mantissa          = x.mantissa;
	exponent          = x.exponent
    }
    in set_any acc (Any_decimal sx)
 *)
  method numeral acc x = acc
(*
    let sx:simple_numeral = {
	location          = x.location;
	level             = x.level;
	value             = x.value;
    }
    in set_any acc (Any_numeral sx)
 *)
  method op_appl acc x = failwith "TODO op_appl."
(*    let sx:simple_op_appl = {
	location          = x.location;
	level             = x.level;
        operator          = x.operator; TODO appel récursif
	operands          = x.operands; TODO appel récursif
    }
    in set_any acc (Any_op_appl sx)
*)		   
  method strng acc x = acc
(*
    let sx = {
	location          = x.location;
	level             = x.level;
	value             = x.value;
    }
    in set_any acc (Any_strng sx)
*)
  method binder acc x = acc
  method lambda acc x = acc

  method expr_or_op_arg acc x = acc
  method assume_prove acc x = acc
(*				
    let str = {
	location          = x.location;
	level             = x.level;
	value             = "TEST";
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
*)
  method new_symb acc x = acc
  method op_def acc x = acc
  method user_defined_op acc x = acc

  method builtin_op acc x = acc
  method op_arg acc x = acc
  method formal_param acc x = acc
  method op_decl acc x = acc

  method expr_or_module_or_module_instance acc x = acc
                                
                         
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
