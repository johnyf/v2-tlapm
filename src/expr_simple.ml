open Expr_visitor
open Simple_expr_ds
open Any_simple_expr

type 'a esacc = ESAcc of Expr_ds.term_db * simple_term_db * anySimpleExpr * 'a
									      
let get_term_db (term_db,_,_,_) = term_db
let get_simple_term_db (_,simple_term_db,_,_) = simple_term_db
let get_any (_,_,any,_) = any
let get_a (_,_,_,a) = a

(*	
let set_term_db ESAcc(_,simple_term_db,any,a) term_db = ESAcc(term_db,simple_term_db,any,a)
let set_simple_term_db ESAcc(term_db,_,any,a) simple_term_db = ESAcc(term_db,simple_term_db,any,a)
let set_any ESAcc(term_db,simple_term_db,_,a) any = ESAcc(term_db,simple_term_db,any,a)
let set_a ESAcc(term_db,simple_term_db,any,_) a = ESAcc(term_db,simple_term_db,any,a)
*)
			
class ['a] expr_to_simple_expr = object(self)
  inherit ['a esacc] visitor as super

  (* failsafe against using this on the module / proof level *)
  method context _ _ = failwith "Can not convert full contexts!"
  method mule _ _ = failwith "Can not convert modules!"
  method entry _ _ = failwith "Can not convert entries!"

  method at acc x = failwith "Remove at first."
  method decimal acc x = acc
  method label acc x = failwith "Remove first."
  method let_in acc x = failwith "Remove first."
  method numeral acc x = acc
  method op_appl acc x = acc
  method strng acc x = acc (*E_string ({value = x.value}:simple_strng)*)
  method binder acc x = acc
  method lambda acc x = acc

  method expr_or_op_arg acc x = acc
  method assume_prove acc x = acc
(*
    match acc with
    | ESAcc (a,b,_,d) -> ESAcc (a,b, (Any_assume_prove ({location = "test"; level = "test"; new_symbols = "test" ; assumes = "test" ; prove = "test"}:simple_assume_prove)), d)
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
  let acc = ESAcc (termdb, [], Nothing, () ) in
  let ESAcc (_, stermdb, pre_ap, _) =
    converter#assume_prove acc assume_prove
  in
  match pre_ap with
  | Any_assume_prove ap -> (stermdb, ap)
  | _ -> failwith "Expr_to_simple_expr failed returning an assume_prove."
