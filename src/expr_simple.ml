open Expr_visitor
open Simple_expr_ds
open Any_simple_expr

type 'a esacc = ESAcc of Expr_ds.term_db * term_db * 'a
       
class ['a] expr_to_simple_expr = object(self)
  inherit ['a] visitor as super

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
  method strng acc x = acc
  method binder acc x = acc
  method lambda acc x = acc

  method expr_or_op_arg acc x = acc
  method assume_prove acc x = acc
  method new_symb acc x = acc
  method op_def acc x = acc
  method user_defined_op acc x = acc

  method builtin_op acc x = acc
  method op_arg acc x = acc
  method formal_param acc x = acc
  method op_decl acc x = acc

  method expr_or_module_or_module_instance acc x = acc
                                
                         
end


