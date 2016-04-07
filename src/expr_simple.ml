open Expr_visitor
open Expr_ds
open Simple_expr_ds
open Any_simple_expr
open Expr_dereference

       

(** ----------------------------------------------**)
(** Accumulator and tools **)
       
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


		    
(** ----------------------------------------------**)
(** Class definition **)
		    
class expr_to_simple_expr = object(self)

  val unany = new extractor
  method get_unany = unany

  inherit [esacc] visitor as super

			       
  (** ----------------------------------------------**)
  (** failsafe against using this on the module / proof level **)

  method context _ _ = failwith "Can not convert full contexts!"
  method mule _ _ = failwith "Can not convert modules!"

			     
			      
  (** ----------------------------------------------**)
  (** failsafe: those operators should be removed during preprocessing **)
			      
  method at acc x = failwith "Remove at first."
  method label acc x = failwith "Remove first."
  method let_in acc x = failwith "Remove first."




				 
  (** ----------------------------------------------**)			       
  (** non recursive expressions **)				 

  (** Decimal **)
  method decimal acc x =
    let sx = {
	location          = x.location;
	level             = x.level;
	mantissa          = x.mantissa;
	exponent          = x.exponent
    }
    in set_any acc (Any_decimal sx)

  (** Numeral **)	       
  method numeral acc x = 
    let sx:simple_numeral = {
	location          = x.location;
	level             = x.level;
	value             = x.value;
    }
    in set_any acc (Any_numeral sx)

  (** Strng **)
  method strng acc x =
    let sx = {
	location          = x.location;
	level             = x.level;
	value             = x.value;
    }
    in set_any acc (Any_strng sx)

  method op_arg acc x = self#operator acc x.argument



				      
  (** ----------------------------------------------**)			       
  (** recursive expressions **)			   

				      
  (** Op_appl **)
  method op_appl acc x =
    let acc1 = self#operator acc (x.operator)
    in
    let soperator:simple_operator =
      unany#operator (get_any acc1)
    in
    let rec f acct (l:(Expr_ds.expr_or_op_arg list)) = match l with
      | [] -> (acct,[])
      | t::q -> let (acctemp,q') = f acct q in
		let acct' = self#expr_or_op_arg acctemp t in
		let t'=unany#expr_or_op_arg (get_any acct') in
		(acct',(t'::q'))
    in
    let (acc2,soperands) = f acc1 x.operands
    in
    let sx:simple_op_appl = {
	location          = x.location;
	level             = x.level;
        operator          = soperator;
	operands          = soperands
      }
    in
    set_any acc2 (Any_op_appl sx)


	    
  (** Expr_or_op_arg **)
  method expr_or_op_arg acc x =
    match x with
    | EO_expr expr ->
       let acc1 = self#expr acc expr
       in
       let sexpr = EO_expr (unany#expr (get_any acc1))
       in
       set_any acc1 (Any_expr_or_op_arg sexpr)
    | EO_op_arg op_arg ->
       let acc1 = self#op_arg acc op_arg
       in
       let sop_arg = EO_op_arg (unany#op_arg (get_any acc1))
       in
       set_any acc1 (Any_expr_or_op_arg sop_arg)
	       
	       
  (** Binder **)   
  method binder acc x =
    let acc1 = self#operator acc x.operator
    in
    let soperator:simple_operator =
      unany#operator (get_any acc1)
    in
    let acc2 = self#expr_or_op_arg acc1 x.operand
    in
    let soperand:simple_expr_or_op_arg =
      unany#expr_or_op_arg (get_any acc2)
    in
    let rec f acct (l:(Expr_ds.bound_symbol list)) = match l with
      | [] -> (acct,[])
      | t::q -> let (acctemp,q') = f acct q in
		let acct' = self#bound_symbol acctemp t in
		let t'=unany#bound_symbol (get_any acct') in
		(acct',(t'::q'))
    in
    let (acc3,sbs) = f acc2 x.bound_symbols
    in
    let sx:simple_binder = {
	location          = x.location;
	level             = x.level;
	operator          = soperator; 
	operand           = soperand; 
	bound_symbols     = sbs
    }
    in set_any acc3 (Any_binder sx)


	       
  (** Bound_symbol **)
  method bound_symbol acc x =
  match x with
  | B_unbounded_bound_symbol ubs ->
     let acc1 = self#unbounded_bound_symbol acc ubs
     in
     let subs = B_unbounded_bound_symbol (unany#unbounded_bound_symbol (get_any acc1))
     in
     set_any acc1 (Any_bound_symbol subs)
  | B_bounded_bound_symbol bbs ->
     let acc1 = self#bounded_bound_symbol acc bbs
     in
     let sbbs = B_bounded_bound_symbol (unany#bounded_bound_symbol (get_any acc1))
     in
     set_any acc1 (Any_bound_symbol sbbs)


	       
  (** Unbounded_bound_symbol **)	       
  method unbounded_bound_symbol acc x =
    let acc1 = self#formal_param acc x.param
    in
    let sparam = unany#formal_param (get_any acc1)
    in
    let sx:simple_unbounded_bound_symbol = {
	param             = sparam;
	tuple             = x.tuple
    }
    in set_any acc1 (Any_unbounded_bound_symbol sx)



  (** Bounded_bound_symbol **)	       
  method bounded_bound_symbol acc x =
    let rec f acct l = match l with
      | [] -> (acct,[])
      | t::q -> let (acctemp,q') = f acct q in
		let acct' = self#formal_param acctemp t in
		let t'=unany#formal_param (get_any acct') in
		(acct',(t'::q'))
    in
    let (acc1,sparams) = f acc x.params
    in
    let acc2 = self#expr acc1 x.domain
    in
    let sdomain = unany#expr (get_any (acc2))
    in
    let sx:simple_bounded_bound_symbol = {
	params            = sparams;
	tuple             = x.tuple; 
	domain            = sdomain 
    }
    in
    set_any acc2 (Any_bounded_bound_symbol sx)


  (** Lambda **)	       
  method lambda acc x =
    let rec f acct l = match l with
      | [] -> (acct,[])
      | (t,b)::q -> let (acctemp,q') = f acct q in
		let acct' = self#formal_param acctemp t in
		let t'=unany#formal_param (get_any acct') in
		(acct',((t',b)::q'))
    in
    let (acc1,sparams) = f acc x.params
    in
    let acc2 = self#expr acc1 x.body
    in
    let sbody = unany#expr (get_any acc2)
    in
    let sx:simple_lambda = {
	location          = x.location;
	level             = x.level;
	arity             = x.arity; 
	body              = sbody; 
	params            = sparams
    }
    in set_any acc2 (Any_lambda sx)


	       
  (** Assume_prove **)	       
  method assume_prove acc x =
    let rec f1 acct l = match l with
      | [] -> (acct,[])
      | t::q -> let (acctemp,q') = f1 acct q in
		let acct' = self#new_symb acctemp t in
		let t'=unany#new_symb (get_any acct') in
		(acct',(t'::q'))
    in
    let (acc1,sns) = f1 acc x.new_symbols
    in
        let rec f2 acct l = match l with
      | [] -> (acct,[])
      | t::q -> let (acctemp,q') = f2 acct q in
		let acct' = self#assume_prove acctemp t in
		let t'=unany#assume_prove (get_any acct') in
		(acct',(t'::q'))
    in
    let (acc2,sassumes) = f2 acc1 x.assumes
    in
    let acc3 = self#expr acc2 x.prove
    in
    let sprove = unany#expr (get_any acc3)
    in
    let sx = {
	location          = x.location;
	level             = x.level;
	new_symbols       = sns;
	assumes           = sassumes;
	prove             = sprove
      }
    in
    set_any acc3 (Any_assume_prove sx)


	       
  (** New symb **)	
  method new_symb acc x =
    let (acc1,sset) = match x.set with	
      | None -> (acc,None)
      | Some e ->
	 let acct = self#expr acc e
	 in
	 let se = unany#expr (get_any acct)
	 in (acct,Some se)
    in
    let acc2 = self#op_decl acc x.op_decl
    in
    let sop_decl = unany#op_decl (get_any  acc2) in
    let sx = {
	location          = x.location;
	level             = x.level;
	op_decl           = sop_decl; 
	set               = sset
    }
    in set_any acc2 (Any_new_symb sx)


	       
  (** Op_def **)				    
  method op_def acc x = match x with
    | O_module_instance _ -> failwith "Can not convert modules!"
    | O_user_defined_op udo -> 
       let acc1 = self#user_defined_op acc udo
       in
       let sudo = unany#user_defined_op (get_any acc1)
       in
       let sx = O_user_defined_op sudo
       in set_any acc1 (Any_op_def sx) 
    | O_builtin_op bo ->
       let acc1 = self#builtin_op acc bo
       in
       let sbo = unany#builtin_op (get_any acc1)
       in
       let sx = O_builtin_op sbo
       in set_any acc1 (Any_op_def sx)


	       
  (** Builtin_op **)			 
  method builtin_op acc x =
    let rec f acct l = match l with
      | [] ->
	 (acct,[])
      | (t,b)::q ->
	 let (acctemp,q') = f acct q in
    	 let acct' = self#formal_param acctemp t in
    	 let t'=unany#formal_param (get_any acct') in
    	 (acct',((t',b)::q'))
    in
    let (acc1,sparams) = f acc x.params
    in
    let sx:simple_builtin_op = {
	level             = x.level;
	name              = x.name;
	arity             = x.arity; 
	params            = sparams
    }
    in
    set_any acc1 (Any_builtin_op sx)


	    
  (** Expr_or_module_or_module_instance **) 
  method expr_or_module_or_module_instance acc x = match x with
   | EMM_expr e -> 
      let acc1 = self#expr acc e
      in
      let se = unany#expr (get_any acc1)
      in
      let sx = EMM_expr se
      in set_any acc1 (Any_expr_or_module_or_module_instance sx)
   | EMM_module _ -> failwith "Can not convert modules!"
   | EMM_module_instance _ -> failwith "Can not convert modules!"

		   


  (** ----------------------------------------------**)			       
  (** recursive expressions with reference **)


  (** Formal_param **) 		   
  method formal_param acc x =

    let fp_:formal_param_ = dereference_formal_param (get_term_db acc) x
    in
    let sfp_:simple_formal_param_ =  {
        location          = fp_.location;
        level             = fp_.level;
        name              = fp_.name;
        arity             = fp_.arity;
      }
    in
    match x with
    | FP_ref i ->
       let rec add (j,t) l = match l with
	 | [] -> [(j,t)]
	 | (k,_)::tl when j=k -> l
	 | hd::tl -> hd::(add (j,t) tl)
       in
      let stdb2 = add (i,FP_entry sfp_) (get_simple_term_db acc)
      in
      let acc2 = (set_simple_term_db acc stdb2)
      in
      set_any acc2 (Any_formal_param (FP_ref i))
    | FP fp_ -> let sx = FP sfp_
		in set_any acc (Any_formal_param sx)

			   
  (** Op_decl **) 		   		 
  method op_decl acc x =
    let od_:Expr_ds.op_decl_ = dereference_op_decl (get_term_db acc) x
    in
    let sod_:simple_op_decl_ = {
        location          = od_.location;
        level             = od_.level;
        name              = od_.name;
        arity             = od_.arity;
        kind              = od_.kind;
      }
    in
    match x with
    | OPD_ref i ->
       let rec add (j,t) l = match l with
	 | [] -> [(j,t)]
	 | (k,_)::tl when j=k -> l
	 | hd::tl -> hd::(add (j,t) tl)
       in
      let stdb2 = add (i,OPDec_entry sod_) (get_simple_term_db acc)
      in
      let acc1 = (set_simple_term_db acc stdb2)
      in
      set_any acc1 (Any_op_decl (OPD_ref i))
    | OPD od_ -> let sx = OPD sod_
		 in set_any acc (Any_op_decl sx)

			    
  (** User_defined_op **) 		   		 			    
  method user_defined_op acc x =
    let udo_:Expr_ds.user_defined_op_ = dereference_user_defined_op (get_term_db acc) x
    in
    let rec f acct l = match l with
      | [] -> (acct,[])
      | (t,b)::q -> let (acctemp,q') = f acct q in
		let acct' = self#formal_param acctemp t in
		let t'=unany#formal_param (get_any acct') in
		(acct',t'::q')
    in
    let (acc1,sparams) = f acc udo_.params
    in
    let acc2 = self#expr acc1 udo_.body
    in
    let sbody = unany#expr (get_any acc2)
    in
    let sudo_ = {
        location          = udo_.location;
        level             = udo_.level;
        name              = udo_.name;
        arity             = udo_.arity;
        body              = sbody;
        params            = sparams;
        recursive         = udo_.recursive;
      }
    in
    match x with
    | UOP_ref i ->
       let rec add (j,t) l = match l with
	 | [] -> [(j,t)]
	 | (k,_)::tl when j=k -> l
	 | hd::tl -> hd::(add (j,t) tl)
       in
      let stdb2 = add (i,OPDef_entry (O_user_defined_op (UOP sudo_))) (get_simple_term_db acc2)
      in
      let acc3 = (set_simple_term_db acc2 stdb2)
      in
      set_any acc3 (Any_user_defined_op (UOP_ref i))
    | UOP _ -> let sx = UOP sudo_
	       in set_any acc2 (Any_user_defined_op sx)





  (** ----------------------------------------------**)			       		  
  (** global constructors **)


  (** Operator **)
  method operator acc x = match x with
    | FMOTA_formal_param e ->
       let acc1 = self#formal_param acc e
       in
       let se = unany#formal_param (get_any acc1)
       in
       let sx = FMOTA_formal_param se
       in set_any acc1 (Any_operator sx)
    | FMOTA_op_decl e ->
       let acc1 = self#op_decl acc e
       in
       let se = unany#op_decl (get_any acc1)
       in
       let sx = FMOTA_op_decl se
       in set_any acc1 (Any_operator sx)
    | FMOTA_op_def e ->
       let acc1 = (self#op_def acc e)
       in
       let se = unany#op_def (get_any acc1)
       in
       let sx = FMOTA_op_def se
       in set_any acc1 (Any_operator sx)
    | FMOTA_theorem e -> failwith "TODO : extract statement from theorems"
    | FMOTA_module _ -> failwith "Cannot convert module!"
    | FMOTA_assume _ -> failwith "Cannot convert assume!"
    | FMOTA_ap_subst_in _ -> failwith "Cannot convert ap_subst_in!"
       


  (** Expr **)				      
  method expr acc x = match x with
    | E_decimal e -> 			 
      let se = unany#decimal (get_any (self#decimal acc e))
      in
      let sx = E_decimal se
      in set_any acc (Any_expr sx)
    | E_numeral e -> 			 
      let se = unany#numeral (get_any (self#numeral acc e))
      in
      let sx = E_numeral se
      in set_any acc (Any_expr sx)
    | E_string e -> 			 
      let se = unany#strng (get_any (self#strng acc e))
      in
      let sx = E_string se
      in set_any acc (Any_expr sx)
    | E_lambda e -> 			 
      let acc1 = self#lambda acc e
      in
      let se = unany#lambda (get_any acc1)
      in
      let sx = E_lambda se
      in set_any acc1 (Any_expr sx)
    | E_op_appl e ->
      let acc1 = self#op_appl acc e
      in
      let se = unany#op_appl (get_any acc1)
      in
      let sx = E_op_appl se
      in
      set_any acc1 (Any_expr sx)
    | E_binder e ->
       let acc1 = self#binder acc e
       in
       let se = unany#binder (get_any acc1)
       in
       let sx = E_binder se
       in set_any acc1 (Any_expr sx)
    | E_subst_in e -> failwith "remove -subst_in- during preprocessing"
    | E_label _ -> failwith "remove -label- during preprocessing"
    | E_at _ -> failwith "remove -at- during preprocessing"
    | E_let_in _ -> failwith "remove -let_in- during preprocessing"
		     

  (** Entry **)				      
  method entry acc x = match x with
    | (i, FP_entry fp_) ->
      let sfp_:simple_formal_param_ =  {
        location          = fp_.location;
        level             = fp_.level;
        name              = fp_.name;
        arity             = fp_.arity;
	}
      in set_any acc (Any_entry (i,FP_entry sfp_))
    | (i, OPDec_entry od_) ->
      let sod_:simple_op_decl_ = {
        location          = od_.location;
        level             = od_.level;
        name              = od_.name;
        arity             = od_.arity;
        kind              = od_.kind;
      }
      in set_any acc (Any_entry (i,OPDec_entry sod_))
    | (i, OPDef_entry op) ->
      let acc1 = self#op_def acc op
      in
      let sop = unany#op_def (get_any acc1)
      in set_any acc1 (Any_entry (i,OPDef_entry sop))
    | (_,MOD_entry _) -> failwith "Can not get module as entry" 			 
    | (_,THM_entry _) -> failwith "Can not get theorem as entry" 			 
    | (_,ASSUME_entry _) -> failwith "Can not get assume as entry" 			 
    | (_,APSUBST_entry _) -> failwith "Can not get ap_subst as entry" 			 
			     
end

			          

			      

(** ----------------------------------------------**)			       
(** Entry point **)
			      
let parse_expr termdb assume_prove =
  let converter = new expr_to_simple_expr in
  let acc = (termdb, [], Nothing ) in
  let (_, stermdb, pre_ap) =
    converter#assume_prove acc assume_prove
  in
  (stermdb, converter#get_unany#assume_prove pre_ap)
