open Kaputt.Abbreviations
open Commons
open Any_expr
open Expr_ds
open Expr_map
open Expr_formatter
open Expr_builtins
open Expr_substitution
open Expr_termdb_utils
open Expr_dereference
open Util
open List
open Test_common
open Format


let location = mkDummyLocation

let term_db : term_db = [
    (* op declarations *)
    (  1, OPDec_entry { location; level = Some ConstantLevel; name = "x";
                      arity = 0; kind = ConstantDecl } );
    (  2, OPDec_entry { location; level = Some ConstantLevel; name = "y";
                      arity = 0; kind = ConstantDecl } );
    (  3, OPDec_entry { location; level = Some ConstantLevel; name = "P";
                      arity = 2; kind = ConstantDecl } );
    (  4, OPDec_entry { location; level = Some ConstantLevel; name = "Q";
                      arity = 2; kind = ConstantDecl } );
    (  5, OPDec_entry { location; level = Some ConstantLevel; name = "f";
                      arity = 1; kind = ConstantDecl } );
    (* formal parameters *)
    (101, FP_entry { location; level = Some ConstantLevel; name = "x";
                     arity = 0  });
    (102, FP_entry { location; level = Some ConstantLevel; name = "y";
                     arity = 0  });
    (103, FP_entry { location; level = Some ConstantLevel; name = "z";
                     arity = 0  });
  ]

let applyPxy pref xref yref =
  E_op_appl {
      location;
      level = Some ConstantLevel;
      operator = FMOTA_op_decl (OPD_ref pref);
      operands = [
          EO_op_arg {
              location;
              level = None;
              argument =
                FMOTA_formal_param (FP_ref xref)
            };
          EO_op_arg {
              location;
              level = None;
              argument =
                FMOTA_formal_param (FP_ref yref)
            };
        ];
    }

(* Corresponds to \A y : P(y,z)*)
let domain = E_binder {
                 location; level = Some ConstantLevel;
                 operator = FMOTA_op_def (O_builtin_op unbounded_forall);
                 operand = EO_expr (applyPxy 3 102 103);
                 bound_symbols = [
                     B_unbounded_bound_symbol {
                         param = FP_ref 102;
                         tuple = false;
                       }
                   ];
               }

(* Corresponds to \A x \in (\A y \in P(y,z)) : Q(x,z) *)
let formula = E_binder {
                  location; level = Some ConstantLevel;
                  operator = FMOTA_op_def (O_builtin_op bounded_forall);
                  operand = EO_expr (applyPxy 4 101 103);
                  bound_symbols = [
                      B_bounded_bound_symbol {
                          params = [FP_ref 101];
                          tuple  = false;
                          domain;
                        }
                    ];
                }

let sub1 = [Subst.Subst
              (FP_ref 103,
               EO_op_arg { location; level = Some ConstantLevel;
                           argument = FMOTA_formal_param (FP_ref 101) })]
let sub2 = [Subst.Subst
              (FP_ref 103,
               EO_op_arg { location; level = Some ConstantLevel;
                           argument = FMOTA_formal_param (FP_ref 102) })]

(* helpers *)
class free_bound_lists_visitor = object
  inherit [(formal_param list) * (op_decl list) * (formal_param list)]
      tdb_visitor as super

  method unbounded_bound_symbol ((bs, fs, allfps), tdb) ({ param; _ } as sym ) =
    let acc = ((append bs [param], fs, allfps), tdb) in
    super#unbounded_bound_symbol acc sym

  method bounded_bound_symbol ((bs, fs, allfps), tdb) ({ params; _ } as sym ) =
    let acc = ((append bs params, fs, allfps), tdb) in
    super#bounded_bound_symbol acc sym

  method op_decl ((bs, fs, allfps), tdb) opd =
    let acc = ((bs, append fs [opd], allfps), tdb) in
    super#op_decl acc opd

  method formal_param ((bs, fs, allfps), tdb) fp =
    let acc = ((bs, fs, append allfps [fp]), tdb) in
    super#formal_param acc fp
end

(* extracts bound variables, free variables and all formal parameters from an
   expression. preserves duplications. *)
let free_bound_lists tdb expr =
  let r, _ = (new free_bound_lists_visitor)#expr (([],[],[]), tdb) expr in
  r

let intersect list1 list2 =
  filter (fun x -> mem x list2) list1

let intersect_free_bound tdb exp =
  let ( bound_vars, free_vars, all_fp) = free_bound_lists term_db formula in
  let free_names = map (fun x ->
      let opdi = dereference_op_decl tdb x in
      opdi.name
    ) free_vars in
  let bound_names = map (fun x ->
      let opdi = dereference_formal_param tdb x in
      opdi.name
    ) bound_vars in
  let free_and_bound = intersect free_names bound_names in
  fprintf std_formatter "@[Bound vars: %a@]@,"
    (fmt_list (fmt_formal_param term_db)) bound_vars ;
  fprintf std_formatter "@[Free vars: %a@]@,"
    (fmt_list (fmt_op_decl term_db)) free_vars ;
  let context_bound = filter (fun x -> not (mem x bound_vars)) all_fp in
  fprintf std_formatter "@[Context bound vars: %a@]@,"
    (fmt_list (fmt_formal_param term_db)) context_bound ;
  free_and_bound

let test_subst substs =
  Test.make_assert_test
    ~title: ("testing substitution on custom expressions.")
    (fun () -> ())
    (fun () ->
       fprintf std_formatter "@[<v>";
       fprintf std_formatter "@[Subst: %a@]@,"
         (SubFormat.fmt_substs term_db) substs;
       fprintf std_formatter "@[Input:  %a@]@,"
         (fmt_expr term_db) formula;
       let fb = intersect_free_bound term_db formula in
       let fbstring = asprintf "%a" (fmt_list pp_print_string) fb in
       Assert.equal
         ~msg:"Free and bound variable names don't intersect in input."
         fbstring "[]";
       let (t1, term_db1) = subst_expr term_db substs formula in
       fprintf std_formatter "@[Output: %a@]@," (fmt_expr term_db1) t1;
       Assert.equal_bool ~msg:"Termdb after sub is inconsistent!"
         (is_consistent term_db1) true;
       let fb1 = intersect_free_bound term_db1 t1 in
       let fbstring1 = asprintf "%a" (fmt_list pp_print_string) fb1 in
       fprintf std_formatter "@]@.";
       Assert.equal
         ~msg:"Free and bound variable names don't intersect in output."
         fbstring1 "[]";
       ()
    )
    (fun () -> ()  )


let test_rename =
  Test.make_assert_test
    ~title:"testing substition range"
    (fun () -> ())
    (fun () ->
       let rg1 = Subst.formal_params_in_range term_db sub1 in
       Assert.equal ~msg:"Sub1 maps exactly one variable." (length sub1) 1;
       let msg = asprintf "Range %a of sub1 must contain x."
           (fmt_list (fmt_formal_param term_db)) rg1 in
       Assert.equal ~msg (mem (FP_ref 101) rg1) true;
       Assert.equal ~msg:"Sub2 maps exactly one variable." (length sub2) 1;
       let rg2 = Subst.formal_params_in_range term_db sub2 in
       let msg = asprintf "Range %a of sub2 must contain y."
           (fmt_list (fmt_formal_param term_db)) rg1 in
       Assert.equal ~msg (mem (FP_ref 102) rg2) true;
       ()
    )
    (fun () -> ())

let get_tests = [test_subst sub1; test_subst sub2; test_rename]
