open Kaputt.Abbreviations
open Commons
open Any_expr
open Expr_ds
open Expr_map
open Expr_formatter
open Expr_builtins
open Expr_substitution
open Expr_termdb_utils
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
                         tuple=false;
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
                          tuple = false;
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

let test_fun substs () =
  fprintf std_formatter "Input:  %a@," (fmt_expr term_db) formula;
  let (t1, term_db1) = subst_expr term_db substs formula in
  fprintf std_formatter "Output: %a@," (fmt_expr term_db1) t1;
  (*
  fprintf std_formatter "tdb ids: %a@,"
          (fmt_list ~front:"[" ~sep:", " ~back:"]"
                    (fun f ->
                     function (x,_) ->
                              fprintf f "%d" x
                    )
          )
          term_db1
          ;
   *)
  fprintf std_formatter "%!";
  Assert.equal_bool ~msg:"Termdb after sub is inconsistent!"
                    (is_consistent term_db1) true;
  ()

let test_subst substs =
  Test.make_assert_test
    ~title: ("testing substitution on custom expressions.")
    (fun () -> ())
    (test_fun substs)
    (fun () -> ()  )


let get_tests = [test_subst sub1; test_subst sub2]
