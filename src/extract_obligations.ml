open Printf
open List
open Commons
open Expr_ds
open Expr_utils
open Expr_dereference
open Expr_visitor
open Obligation
open Expr_prover_parser
open Util


(* customized failwith for obligation extraction *)
let failwith_msg loc msg =
  let e = sprintf "Error extracting obligation at %s: %s"
                  (format_location loc) msg in
  failwith e

(* used to track the nesting level throughout a proof *)
type nesting =
  | Module
  | Expression
  | ProofStep
  | By

(* used to track the currently visible objects *)
type current_context = {
(* the current goal *)
goal : assume_prove option;

(* the facts currently known *)
usable_facts : assume_prove list;

(* the expanded definitions *)
expanded_defs : op_def list;

(* the term database *)
term_db : term_db;

(* visible theorems etc.  not sure if we need that *)
constants         : op_decl list;
variables         : op_decl list;
definitions       : op_def list ;
assumptions       : assume list ;
theorems          : theorem list ;

(* the own step has to be treated differently, so we keep it seperate *)
thm_statements : (theorem_ * assume_prove list) list;
}

let emptyCurrentContext term_db = {
goal = None;
usable_facts = [];
expanded_defs = [];
term_db;
constants = [];
variables = [];
definitions = [];
assumptions = [];
theorems = [];
thm_statements = [];
}


(* the accumulator type - containing an open parameter for derived classes *)
type 'a eoacc = current_context * obligation list * nesting * 'a

(* extractors for the constituents of the accumulator *)
let get_cc (c, _, _, _) = c
let get_obligations (_, o, _, _) = o
let get_nesting (_, _, n, _) = n

(* accumulator updates *)
let update_cc (_,o,n,a) cc = (cc,o,n,a)
let update_obligations (cc,_,n,a) o = (cc,o,n,a)
let update_nesting (cc,o,_,a) n = (cc,o,n,a)

(* convenience function adding one obligation to the accumulator *)
let enqueue_obligation acc newobs =
  let obs = get_obligations acc in
  update_obligations acc (append obs newobs)

(* that doesn't work in general anymore, because builtins are expanded and
    may not be present in the term db anymore *)
let find_builtin term_db opname =
  let check_bopname = function
    | OPDef_entry (O_builtin_op bop) ->
       bop.name = opname
    | _ ->
       false
  in
  match filter (fun x -> check_bopname (snd x) ) term_db with
  | [] ->
     let msg = sprintf "Could not find built in %s in term db!" opname in
     failwith msg
  | [(_,x)] ->
     x
  | _ ->
     let msg = sprintf "Found multiple built ins of %s in term db!" opname in
     failwith msg


let formal_param i arity =
  FP {
  location = mkDummyLocation;
  level = None;
  name = "fparam" ^ (string_of_int i);
  arity;
  }

let bounded_exists =
  O_builtin_op { name = "$BoundedExists";
                 level = None;
                 arity = 1;
                 params = [(formal_param 0 0, false)];  (* TODO: check if this is correct - in sany the quantifier has arity -1 *)
               }

let unbounded_exists =
  O_builtin_op { name = "$UnboundedExists";
                 level = None;
                 arity = 1;
                 params = [(formal_param 0 0, false)]; (* TODO: check if this is correct *)
               }

let set_in =
  O_builtin_op { name = "\\in";
                 level = None;
                 arity = 2;
                 params = [(formal_param 0 0, true)]
               }

let tuple =
  O_builtin_op { name = "$Tuple";
                 level = None;
                 arity = -1;
                 params = [];
               }

(* extracts prover tags from by list *)
let rec split_provers term_db = function
  | [] -> ([],[])
  (* prover pragmas are expression, everything else is ignored *)
  | (EMM_expr x) as element::xs -> (
  let (provers, exprs) = split_provers term_db xs in
  match expr_to_prover term_db x with
  | Some prover ->
     (prover :: provers, exprs)
  | None ->
     (provers,element::exprs)
  )
  | x::xs ->
     let (provers, exprs) = split_provers term_db xs in
     (provers,x::exprs)

(* split facts into list of theorem_ and rest *)
let split_theorem_facts term_db facts =
  let split_theorem_facts_ term_db = fold_left
                                     (fun r fact ->
                                      let (thms, rest) = r in
                                      match fact with
                                      | EMM_expr (E_op_appl appl) ->
                                         (
                                         match appl.operator, appl.operands with
                                         | FMOTA_theorem thm, [] ->
                                            (dereference_theorem term_db thm :: thms, rest)
                                         | _ ->
                                            (thms, fact::rest)
                                         )
                                      | _ ->
                                         (thms, fact::rest)
                                     )
                                     ([],[])
  in
  (* during fold we prepended, reverse lists to preserve order *)
  let (thms, facts) = split_theorem_facts_ term_db facts in
  (rev thms, rev facts)


(* the actual visitor subclass *)
class ['a] extract_obligations =
object(self)
inherit ['a eoacc] visitor

method assume acc a =
  let cc = get_cc acc in
  let assumptions = a :: cc.assumptions in
  update_cc acc { cc with assumptions }

method op_decl acc opdecl =
  let cc = get_cc acc in
  let decl_instance = dereference_op_decl cc.term_db opdecl in
  let ccnew = match decl_instance.kind with
  | ConstantDecl -> { cc with constants = opdecl :: cc.constants }
  | VariableDecl -> { cc with variables = opdecl :: cc.variables }
  | _ -> cc in
  update_cc acc ccnew

method op_def acc opdef =
  let cc = get_cc acc in
  let definitions = opdef :: cc.definitions in
  update_cc acc { cc with definitions }

method private parse_by_def term_db = function
  | UMTA_user_defined_op uop ->
     [O_user_defined_op uop]
  | UMTA_module_instance mi ->
     let mii = dereference_module_instance term_db mi in
     failwith_msg mii.location "don't know what to do with module instance in BY DEF!"
  | UMTA_theorem thm ->
     let thmi = dereference_theorem term_db thm in
     failwith_msg thmi.location "don't know what to do with theorem in BY DEF!"
  | UMTA_assume assume ->
     let assi = dereference_assume term_db assume in
     failwith_msg assi.location "don't know what to do with assume in BY DEF!"

method private by acc (by : by) =
  let cc = get_cc acc in
  (* filter provers from by.facts. No prover means use deafault. *)
  let provers, other_bys = split_provers cc.term_db by.facts in
  let provers =
    match provers with
    | [] -> [Default]
    | x -> x
  in
  (* parse by def and add it to visible defs *)
  let additional_defs : op_def list =
    flat_map (self#parse_by_def cc.term_db) by.defs in
  let expanded_defs = append cc.expanded_defs additional_defs in
  (* parse by references to theorems *)
  let thm_bys, other_bys =
    split_theorem_facts cc.term_db other_bys in
  let extract_name ((thmi:theorem_),lst) = (thmi.name, lst) in
  let cc_thm_stmts = map extract_name cc.thm_statements in
  let thm_by_stmts =
    map (fun (x : theorem_) ->
         match mem_assoc x.name cc_thm_stmts with
         | true -> assoc x.name cc_thm_stmts
         | false -> (
         let name = match x.name with
         | Some n -> n
         | None -> "(unknown)"
         in
         let tnames = mkString (function
                               | (Some x,_) -> x
                               | (None, _) -> "(none)"
                               ) cc_thm_stmts in
         let msg = sprintf "Could not find by theorem %s in %s" name tnames in
         failwith_msg by.location msg
         )
        ) thm_bys
  in
  let thm_assumptions = flatten thm_by_stmts in
  (* extend assumptions with usable facts *)
  let assumes = concat [
                thm_assumptions;
                cc.usable_facts;
                ] in
  match cc.goal with
  | Some ccgoal ->
     printf "Obl %s" (format_location by.location);
     let obligation = {
     goal = { ccgoal with assumes = append assumes ccgoal.assumes };
     expanded_defs;

     constants = cc.constants;
     variables = cc.variables;
     definitions = cc.definitions;
     assumptions = cc.assumptions;
     theorems = cc.theorems;

     provers;
     term_db = cc.term_db;
     } in
     let acc0 = enqueue_obligation acc [obligation] in
     acc0
  | None ->
     (* TODO : add error obligation *)
     failwith_msg by.location "No goal for obligation!"

(* this method is reused for the suffices and case steps. thmi may not be
   changed for that reason. *)
method private update_cc_formula acc (thmi:theorem_) assume_prove =
  let cc = get_cc acc in
  (* TODO: boxed flag might be wrong *)
  let goal = {assume_prove with assumes = []; } in
  let (vs, cs, acs, ss, ts) =
    fold_left (fun (vs,cs,acs,ss,ts) sym ->
               let decl = sym.op_decl  in
               let decli = dereference_op_decl cc.term_db decl in
               match decli.kind with
               | NewVariable -> (decl::vs,cs,acs,ss,ts)
               | NewConstant -> (vs,decl::cs,acs,ss,ts)
               | NewAction   -> (vs,cs,decl::acs,ss,ts)
               | NewState    -> (vs,cs,acs,decl::ss,ts)
               | NewTemporal -> (vs,cs,acs,ss,decl::ts)
               | _ ->
                  let msg = "Unexpected operator kind in ASSUME NEW construct!"
                  in failwith_msg decli.location msg
              ) ([],[],[],[],[]) assume_prove.new_symbols in
  let constants = append cc.constants (rev cs) in
  (* TODO: should we extend the context by action and temporal variables? *)
  let variables = concat [cc.variables; (rev vs); (rev acs); (rev ts) ] in
  let inner_stmt =  (thmi, assume_prove.assumes) in
  let inner_cc = { cc with goal = Some goal;
                           constants;
                           variables;
                           thm_statements = inner_stmt :: cc.thm_statements;
                 }
  in
  let outer_stmt = (thmi, [assume_prove]) in
  let outer_cc = { cc with theorems = (THM thmi) :: cc.theorems;
                           usable_facts = assume_prove :: cc.usable_facts;
                           thm_statements = outer_stmt :: cc.thm_statements;
                 }
  in
  (outer_cc, inner_cc)

method private update_cc_case acc (thmi:theorem_) formula =
  let cc = get_cc acc in
  (* TODO: boxed flag might be wrong *)
  let toprove = match cc.goal with
  | None -> failwith_msg thmi.location
                         "Encountered case statement without active goal!"
  | Some f -> f
  in
  let location = extract_location formula in
  let flevel = extract_level formula in
  let level = match toprove.level, flevel with
  (* TODO: check if the level recomputation is correct *)
  | Some l, Some f when l > f   -> Some l
  | Some l, Some f (* l <= f *) -> Some f
  | Some l, None   (* only l *) -> None
  | None,   Some f (* only f *) -> None
  | None,   None -> None
  in
  let assumes = append toprove.assumes [assume_prove_from_expr formula false] in
  let suffices = false in
  let boxed = false in (* TODO: need to recompute boxed flag *)
  let ap = { toprove with location; level; assumes; suffices; boxed; } in
  let (outer_cc, inner_cc) = self#update_cc_formula acc thmi ap in
  (outer_cc, inner_cc)

(* \sigma pick vars : F(vars) is translated into two steps:
   \rho \E vars : F(vars)
   \sigma SUFFICES ASSUME NEW vars, F(vars)
          PROVE G

   We end up with 3 obligations:
   1: \E vars : F(vars)
   2: ASSUME \E vars : F(vars),
             ASSUME NEW c, F(c) PROVE G
      PROVE G
   3: vars, F(vars) added to assumptions of G

   where 2 is trivial.
 *)
method private update_cc_pick acc (thmi : theorem_) {variables; formula} =
  let cc = get_cc acc in
  (*
  let fmt = Expr_formatter.expr_formatter in
  let state = (Format.str_formatter, cc.term_db, false,
               Expr_formatter.Expression, 0) in
  let str = fmt#expr state formula in
  Printf.printf "Pick formula starts with: %s\n"
                (Format.flush_str_formatter ());
   *)
  let location = extract_location formula in
  let level = extract_level formula in
  let (bounded_qs, unbounded_qs) as qs =
    partition (function
              | B_bounded_bound_symbol _ -> true;
              | _ -> false ) variables in
  let (bounds, quantifier) = match qs with
  | ([], uqs) -> ([], unbounded_exists)
  | (bqs, []) ->
     let get_bounds =
       function
       | B_bounded_bound_symbol x -> x;
       | B_unbounded_bound_symbol _ ->
          let msg =
            "Implementation error: may not encounter unbounded symbols here!"
          in
          failwith_msg thmi.location msg
     in
     (map get_bounds bqs, bounded_exists)
  | _ -> failwith_msg thmi.location
                      "Pick mixes bounded and unbounded quantifiers!"
  in
  let ex_formula = E_binder { location; level;
                              operator = FMOTA_op_def quantifier;
                              operand  = EO_expr formula;
                              bound_symbols = variables;
                            } in
  let bounds_formulas = map (function
                            |  { params;
                                 tuple;
                                 domain;
                               } ->
                                ()
                            )
                            bounds in
  let inner_statement = {location;
                         level;
                         new_symbols = [];
                         assumes = [];
                         prove = ex_formula;
                         suffices = false;
                         boxed = true;
                        } in
  let outer_statement = {location;
                         level;
                         new_symbols = [];
                         assumes = [];
                         prove = formula;
                         suffices = true;
                         boxed = true;
                        } in
  let inner_cc = {cc with
                 goal = Some inner_statement;
                 } in
  let outer_cc = {cc with
                 goal = Some outer_statement;
                 (* check this *)
                 thm_statements = (thmi, [outer_statement]) :: cc.thm_statements;
                 } in
  (outer_cc, inner_cc)

method theorem acc thm =
  let cc = get_cc acc in
  let thmi  = dereference_theorem cc.term_db thm in
  (* we prepare one context for proving the statement and one for continuing *)
  let (oouter_cc, oinner_cc) = match thmi.statement with
  | ST_FORMULA f ->
     let (x,y) = self#update_cc_formula acc thmi f in
     (Some x, Some y)
  | ST_SUFFICES f ->
     let (x,y) = self#update_cc_formula acc thmi f in
     (Some y,Some x)
  | ST_CASE f ->
     let (x,y) = self#update_cc_case acc thmi f in
     (Some x, Some y)
  | ST_PICK f ->
     let (x,y) = self#update_cc_pick acc thmi f in
     (Some x, Some y)
  | ST_HAVE e ->
     (Some cc, Some cc) (* TODO *)
  | ST_TAKE f ->
     (Some cc, Some cc) (* TODO *)
  | ST_WITNESS f ->
     (Some cc, Some cc) (* TODO *)
  | ST_QED ->
     (Some cc, Some cc) (* TODO *)
  in
  (* extract inner proof, if it exists *)
  let acc0 = match oinner_cc with
  | Some inner_cc ->
     let acc0a = update_cc acc inner_cc in
     let acc0b = self#proof acc0a thmi.proof in
     (* we need to reset the current context, usable facts etc are not
        visible outside the sub-proof *)
     let inner_obs = get_obligations acc0b in
     let acc0c = update_obligations acc0b inner_obs in
     acc0c
  | None ->
     acc
  in
  (* extract outer proof if it exists *)
  let acc1 = match oouter_cc with
  | Some outer_cc ->
     update_cc acc0 outer_cc
  | None ->
     acc
  in
  let no_oldobs = length (get_obligations acc) in
  let no_newobs = length (get_obligations acc1) in
  (* assert that no obligations were lost*)
  if (no_newobs < no_oldobs) then failwith "lost obligations!";
  Printf.printf "no of obs: %d delta %d\n" no_newobs (no_newobs - no_oldobs);
  acc1


method proof acc0 = function
  (* TODO *)
  | P_omitted _ -> acc0
  | P_obvious _ -> acc0
  | P_by by ->
     self#by acc0 by
  | P_steps {steps; location; _ } ->
     fold_left self#step acc0 steps
  | P_noproof -> acc0


method use_or_hide acc0 {  location; level; facts; defs; only; hide } =
  (* TODO *)
  acc0

method instance acc _ =
  (* TODO *)
  acc

method context acc { entries; modules;} =
  let old_cc = get_cc acc in
  let new_cc = { old_cc with term_db = entries } in
  let acc0 = update_cc acc new_cc in
  let acc1 = fold_left self#mule acc0 modules in
  acc1

end
