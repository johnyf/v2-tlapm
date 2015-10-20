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
self_step : theorem option;
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
self_step = None;
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

(*
method private parse_by_def = function
  | UMTA_user_defined_op uop ->
     [OPDef (O_user_defined_op uop)]
  | UMTA_module_instance mi ->
     failwith_msg "don't know what to do with module instance in BY DEF!"
  | UMTA_theorem thm ->
     failwith_msg "don't know what to do with theorem in BY DEF!"
  | UMTA_assume assume ->
     failwith_msg "don't know what to do with assume in BY DEF!"


method private by_formula acc (thmi:theorem_) (by:by) formula =
  let cc = get_cc acc in
  let provers, other_bys = split_provers cc.term_db by.facts in
  (* parse by def and add it to visible defs *)
  let additional_defs : op_def list =  flat_map self#parse_by_def by.defs in
  let expanded_defs = append cc.expanded_defs additional_defs in
  (* parse by references to theorems *)
  let thm_assumptions, other_bys =
    split_theorem_facts cc.term_db other_bys in
  (* self references only insert the assumptions - split up *)
  let own_thm, thm_assumptions =
    partition (fun (thm : theorem_) ->
                    thm.name = thmi.name)
                   thm_assumptions in
  let thm_assumptions =
    flat_map (fun (t:theorem_) ->
              match t.statement with
              | ST_FORMULA f -> [f]
              | _ -> [] (* TODO: check *)
             )
             thm_assumptions in
  let own_assumptions = (* TODO: don't duplicate and eror check *)
    flat_map (fun (t:theorem_) ->
              match t.statement with
              | ST_FORMULA {assumes; _ } -> assumes
              | _ -> [] (* TODO: check *)
             ) own_thm in
  (* extend assumptions with usable facts *)
  let assumes = concat [
                own_assumptions;
                thm_assumptions;
                cc.usable_facts;
                ] in
  let obligation = {
  o_type = Formula;
  goal = formula;
  expanded_defs;

  constants = cc.constants;
  variables = cc.variables;
  definitions = cc.definitions;
  assumptions = cc.assumptions;
  theorems = cc.theorems;

  provers;
  term_db = cc.term_db;
  } in
  let racc = enqueue_obligation acc [obligation] in
  let theorems = THM thmi :: cc.theorems in
  let usable_facts = append cc.usable_facts [formula]  in
  update_cc racc { cc with theorems; usable_facts }
 *)

method private update_cc_formula acc (thmi:theorem_) formula =
  let cc = get_cc acc in
  (* TODO: boxed flag might be wrong *)
  let goal = {formula with assumes = []; } in
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
                  failwith_msg decli.location
                               "Unexpected operator kind in ASSUME NEW construct!"
              ) ([],[],[],[],[]) formula.new_symbols in
  let constants = append cc.constants (rev cs) in
  (* TODO: should we extend the context by action and temporal variables? *)
  let variables = concat [cc.variables; (rev vs); (rev acs); (rev ts) ] in
  (cc, { cc with goal = Some goal; constants; variables;  })

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
  | Some l, Some f when l > f -> Some l
  | Some l, Some f (* l <= f *) -> Some f
  | Some l, None   (* only l *) -> None
  | None,   Some f (* only f *) -> None
  | None,   None -> None
  in
  let assumes = append toprove.assumes [assume_prove_from_expr formula false] in
  let suffices = false in
  let boxed = false in (* TODO: need to recompute boxed flag *)
  let ap =  {toprove with location; level; assumes; suffices; boxed; } in
  (cc,cc)


method theorem acc thm =
  let cc = get_cc acc in
  let thmi  = dereference_theorem cc.term_db thm in
  (* we prepare one context for proving the statement and one for continuing *)
  let (outer_cc, inner_cc) = match thmi.statement with
  | ST_FORMULA f ->
     self#update_cc_formula acc thmi f
  | ST_SUFFICES f ->
     let (x,y) = self#update_cc_formula acc thmi f in
     (y,x)
  | ST_CASE f ->
     self#update_cc_case acc thmi f
(*
  | ST_PICK f ->
     self#by_pick acc thmi by f
  | ST_HAVE e ->
     acc (* TODO *)
  | ST_TAKE f ->
     acc (* TODO *)
  | ST_WITNESS f ->
     acc (* TODO *)
  | ST_QED ->
     acc
 *)
  in
(*  let racc = match thmi.proof with
  | P_omitted _ -> acc
  | P_obvious _ -> acc
  | P_by by ->
     (
     )
  | P_steps steps ->
     let acc0 = fold_left self#step acc steps.steps in
     (* we need to reset the current context, usable facts etc are not
        visible outside the sub-proof *)
     update_cc acc0 (get_cc acc)
  | P_noproof -> acc
  in *)
  let no_oldobs = length (get_obligations acc) in
(*  let no_newobs = length (get_obligations racc) in
  (* assert that no obligations were lost*)
  if (no_newobs < no_oldobs) then failwith "lost obligations!";
  Printf.printf "no of obs: %d delta %d\n" no_newobs (no_newobs - no_oldobs);
  racc *)
  acc
end
