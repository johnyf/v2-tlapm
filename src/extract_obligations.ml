open Commons
open Expr_ds
open Expr_dereference
open Expr_visitor
open Obligation
open Expr_prover_parser

(* customized failwith for obligation extraction *)
let failwith_msg msg = failwith ("Error extracting obligation: " ^ msg)

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
  update_obligations acc (List.append obs newobs)


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

let split_theorem_facts term_db facts =
  (* split facts into list of theorem_ and rest *)
  let split_theorem_facts_ term_db =
    List.fold_left
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
  (List.rev thms, List.rev facts)


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

method theorem acc thm =
  let cc = get_cc acc in
  let current_thmi  = dereference_theorem cc.term_db thm in
  let racc = match current_thmi.proof with
  | P_omitted _ -> acc
  | P_obvious _ -> acc
  | P_by by ->
     (* TODO: the context isn't correct, statements like PICK etc
              are not supported yet  *)
     let provers, other_bys = split_provers cc.term_db by.facts in
     (* parse by def and add it to visible defs *)
     let additional_defs : op_def list =
       Util.flat_map
       (function
       | UMTA_user_defined_op uop ->
          [OPDef (O_user_defined_op uop)]
       | UMTA_module_instance mi ->
          failwith_msg "don't know what to do with module instance in BY DEF!"
       | UMTA_theorem thm ->
          failwith_msg "don't know what to do with theorem in BY DEF!"
       | UMTA_assume assume ->
          failwith_msg "don't know what to do with assume in BY DEF!"
       ) by.defs in
     let expanded_defs = List.append cc.expanded_defs additional_defs in
     let thm_assumptions, other_bys =
       split_theorem_facts cc.term_db other_bys in
     (* self references only insert the assumptions - split up *)
     let own_thm, thm_assumptions =
       List.partition (fun (thm : theorem_) ->
                       thm.name = current_thmi.name)
                      thm_assumptions in
     let thm_assumptions =
       List.map (fun (t:theorem_) -> t.expr) thm_assumptions in
     let own_assumptions = (* TODO: don't duplicate and eror check *)
       Util.flat_map (fun (t:theorem_) -> t.expr.assumes) own_thm in
     (* extend assumptions with usable facts *)
     let assumes = List.concat [
                   own_assumptions;
                   thm_assumptions;
                   cc.usable_facts;
                   ] in
     let goal = { current_thmi.expr with assumes} in
     let obligation = {
     goal;
     expanded_defs;

     constants = cc.constants;
     variables = cc.variables;
     definitions = cc.definitions;
     assumptions = cc.assumptions;
     theorems = cc.theorems;

     provers;
     term_db = cc.term_db;
     } in
     enqueue_obligation acc [obligation]
  | P_steps steps ->
     List.fold_left self#step acc steps.steps
  | P_noproof -> acc
  in
  let theorems = thm :: cc.theorems in
  let usable_facts = List.append cc.usable_facts [current_thmi.expr] in
  update_cc racc { cc with theorems; usable_facts }
end
