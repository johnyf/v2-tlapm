open Util
open Commons
open Expr_ds
open Expr_map
open Expr_utils
open Expr_prover_parser
open Expr_dereference
open Any_expr

let step_names = ["$Pfcase"; "$Have"; "$Pick"; "$Suffices"; "$Witness"]

type 'a ptacc = PTacc of term_db option * (node * theorem_def) IntMap.t * 'a

let get_ptacc_tdb (PTacc (db,_,_)) = db
let get_ptacc_thm_map (PTacc (_,map,_)) = map

let tdb macc =
  match get_ptacc_tdb (get_acc macc) with
  | Some db -> db
  | _ -> failwith "Accumulator does not contain the term database!"

let get_thm_map macc = get_acc macc |> get_ptacc_thm_map


let set_tdb macc db =
  let PTacc (_, map, rest) = get_acc macc in
  set_acc macc (PTacc (db, map, rest))

let set_thm_map macc map =
  let PTacc (db, _, rest) = get_acc macc in
  set_acc macc (PTacc (db, map, rest))

let add_statement  acc id node def =
  let map = get_thm_map acc in
  let map0 = IntMap.add id (node, def) map in
  set_thm_map acc map0

(* TODO: check if theorem and def rewriting agrees, handle WITNESS *)
class ['a] expr_parse_theorems =
  object(self)
    inherit ['a ptacc] expr_map as super

    (* Because TAKE x \in Nat is a SANY op appl with bound variable x but no
       body, it is rewritten in sany_exp *)
    method private parse_formula acc thm = function
      | N_assume_prove _
      | N_ap_subst_in _ ->
        (* proof step symbols are only explicit expressions *)
        super#theorem_ acc thm
      | N_expr expr ->
        (* recurse on subterms (for the subproofs) *)
        let acc1 = super#theorem_ acc thm in
        let extract = self#get_macc_extractor in
        let {id; location; level; definition; statement; proof } =
          extract#theorem_ acc1
        in
        let term_db = tdb acc1 in
        match expr, match_function (tdb acc) expr with
        |  _, Some ("$Pfcase", args) ->
          (* CASE proof step *)
          (* Printf.printf "Case!"; *)
          (* create new theorem and update accumulator *)
          let statement, acc2 = match args with
            | [EO_expr expr] ->
              let acc2 = CCOpt.map_or ~default:acc1
                  (add_statement acc1 id (N_expr expr)) definition in
              ST_CASE expr, acc2
            | [EO_op_arg _] ->
              failwith "Don't know what to do with op arg passed to case step!"
            | _ ->
              failwith "Step case operator expects exactly one argument!"
        in
        let thm = {id; location; level; definition; statement; proof } in
        set_anyexpr acc2 (Any_theorem_ thm)
        |  _, Some ("$Have", args) ->
          (* HAVE proof step *)
          (* create new theorem and update accumulator *)
          let statement, acc2 = match args with
            | [EO_expr expr] ->
              let acc2 = CCOpt.map_or ~default:acc1
              (add_statement acc1 id (N_expr expr)) definition in
              ST_HAVE expr, acc2
            | [EO_op_arg _] ->
              failwith "Don't know what to do with op arg passed to case step!"
            | _ ->
              failwith "Step case operator expects exactly one argument!"
          in
          let thm = {id; location; level; definition; statement; proof } in
          set_anyexpr acc2 (Any_theorem_ thm)
        |  E_binder { operator = FMOTA_op_def opd ;
                      operand;
                      bound_symbols; _ }, None ->
          (* pick proof step *)
          (
            match opd with
            | O_builtin_op bop
              when (Deref.builtin_op term_db bop).name ="$Pick" ->
              (
                (* Printf.printf "Pick! %s" (format_location location); *)
                (* change formula to pick version *)
              let formula = match operand with
                | EO_expr expr -> expr
                | EO_op_arg _ ->
                  failwith ("Don't know what to do with an op_arg as" ^
                            " parameter for PICK!")
                in
                let statement = ST_PICK { variables = bound_symbols;
                                          formula; } in
                (* create new theorem and update accumulator *)
                let thm = { id; location; level; definition; statement; proof;}  in
                let acc2 = CCOpt.map_or
                    ~default:acc1
                    (add_statement acc1 id (N_expr expr)) definition in
                set_anyexpr acc2 (Any_theorem_ thm)
              )
            | _ -> super#theorem_ acc thm
          )
        |  _, Some("$Qed", []) ->
          (* Qed proof step *)
          (* Printf.printf "Qed!"; *)
          (* create new theorem and update accumulator *)
          let thm = {id; level; location; definition;
                     statement = ST_QED; proof }  in
          let acc2 = CCOpt.map_or
              ~default:acc1
              (add_statement acc1 id (N_expr expr)) definition in
          set_anyexpr acc2 (Any_theorem_ thm)
        | _ ->
          super#theorem_ acc thm

    method private parse_suffices acc t = function
      | N_ap_subst_in _
      | N_assume_prove _ ->
        (* SUFFICES ASSUME ... PROVE should already be handled in sany_expr *)
        super#theorem_ acc t
      | N_expr expr ->
        (* recurse on subterms (for the subproofs) *)
        let acc1 = super#theorem_ acc t in
        let extract = self#get_macc_extractor in
        let {id; location; level; definition; statement; proof } =
          extract#theorem_ acc1
        in
        (* SUFFICES F still has the operator we need to strip *)
        match match_function (tdb acc) expr with
        | None -> failwith "Expected suffices as prove operator!"
        | Some ("$Suffices", [EO_expr expr]) ->
          let acc2= self#expr acc expr in
          let statement = ST_SUFFICES (N_expr (extract#expr acc2)) in
          let thm = {id; location; level; definition;
                     statement; proof; } in
          set_anyexpr acc (Any_theorem_ thm)
        | Some ("$Suffices", [EO_op_arg oa]) ->
          failwith "suffices found, but has an op arg, not an expr as argument!";
        | Some ("$Suffices", args) ->
          failwith "Expected only one argument of suffices!"
        | Some (name, _ ) ->
          failwith ("Expected application of suffices, but found " ^ name)

    method theorem_ acc
        ({ id; location; level; definition; statement; proof; } as thmi) =
      match statement with
          | ST_FORMULA f ->
            self#parse_formula acc thmi f
          | ST_SUFFICES f ->
            (* Printf.printf "ghg %s\n" (Commons.format_location location); *)
            (* remove suffices constant *)
            self#parse_suffices acc thmi f
          | _ ->
            (* skip other proof step *)
            super#theorem_ acc thmi

    method theorem_def_ acc ({id; location; level; name; body} as td)=
      match body with
      | N_expr (E_op_appl {operator = FMOTA_op_def (O_builtin_op bop); operands;_}) ->
        begin
          match (Deref.builtin_op (tdb acc) bop).name, operands with
          | "$Pfcase", [EO_expr e]
          | "$Have", [EO_expr e]
          | "$Pick", [EO_expr e]
          | "$Suffices", [EO_expr e] ->
            let acc1 = super#theorem_def_ acc td in
            let td1 = self#get_macc_extractor#theorem_def_ acc1 in
            let body = N_expr e in
            set_anyexpr acc1 (Any_theorem_def_ {td1 with body})
          | name, _ when List.mem name step_names ->
            let msg = CCFormat.sprintf "Unhandled proof step %s at %a"
                name Commons.fmt_location location in
            failwith msg
          | _, _ ->
            super#theorem_def_ acc td
        end;
      | _ ->
        super#theorem_def_ acc td

    method context acc { root_module; entries; modules } =
      let acc1 = set_tdb acc (Some entries) in
      super#context acc1 {root_module; entries; modules }
  end

let instance = new expr_parse_theorems

let expr_parse_theorems_context context =
  let me = instance#get_macc_extractor in
  let init_acc = (Any_context context,
                  PTacc (Some context.entries,IntMap.empty,())) in
  let acc = instance#context init_acc context in
  let context = me#context acc in
  context
