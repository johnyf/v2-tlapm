open Commons
open Expr_ds
open Expr_map
open Expr_utils
open Expr_prover_parser
open Expr_dereference
open Any_expr

type 'a ptacc = term_db option * 'a

let get_ptacc_tdb (db,_) = db
let tdb macc =
  match get_ptacc_tdb (get_acc macc) with
  | Some db -> db
  | _ -> failwith "Accumulator does not contain the term database!"

let set_tdb macc db =
  let (_, rest) = get_acc macc in
  set_acc macc (db, rest)

class ['a] expr_parse_theorems =
object(self)
inherit ['a ptacc] expr_map as super

method private parse_formula acc ({ location; level; new_symbols;
                    assumes; prove; suffices; boxed; }) thm =
  match assumes, prove, match_function (tdb acc) prove with
  |  _::_, _, _ ->
      (* nonempty assumptions are a normal formula, don't change *)
      super#theorem acc thm
  |  _, _, Some ("$Pfcase", args) ->
      (* CASE proof step *)
       (* Printf.printf "Case!"; *)
      (* recurse on subterms *)
      let acc1 = super#theorem acc thm in
      let extract = self#get_macc_extractor in
      let {location; level; name; statement; proof } =
        match extract#theorem acc1 with
        | THM_ref  _ -> failwith "Expected theorem, not theorem ref!"
        | THM x -> x
      in
      (* create new theorem and update accumulator *)
      let statement = match args with
      | [EO_expr expr] -> ST_CASE expr
      | [EO_op_arg _] ->
         failwith "Don't know what to do with op arg passed to case step!"
      | _ ->
         failwith "Step case operator expects exactly one argument!"
      in
      let thm = THM {location; level; name; statement; proof } in
      set_anyexpr acc1 (Any_theorem thm)
  |  _, _, Some ("$Have", args) ->
      (* HAVE proof step *)
      (* recurse on subterms *)
      let acc1 = super#theorem acc thm in
      let extract = self#get_macc_extractor in
      let {location; level; name; statement; proof } =
        match extract#theorem acc1 with
        | THM_ref  _ -> failwith "Expected theorem, not theorem ref!"
        | THM x -> x
      in
      (* create new theorem and update accumulator *)
      let statement = match args with
      | [EO_expr expr] -> ST_HAVE expr
      | [EO_op_arg _] ->
         failwith "Don't know what to do with op arg passed to case step!"
      | _ ->
         failwith "Step case operator expects exactly one argument!"
      in
      let thm = THM {location; level; name; statement; proof } in
      set_anyexpr acc1 (Any_theorem thm)
  |  _, E_binder { operator = FMOTA_op_def opd ;
                   operand;
                   bound_symbols; _ }, None ->
      (* pick proof step *)
      (
      match opd with
      | O_builtin_op { name = "$Pick";  _  } ->
         (
         Printf.printf "Pick! %s" (format_location location); (* *)
         (* recurse on subterms *)
         let acc1 = super#theorem acc thm in
         let extract = self#get_macc_extractor in
         let {location; level; name; statement; proof } =
           match extract#theorem acc1 with
           | THM_ref  _ -> failwith "Expected theorem, not theorem ref!"
           | THM x -> x
         in
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
         let thm = THM { location; level; name; statement; proof;}  in
         set_anyexpr acc1 (Any_theorem thm)
         )
      | _ -> super#theorem acc thm
      )
  |  _, _, Some("$Qed", []) ->
      (* Qed proof step *)
      (* Printf.printf "Qed!"; *)
      (* recurse on subterms *)
      let acc1 = super#theorem acc thm in
      let extract = self#get_macc_extractor in
      (* create new theorem and update accumulator *)
      let thmi =
        match extract#theorem acc1 with
        | THM_ref  _ -> failwith "Expected theorem, not theorem ref!"
        | THM x -> x
      in
      let thm = THM { thmi with statement = ST_QED }  in
      set_anyexpr acc1 (Any_theorem thm)
  | _ ->
     super#theorem acc thm

method private parse_suffices acc {location; level; new_symbols;
                                   assumes; prove; suffices; boxed;} t =
  match assumes, prove with
  | _::_, _ ->
  (* SUFFICES ASSUME ... PROVE ... should already be handled *)
     super#theorem acc (THM t)
  | [], exp ->
     (* SUFFICES F still has the operator we need to strip *)
     match match_function (tdb acc) exp with
     | None -> failwith "Expected suffices as prove operator!"
     | Some ("$Suffices", [EO_expr expr]) ->
        let acc1 = self#expr acc expr in
        let extract = self#get_macc_extractor in
        let prove = extract#expr acc1 in
        let ap = { location; level; new_symbols; assumes;
                   prove; boxed; suffices; } in
        let acc2 = self#proof acc1 t.proof in
        let proof = extract#proof acc2 in
        let thm = THM {location = t.location; level = t.level; name = t.name;
                   statement = ST_SUFFICES ap; proof; } in
        set_anyexpr acc (Any_theorem thm)
     | Some ("$Suffices", [EO_op_arg oa]) ->
        failwith "suffices found, but has an op arg, not an expr as argument!";
     | Some ("$Suffices", args) ->
        failwith "Expected only one argument of suffices!"
     | Some (name, _ ) ->
        failwith ("Expected application of suffices, but found " ^ name)

method theorem acc thm = match thm with
  | THM_ref i -> super#theorem acc thm
  | THM ({ location; level; name; statement; proof; } as thmi) ->
     (
     match statement with
     | ST_FORMULA f ->
        self#parse_formula acc f thm
     | ST_SUFFICES f ->
        (* Printf.printf "ghg %s\n" (Commons.format_location location); *)
     (* remove suffices constant *)
        self#parse_suffices acc f thmi
     | _ ->
        (* skip other proof step *)
        super#theorem acc thm
     )

method context acc { root_module; entries; modules } =
  let acc1 = set_tdb acc (Some entries) in
  super#context acc1 {root_module; entries; modules }
end
