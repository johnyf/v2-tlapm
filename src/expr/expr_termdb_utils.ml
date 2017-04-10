open Util
open List
open Expr_ds
open Expr_visitor
open Expr_dereference


module Tdb = IntMap
type tdb = entry Tdb.t

let error_id = -42 

(* ---------------- not exposed in the module interface ----------------  *)
let to_list set = IntSet.fold (fun x y -> x::y) set []

type acc = MentionedEntry of IntSet.t * IntSet.t
let empty_acc = MentionedEntry (IntSet.empty, IntSet.empty)
let add_mentioned id (MentionedEntry (m, e)) =
  MentionedEntry ((IntSet.add id m), e)
let add_entry id (MentionedEntry (m, e)) =
  MentionedEntry (m, (IntSet.add id e))

class ref_ids = object
  inherit [acc] visitor as super

  method formal_param acc = function
    | FP_ref id -> add_mentioned id acc
    | f -> super#formal_param acc f

  method mule acc = function
    | MOD_ref id -> add_mentioned id acc
    | m -> super#mule acc m

  method op_decl acc = function
    | OPD_ref id -> add_mentioned id acc
    | o -> super#op_decl acc o

  method user_defined_op acc = function
    | UOP_ref id -> add_mentioned id acc
    | o -> super#user_defined_op acc o

  method module_instance acc = function
    | MI_ref id -> add_mentioned id acc
    | m -> super#module_instance acc m

  method theorem acc = function
    | THM_ref id -> add_mentioned id acc
    | t -> super#theorem acc t

  method assume acc = function
    | ASSUME_ref id -> add_mentioned id acc
    | a -> super#assume acc a

  method entry acc = function
    | (id, _) as e -> add_entry id (super#entry acc e)

end

(* An instance of the id extractor. Since the visitor is thread safe
   (it does not contain any members), we can reuse it in all functions.
*)
let inst = new ref_ids

(* Extracts the sets of mentioned ids and entry ids from a term_db. Applies the
   function f on both sets.
*)
let map_term_db f term_db =
  match fold_left inst#entry empty_acc term_db with
  | MentionedEntry (m, e) -> f m e


(* --------------- public interface --------------- *)

let mentioned_ids =
  map_term_db (fun m _ -> to_list m)

let entry_ids =
  map_term_db (fun _ e -> to_list e)

let get_ids =
  map_term_db (fun m e -> to_list (IntSet.union m e))


let inconsistent_entries term_db =
  IntSet.fold (fun x xs -> x::xs) (map_term_db IntSet.diff term_db) []

(* checks if there are mentioned references without term_db entries *)
let is_consistent term_db =
  match inconsistent_entries term_db with
  | [] -> true
  | _ -> false

(* checks if there are entries which are not mentioned at all *)
let unmentioned_entries =
  map_term_db (fun m e -> IntSet.fold (fun x y -> match IntSet.mem x m with
      | false -> x::y
      | true  -> y) e [])

(* checks if there are entries which are not mentioned at all *)
let has_unmentioned_entries term_db =
  match unmentioned_entries term_db with
  | [] -> true
  | _ -> false


(* recursively removes unmentioned entries until only mentioned ones remain *)
let rec prune_term_db term_db =
  match unmentioned_entries term_db with
  | [] -> term_db
  | unmentioned ->
    let term_db' =
      filter (function | (id, _) -> not (mem id unmentioned)) term_db
    in prune_term_db term_db'

(* generates a fresh id, at least 10000 *)
let generate_id term_db =
  let rec max = IntSet.fold
      (fun x ->
         (function
           | None                -> Some x
           | Some cm when x > cm -> Some x
           | Some xm (* else *)  -> Some xm
         )
      )
  in
  map_term_db (fun m e ->
      match max m (max e None) with
      | Some mid when mid >= 10000 -> mid+1
      | Some mid (* else *)       -> 10000
      | None                      -> 10000
    ) term_db

(* Building block for the mkref_xxx function. Performs a reverse look-up of
   ref_content in the term_db. If it is not present, it creates a new id
   (>=10000) and extends the term_db by an entry (id, ref_content).
   Returns the (possibly extended) term_db and a reference to the content.
*)
let mkref_entry cmp mkentry mkref term_db ref_content =
  match filter (function
      | (id, e) -> cmp e (mkentry ref_content)
    ) term_db
  with
  | [] -> (* create new entry *)
    let id = generate_id term_db in
    let term_db' = append term_db [(id, mkentry ref_content)] in
    (term_db', mkref id)
  | [ (id, _) ] ->
    (term_db, mkref id)
  | _ :: _ ->
    failwith "Multiple entries matching when creating a FP reference!"


(* creates a reference to a formal_param and enters into the term_db, if necessary *)
let mkref_formal_param ?compare:(cmp=(=)) =
  mkref_entry cmp (fun x -> FP_entry x) (fun x -> FP_ref x)

(* creates a reference to a module and enters into the term_db, if necessary *)
let mkref_mule ?compare:(cmp=(=)) =
  mkref_entry cmp (fun x -> MOD_entry x) (fun x -> MOD_ref x)

(* creates a reference to an operator declaration and enters into the term_db,
   if necessary *)
let mkref_opdec ?compare:(cmp=(=)) =
  mkref_entry cmp (fun x -> OPDec_entry x) (fun x -> OPD_ref x)

(* creates a reference to a user defined operator and enters into the term_db,
   if necessary *)
let mkref_user_defined_op ?compare:(cmp=(=)) =
  mkref_entry cmp (fun x -> OPDef_entry (O_user_defined_op (UOP x)))
    (fun x -> UOP_ref x)

(* creates a reference to a module instance and enters into the term_db,
   if necessary *)
let mkref_module_instance  ?compare:(cmp=(=)) =
  mkref_entry cmp (fun x -> OPDef_entry (O_module_instance (MI x)))
    (fun x -> MI_ref x)

let termdb_of_tdb tdb =
  Tdb.fold (fun id entry db ->  (id, entry)::db) tdb []

let tdb_of_termdb = List.fold_left
    (fun  map (id,entry) -> Tdb.add id entry map) Tdb.empty


module DeepTraversal = struct
  (* visitor with added term db and dereferencing *)
  class ['a] tdb_visitor = object
    inherit ['a * term_db] visitor as super
    method user_defined_op (vs, tdb) uop =
      let opdefi = dereference_user_defined_op tdb uop in
      super#user_defined_op (vs, tdb) (UOP opdefi)

    method theorem (vs, tdb) thm =
      let thmi = dereference_theorem tdb thm in
      super#theorem (vs, tdb) (THM thmi)
  end

  (* free variables *)
  module OPD_comparable = struct
    type t = op_decl
    let compare = Pervasives.compare
  end
  module OPD_Set = Set.Make( OPD_comparable )


  class free_variables_visitor = object
    inherit [OPD_Set.t] tdb_visitor as super

    method op_decl (vs, tdb) opd =
      super#op_decl ((OPD_Set.add opd vs), tdb) opd
  end

  let fv_object = new free_variables_visitor

  let free_variables tdb expr =
    let vs, _ = fv_object#expr (OPD_Set.empty, tdb) expr in
    OPD_Set.fold (fun x xs -> x::xs) vs []

  (* formal parapemeters *)
  module FP_comparable = struct
    type t = formal_param
    let compare = Pervasives.compare
  end
  module FP_Set = Set.Make( FP_comparable )

  class formal_param_visitor = object
    inherit [FP_Set.t] tdb_visitor as super

    method formal_param (vs, tdb) fp =
      let vs1 = FP_Set.add fp vs in
      super#formal_param (vs1, tdb) fp
  end

  let formal_param_visitor_obj = new formal_param_visitor

  let formal_params tdb expr =
    let vs, _ = formal_param_visitor_obj#expr (FP_Set.empty, tdb) expr in
    FP_Set.elements vs

  (* binders *)
  let formal_params_from_binder tdb { bound_symbols; _ } =
    flat_map (function
        | B_unbounded_bound_symbol { param; _ } ->
          [param]
        | B_bounded_bound_symbol { params; _ } ->
          params
      ) bound_symbols

  module B_comparable = struct
    type t = binder
    let compare = Pervasives.compare
  end
  module B_Set = Set.Make( B_comparable )

  class binder_finder = object
    inherit [B_Set.t] tdb_visitor as super
    method binder (vs, tdb) b =
      super#binder ((B_Set.add b vs), tdb) b
  end

  let bv_object = new binder_finder

  let bound_variables tdb expr =
    let vs, _ = bv_object#expr (B_Set.empty, tdb) expr in
    let bs = B_Set.fold (fun x xs -> (formal_params_from_binder tdb x)::xs) vs []
    in List.concat bs

  (* context bound formal parameters i.e. some which are bound outside the
     expression, e.g. the arguments of a definition  *)
  let context_bound_variables tdb expr =
    let all_fp = formal_params tdb expr in
    let bound_fp = bound_variables tdb expr in
    filter (fun x -> not (mem x bound_fp)) all_fp

end
