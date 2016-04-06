open List
open Expr_ds
open Expr_visitor

(* ---------------- not exposed in the module interface ----------------  *)
module IntOrder = struct
    type t = int
    let compare = Pervasives.compare
  end

module IntSet = Set.Make (IntOrder)

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
