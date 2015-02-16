open Xmlm

module ContextMap =  Map.Make(struct type t = int let compare : int -> int -> int = compare end) (* Map.Make(Integer) *)

let formatSignal (i : signal) = match i with
  | `El_start tg -> "<" ^ (snd (fst tg)) ^ ">"
  | `El_end -> "</>"
  | `Dtd t -> "dtd"
  | `Data str -> "Data:" ^ str 

let open_tag i tg = let signal = (input i) in match signal  with
  | `El_start tag when (snd (fst tag) = tg) -> ()
  | `El_start tag -> failwith ("Illegal XML start tag: " ^ (snd(fst tag)) ^ ". expected: " ^ tg ^ ".")
  | `El_end -> failwith ("Illegal tag encountered: expecting start tag for: " ^ tg ^
				", but got end tag.")
  | _ -> failwith ("Illegal XML element, expecting open tag: " ^ (formatSignal signal))

let close_tag i tg = match (input i) with
  | `El_end -> () (* can we check that we expect the end to be </tg>? *)
  | `El_start d -> failwith ("Illegal tag encountered: expecting end tag for: " ^ tg ^
				", but got start tag for: " ^ (snd(fst d)))
  | _ -> failwith ("Illegal XML element, expecting close tag: " ^ (formatSignal (peek i)))
    

let read_flag i tg = match (peek i) with
  | `El_start ((_,tg),_) ->
    open_tag i tg;
    close_tag i tg;
    true
  | _ ->
    false
    
let get_data_in i tg f =
  open_tag i tg;
  let ret = f i in
  close_tag i tg;
  ret

let read_int i = match (input i) with
  | `Data d -> int_of_string d
  | _ -> failwith "expected data element"

let read_string i = match (input i) with
  | `Data d -> d
  | _ -> failwith "expected data element"

    
(* TODO: cleanup these comments
 * if context is given, then we check both tg and tg^"Ref"
 * and if there is Ref, we get the item from the context.
 * MRI: that only works if the referenced item has been parsed first: current plan is parse with ref, then insert refs
 * We need to remember to use mk functions for term sharing
 * TODO in SANY, move the context element to be appended first in the tree (before, location, name, etc.)
 * TODO: After resolving refs, the data structure may be cyclic. No dereferencing for now?
 *)
let rec get_children_choice ?context:(con=None) i tgs_funs =
  let filter_by_name tag = List.filter (fun x ->
    let p = (fst x) in let name = snd (fst tag) in p name) tgs_funs in
  match (peek i) with 
  | `El_start tag -> (
    match (filter_by_name tag) with (* check if one of the names applies       *)
    | (_, f) :: _ ->              (* find the corresponding function         *)
      let child =  f i in
      child :: (get_children_choice ~context:con i tgs_funs)
    | [] -> []                    (* if no predicate matches, return nothing *)
  )
  | `El_end -> []
  | _ -> failwith ("Illegal XML element, expecting child nodes but got " ^ (formatSignal (peek i)))


(** this is a reduction of get_children_choice to 0 or 1 occurrences  *)
let get_optchild_choice ?context:(con=None) i tgs_funs =
  match get_children_choice ~context:con i tgs_funs with
  | (_x1::_x2::_xs) as l ->
    failwith ("We expected exactly one matching child in " ^ (formatSignal (peek i)) ^
		 " but got " ^ (string_of_int (List.length l)))
  | list -> list
  
(** this is a reduction of get_children_choice to exatly 1 occurrences  *)
let get_child_choice ?context:(con=None) i tgs_funs =
  match get_children_choice ~context:con i tgs_funs with
  | [x] -> x
  | l   -> failwith ("We expected exactly one matching child in " ^ (formatSignal (peek i)) ^
			" but got " ^ (string_of_int (List.length l)))
    
(** processes children (left to right) of the current node. return a list of children having the name tg and
   maps the function f on each of them. processing stops after the first child not named tg  
*)
let get_children ?context:(con=None) i tg f =
  get_children_choice ~context:con i [((=) tg,f)]    (* the guard is just equality to tg *)

(** this is a reduction of get_children to 0 or 1 occurrences  *)
let get_optchild ?context:(con=None) i tg f =
  match get_children_choice ~context:con i [((=) tg,f)] with
  | (_x1::_x2::_xs) as l ->
    failwith ("We expected exactly one matching child in " ^ (formatSignal (peek i)) ^
		 " but got " ^ (string_of_int (List.length l)))
  | list -> list

let get_child ?context:(con=None) i tg f =
  let chldn = get_children ~context:con i tg f in
  assert (List.length chldn = 1);
  List.hd chldn

(* expects a node named tg_par and returns the all the children with tag tg_chdren. 
   remark: tag is closed afterwards, cannot process any remaining children
  *)
let get_children_in ?context:(con=None) i tg_par tg_chdrn f =
  open_tag i tg_par;
  let ret = get_children ~context:con i tg_chdrn f in
  close_tag i tg_par;
  ret

let get_children_choice_in ?context:(con=None) i tg_par tgs_funs =
  get_data_in i tg_par (fun i -> get_children_choice ~context:con i tgs_funs)
    
let get_child_in ?context:(con=None) i tg_par tg_chd f =
  let chldn = get_children_in ~context:con i tg_par tg_chd f in
  assert (List.length chldn = 1);
  List.hd chldn

