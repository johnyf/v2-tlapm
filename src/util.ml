(*
 * util.ml --- format utilities
 *
 *
 * Copyright (C) 2008-2010  INRIA and Microsoft Corporation
 *)
open Ext
open Format

type 'a fmt = formatter -> 'a -> unit

(* FIXME remove, replace with assert false *)
exception Bug

(* FIXME remove, replace with assert false *)
let bug ?at msg =
  let bugmsg =
    let lines = [
      "Oops! This appears to be a bug in the TLA+ Proof Manager." ;
      "" ;
      "Please file a bug report, including as much information" ;
      "as you can. Mention the following in your report: " ;
      "" ]
    (* @ Params.configuration false false *)
    in
    String.concat "\n" lines ^ "\n"
  in
  (*Backend.Toolbox.print_message "error" msg ^"\n"^bugmsg;*)
  (* Format.eprintf ?at ~prefix:"BUG: " "%s\n%s%!" msg bugmsg ; *)
  Format.eprintf "BUG: %s\n%s%!" msg bugmsg ; (* TODO: print location *)
  raise Bug

type csum = int * int

let checksum f =
  let scmd = Printf.sprintf "sum %s" f in
  let sin = Unix.open_process_in scmd in
  let hash = Std.input_all sin in
  ignore (Unix.close_process_in sin) ;
  Scanf.sscanf hash "%d %d" (fun j k -> (j, k))

let pp_print_csum ff (j, k) =
  Format.fprintf ff "<%d-%d>" j k

let plural ?(ending="s") count s =
  if count <> 1 then s ^ ending else s

let line_wrap ?(cols=70) s =
  let slen = String.length s in
  let buf = Buffer.create slen in
  let rec next_space x =
    if x >= slen then slen
    else if s.[x] = ' ' then x
    else next_space (x + 1)
  in
  let rec drop_spaces x =
    if x >= slen then invalid_arg "drop_spaces"
    else if s.[x] = ' ' then drop_spaces (x + 1)
    else x
  in
  let rec spin x ccol =
    if x >= slen then () else
      let nx = next_space (x + 1) in
      if nx - x + ccol >= cols then begin
        Buffer.add_string buf "\n" ;
        spin (drop_spaces x) 0
      end else begin
        Buffer.add_substring buf s x (nx - x) ;
        spin nx (ccol + nx - x)
      end
  in
  spin 0 0 ;
  Buffer.contents buf

let heap_stats () =
  Gc.full_major () ;
  let st = Gc.stat () in
  Format.eprintf "@.@.>>>>> HEAP STATS: live_words = %d <<<<<<@.@." st.Gc.live_words

let add_hook h f x =
  let old = !h in
  h := (fun () -> f x; old ());
;;

(* TODO: depends on Params, change dependency to new param handling
   let rm_temp_file fname =
   if not (Params.debugging "tempfiles") then begin
   try Sys.remove fname with _ -> ()
   end;
   ;;

   let temp_file (clean_hook : (unit -> unit) ref) suffix =
   let (fname, chan) =
   Filename.open_temp_file ~temp_dir:!Params.output_dir ~mode:[Open_binary]
                          "tlapm_" suffix
   in
   let f () =
   close_out chan;
   rm_temp_file fname;
   in
   add_hook clean_hook f ();
   (fname, chan)
   ;;
*)

(* some general formatting utils *)
let fmt_string = pp_print_string

let fmt_pair ?front:(f="(") ?middle:(m=", ") ?back:(b=")")
    fmt_left fmt_right formatter (x,y) =
  fprintf formatter "%s%a%s%a%s" f fmt_left x m fmt_right y b;
  ()

let fmt_option ?none:(none="None") ?some:(some="Some ") ?some_back:(back="")
    fmt_arg formatter =
  function
  | Some s -> fprintf formatter "%s%a%s" some fmt_arg s back
  | None -> fprintf formatter "%s" none


let fmt_list ?front:(f="[") ?middle:(m="; ") ?back:(b="]") fmt formatter lst =
  let rec fmt_list_ ?middle:(m=";") formatter = function
    | [] ->
      ()
    | [x] ->
      fprintf formatter "%a" fmt x
    | x::xs ->
      fprintf formatter "%a%s%a" fmt x m (fmt_list_ ~middle:m) xs;
      ()
  in
  fprintf formatter "%s%a%s" f (fmt_list_ ~middle:m) lst b;
  ()


let mkString ?front:(f="[") ?middle:(m="; ") ?back:(b="]") fmt lst =
  let fmt_element formatter x = fprintf formatter "%s" (fmt x) in
  asprintf "%a" (fmt_list ~front:f ~middle:m ~back:b fmt_element) lst

(* right-associative function application just like Haskell's $ *)
let (  @$ ) f x = f x;;


(* given a list of key * dependeny pairs, create a list of key * dependency
   list, where all dependencies of a key is contained in the dependency list  *)
let rec collect_dependencies  keys_dependencies = function
  | (x,y)::xs -> (
      (* look at the first key-dependency pair, check if the key has already
         dependencies and add the dependency to the list, if necessary *)
      match List.partition (fun kd -> x = fst kd) keys_dependencies with
      | ([], rest) ->
        (* if key does not have a list associated, make a new entry *)
        collect_dependencies  ((x,[y]) :: rest) xs
      | ([(_, dependencies)], rest) when List.mem y dependencies ->
        (* y is already in the dependencies, don't add it twice *)
        collect_dependencies  keys_dependencies xs
      | ([(_, dependencies)], rest) (* when not List.mem y dependencies *) ->
        collect_dependencies  ((x, y::dependencies) :: rest) xs
      | _ ->
        failwith "Implementation error in finding an ordering!"
    )
  | [] -> keys_dependencies

(** appends list2 to list without creating duplicates. does not remove
    duplicates from list. *)
let rec add_missing list = function
  | x::xs when List.mem x list -> add_missing list xs
  | x::xs (* otherwise *)      -> add_missing (List.append list[x]) xs
  | _ -> list


let rec flat_map f l = List.flatten ( List.map f l)


(** removes all passed elements from the dependency list of each entry in the
    completion list *)
let remove_from_completion elements completion =
  List.map (fun (key,deps) ->
      (key, List.filter (fun x -> not (List.mem x elements)) deps)
    ) completion

let rec find_ordering_from_completion  completion = (
  (* let collect_keys =
     List.fold_left (fun list (key,deps) -> add_missing list [key]) [] in *)
  let collect_deps =
    List.fold_left (fun list (key,deps) -> add_missing list deps) [] in
  let collect_all  =
    List.fold_left (fun list (key,deps) -> add_missing list (key::deps)) [] in
  let all_in_nodes = collect_deps completion in
  match List.partition
          (fun (key,_) -> not (List.mem key all_in_nodes) ) completion with
  | ([], []) -> []
  | ([], _) ->
    failwith ("Could not find a least element to in the given list. "^
              "Could not create an ordering.")
  | (least_elements, rest ) ->
    let reduced_rest =
      remove_from_completion (fst @$ List.split least_elements) rest in
    let keys = List.map fst least_elements in
    let all_rest = collect_all reduced_rest in
    let all_least_dependencies = collect_deps least_elements in
    let single_elements =
      List.filter (fun x -> not (List.mem x all_rest)) all_least_dependencies in
    let keys_single = add_missing keys single_elements in
    List.append keys_single (find_ordering_from_completion reduced_rest)
)

let find_ordering  constraints =
  let completion  = collect_dependencies  [] constraints in
  find_ordering_from_completion completion

let multiset_equal_lists l1 l2 =
  let s1 = List.sort l1 in
  let s2 = List.sort l2 in
  s1 = s2

let autodetect_executable_path =
  let bin_string = Array.get Sys.argv 0 in
  let lib_path = Str.global_replace (Str.regexp "/[^/]*$") "" bin_string in
  lib_path

(* Maps for some types *)
module IntOrdering =
  struct
    type t = int
    let compare : t->t->t = Pervasives.compare
  end

module IntMap = CCMap.Make(IntOrdering)
module IntSet = CCSet.Make(IntOrdering)

module StringMap = CCMap.Make(String)
module StringSet = CCSet.Make(String)
