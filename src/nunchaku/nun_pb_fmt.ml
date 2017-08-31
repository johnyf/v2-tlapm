open Commons
open Simple_expr_ds
open Simple_expr_visitor
open Simple_expr_utils
open Simple_expr_dereference
open Format
open Nun_pb
open Tla_simple_pb
open Backend_exceptions

(* tools for encoding to nunchaku *)
module Encode = struct
  (* universal type *)
  let u_type = var "u"

  (* name mappings from tla to nunchaku syntax*)

  let user name = "user__" ^ name
  let numeral = function
    | n when n >= 0 -> "numeral__pos_" ^ (string_of_int n)
    | n (* n < 0 *) -> "numeral__neg_" ^ (string_of_int (-n))

end

type name_type = Numeral of int | Builtin of string | User of string
type name_translation = (name_type * string) list

type fc = statement list * simple_term_db * term * bool * name_translation

(* these are extractors for the accumulator type  *)
let sta     ( sta, _, _, _, _) = sta (* statement list *)
let tdb     ( _, tdb, _, _, _) = tdb (* term database *)
let ter     ( _, _, ter, _, _) = ter (* term accumulator *)
let top     ( _, _, _, top, _) = top (* flag : is_top_level *)
let ntr     ( _, _, _, _, ntr) = ntr (* tla to nunchaku name mapping *)

let add_axiom l (sta, tdb, ter, top, ntr) =
  let x = axiom l in
  let sta' = if List.mem x sta then sta else (x::sta) in
  (sta', tdb, ter, top, ntr)

let add_decl v t (sta, tdb, ter, top, ntr) =
  let x = decl [] v t in
  let sta' = if List.mem x sta then sta else (x::sta) in
  (sta', tdb, ter, top, ntr)

let add_comm s (sta, tdb, ter, top, ntr) =
  let x = comm s in
  (x::sta, tdb, ter, top, ntr)

let add_include f (sta, tdb, ter, top, ntr) =
  let x = include_ f in
  (x::sta, tdb, ter, top, ntr)

let add_goal t (sta, tdb, ter, top, ntr) =
  let x = goal ( App ((Builtin `Not),[t])) in
  (x::sta, tdb, ter, top, ntr)

let fmt_name_type fmt = function
  | Numeral n -> Format.fprintf fmt "Numeral %d" n
  | User s    -> Format.fprintf fmt "User %s" s
  | Builtin s -> Format.fprintf fmt "Builtin %s" s

let add_ntr name mapped (sta, tdb, ter, top, ntr) =
  try
    match List.assoc name ntr with
    | old when old = mapped  ->
      (sta, tdb, ter, top, ntr)
    | old (* old != mapped *) ->
      let msg = Format.asprintf "Name mapping %a -> %s conflicts with %a -> %s@."
          fmt_name_type name old fmt_name_type name mapped
      in failwith msg
  with
  | Not_found ->
    (sta, tdb, ter, top, (name, mapped) :: ntr)

let set_term ter (sta, tdb, _, top, ntr) = (sta, tdb, ter, top, ntr)

let un_top (sta, tdb, ter, _, ntr) = (sta, tdb, ter, false, ntr)

let acc_fold function_to_apply list_of_args acc =
  List.fold_right (fun arg_temp acc_temp ->
      function_to_apply acc_temp arg_temp) list_of_args acc

let acc_fold_plus function_to_apply list_of_args acc =
  List.fold_right (fun arg_temp -> fun (acc_temp,list_temp) ->
      let acc_temp' = function_to_apply acc_temp arg_temp in
      (acc_temp',(ter acc_temp')::list_temp)
    ) list_of_args (acc,[])

let is_unsupported ({ name; location = {line; column; filename }; arity; _ }
                    : simple_user_defined_op_) =
  match filename with
  | "Naturals" -> true
  | _ -> false

let add_numeral_axioms (sta, tdb, ter, top, ntr) : fc =
  let numerals = Util.flat_map (function
      | (Numeral n, name) -> [var name]
      | _ -> []
    ) ntr
  in
  let rec create_inequalities acc = function
    | [] -> acc
    | x::xs ->
      let ineq y = App (builtin `Neq, [x; y]) in
      let combines = List.map ineq xs in
      let acc0 = List.append acc combines in
      create_inequalities acc0 xs
  in
  match create_inequalities [] numerals with
  | [] ->
    ( sta, tdb, ter, top, ntr)
  | xs ->
    ( sta, tdb, ter, top, ntr) |> add_axiom xs

class formatter =
  object(self)
    inherit [fc] visitor as super

    (* parts of expressions *)
    method location acc { column; line; filename } : 'a =
      add_comm "Crossed location" acc

    method level acc l : 'a =
      add_comm "Crossed level" acc

    (* non-recursive expressions *)
    method decimal acc0 { location; level; mantissa; exponent;  } =
      let value =
        (float_of_int mantissa) /. ( 10.0 ** (float_of_int exponent)) in
      let acc1 = set_term (Var (`Var (string_of_float value))) acc0 in
      (* add_comm ("Crossed decimal = "^(string_of_float value)) acc1 in *)
      acc1

    method numeral acc0 {location; level; value } =
      let name = Encode.numeral value in
      let acc1 = set_term (Var (`Var name)) acc0 in
      (* add_comm ("Crossed numeral = "^(string_of_int value)) acc1 *)
      (* TODO: add axioms for numerals (1 # 2) / add numerals type *)
      acc1 |> add_decl name Encode.u_type |> add_ntr (Numeral value) name

    method strng acc {location; level; value} =
      add_comm ("Crossed string = "^value) acc

    method op_arg acc {location; level; argument } =
      self#operator acc argument

    (* recursive expressions *)
    method at acc {location; level; except; except_component} =
      add_comm "Crossed AT" acc

    method op_appl acc0 {location; level; operator; operands} =
      let acc1 = self#operator acc0 operator in

      let (acc2,t2) = List.fold_right
          (fun x -> fun (a,t) ->
             let a' = self#expr_or_op_arg a x in
             (a',(ter a')::t))
          operands (acc1,[]) in
      let term = match ter acc1 with
        | SetEnum [] -> SetEnum t2
        | _ -> App ((ter acc1),t2)
      in
      set_term term acc2


    method lambda acc {level; arity; body; params} =
      add_comm "Crossed LAMBDA" acc

    method binder acc b =
      match b.bound_symbols with
      | []     -> let acc1 = self#operator acc b.operator in
        self#expr_or_op_arg acc1 b.operand
      | hd::tl -> let b2 = {location = b.location; level = b.level;
                            operator = b.operator; operand = b.operand;
                            bound_symbols = tl} in
        let acc0 = self#binder acc b2 in
        let t = ter acc0 in
        let acc1 = self#operator acc0 b.operator in
        let kind = ter acc1 in
        let acc2 = self#bound_symbol acc1 hd in
        let v = match ter acc2 with
          | Var (`Var x) -> x
          | _ -> failwith "variable identification"
        in
        let v' = (v,Some Encode.u_type) in
        let acc3,set =
          match hd with
          | B_unbounded_bound_symbol _ -> acc2,None
          | B_bounded_bound_symbol bbs ->
            let acctemp = self#expr acc2 (bbs.domain) in
            acctemp,(Some (ter acctemp))
        in
        match kind with
        | Builtin `Forall -> set_term (Forall (v',set,t)) acc3
        | Builtin `Exists -> set_term (Exists (v',set,t)) acc3
        | _ -> failwith "binder not a quantifier"

    method bounded_bound_symbol acc { params; tuple; domain; } =
      match params with
      | [] ->
        failwith "Trying to process empty tuple of bound symbols with domain!"
      | [param] ->
        (* let acc1 = match tuple with *)
        (* 	 | true -> add_to_string "<<" acc *)
        (* 	 | false -> acc *)
        (* in *)
        (* let acc2 = self#formal_param acc param in *)

        (* let acc3 = match tuple with *)
        (* 	 | true -> add_to_string "<<" acc2 *)
        (* 	 | false -> acc2 *)
        (* in *)
        (* let acc4 = add_to_string " \\in " acc3 in *)
        (* let acc5 = self#expr acc4 domain in *)
        self#formal_param acc param

      | _ ->
        (* fprintf (ppf acc) "<<"; *)
        (* let acc1 = ppf_fold_with self#formal_param acc params in *)
        (* fprintf (ppf acc1) ">> \\in "; *)
        (* let acc2 = self#expr acc domain in *)
        acc_fold self#formal_param params acc

    method unbounded_bound_symbol acc { param; tuple } =
      (* if tuple then fprintf (ppf acc) "<<"; *)
      (* let acc1 = self#formal_param acc param in *)
      (* if tuple then fprintf (ppf acc1) ">> "; *)
      self#formal_param acc param

    method formal_param acc fp =
      let { location; level; name; arity; } : simple_formal_param_ =
        dereference_formal_param (tdb acc) fp in
      set_term (var name) acc

    method op_decl acc0 opdec =
      let { location ; level ; name ; arity ; kind ; } =
        dereference_op_decl (tdb acc0) opdec in
      let acc1 = add_decl name Encode.u_type acc0 in
      let mapped_name = Encode.user name in
      acc1 |> set_term (var name) |> add_ntr (User mapped_name) name

    method op_def acc = function
      | O_builtin_op x      ->
        self#builtin_op acc x
      | O_user_defined_op x ->
        let op = dereference_user_defined_op (tdb acc) x
        in
        match op.recursive with
        | true  ->
          raise (UnhandledLanguageElement
                   (Nunchaku, "Cannot deal with recursive user defined ops" ))
        | false -> let rec print_type n = match n with
            | 0 -> "u"
            | _ -> "u -> "^(print_type (n-1))
          in
          let acc1 = add_decl op.name (var (print_type op.arity)) acc in
          let acc2 = self#expr acc1 op.body in
          let acc3 = add_axiom [App (Builtin `Eq,[var op.name;(ter acc2)])] acc2 in
          if is_unsupported op then
            raise (UnhandledLanguageElement
                     (Nunchaku, "Unsupported builtin "^op.name));
          (* TODO add rec if parameters *)
          match op.arity with
          | 0 ->
            let mapped_name = Encode.user op.name
            in acc3
               |> set_term (var mapped_name)
               |> add_ntr (User op.name) mapped_name
          | _ ->
            let msg = Format.asprintf
                "Cannot handle user defined ops (%s) with arguments.@."
                op.name
            in
            raise (UnhandledLanguageElement (Nunchaku, msg))

    method assume_prove acc { location; level; new_symbols; assumes;
                              prove; } =
      let acc0 = un_top acc in
      (* declares new symbols with axioms for domain *)
      let acc1 = acc_fold self#new_symb new_symbols acc0 in
      (* returns a list of assumes *)
      let acc2,l = acc_fold_plus self#assume_prove assumes acc1 in
      let acc3 = self#expr acc2 prove in
      let prove_expr = ter acc3 in
      let my_expr =
        match l with
        | [ ] -> prove_expr
        | [x] -> App (Builtin `Imply,[x;prove_expr])
        |  _  -> let assume_expr = App (Builtin `And,l) in
          App (Builtin `Imply,[assume_expr;prove_expr])
      in
      let acc4 = match top acc with
        | true -> add_goal my_expr acc3        (* if top level *)
        | false -> set_term my_expr acc3        (* else *)
      in
      acc4

    method new_symb acc0 { location; level; op_decl; set } =
      let od = dereference_op_decl (tdb acc0) op_decl in
      let new_decl = match od.kind with
        | NewConstant -> Encode.u_type (* default is constant "CONSTANT " *)
        | _ -> failwith "declared new symbol with a non-constant kind."
      in
      let acc1 = add_decl od.name new_decl acc0 in
      let acc2 = match set with
        | None -> acc1
        | Some e -> let acc' = self#expr acc1 e in
          add_axiom [(App (Builtin `Mem,[Var (`Var od.name);(ter acc')]))] acc'
      in
      acc2

    method let_in acc0 {location; level; body; op_defs } =
      raise (UnhandledLanguageElement (Nunchaku, "let in"))

    method label acc0 ({location; level; name; arity; body; params } : simple_label) =
      add_comm "LABEL" acc0

    method builtin_op acc { level; name; arity; params } =
      let term =
        match name with
        | "$SetEnumerate" -> SetEnum []
        | _ -> (builtin (Simple_expr.translate_builtin_name name))
      in
      set_term term acc

    method user_defined_op acc0 op =
      acc0

    method expr acc x =
      let acc1 = match x with
        | E_binder  b -> self#binder  acc b
        | E_op_appl b -> self#op_appl acc b
        | E_numeral b -> self#numeral acc b
        | _ -> add_comm "Crossed unknown expression" acc
      in acc1

    method name acc x = acc

    method reference acc x = acc

  end


let expr_formatter = new formatter

let mk_fmt (f : fc -> 'a -> fc) term_db (goal : 'a) =
  let initial_commands = [
    comm "Initial commands";
    include_ "\"prelude.nun\"";
    comm "End of initial commands";
  ] in
  let acc =
    (List.rev initial_commands, term_db, Unknown "starting term", true, [])
  in acc
     |> (fun acc -> f  acc goal)
     |> add_numeral_axioms
     |> function
     | (l,_,_,_,_) ->
       l

let fmt_expr = mk_fmt (expr_formatter#expr)

let fmt_assume_prove = mk_fmt (expr_formatter#assume_prove)

let tla_simple_pb_to_nun_ast { goal; term_db; } =
  List.rev (fmt_assume_prove term_db goal)
