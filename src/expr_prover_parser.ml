open Commons
open Expr_ds
open Expr_dereference
open List

let match_function term_db = function
  | E_op_appl appl  ->
     (
     match appl.operator, appl.operands with
     | FMOTA_op_def opd, args ->
        (
        match dereference_op_def term_db opd with
        | O_user_defined_op uop ->
           let uopi = dereference_user_defined_op term_db uop in
           Some (uopi.name, args)
        | O_builtin_op bop ->
           Some (bop.name, args)
        | _ ->  None
        )
     | _ ->  None
     )
  | _ -> None

let match_constant term_db expr =
  match match_function term_db expr with
  | Some (name, []) -> Some name
  | _ -> None

let expr_to_prover term_db expr =
  match match_constant term_db expr with
  | Some "SMT" -> Some SMT
  | Some "LS4" -> Some LS4
  | Some "Isa" -> Some Isabelle
  | Some "Zenon" -> Some Zenon
  | _ -> None

(* TODO: check if the sany parser does not change names *)
let infix_names =
  [
  "!!"; "#"; "##"; "$"; "$$"; "%"; "%%";
  "&"; "&&"; "(+)"; "(-)"; "(.)"; "(/)"; "(\\X)";
  "*"; "**"; "+"; "++"; "-"; "-+->"; "--";
  "-|"; ".."; "..."; "/"; "//"; "/="; "/";
  "::="; ":="; ":>"; "<"; "<:"; "<=>"; "=";
  "=<"; "=>"; "=|"; ">"; ">="; "?";
  "??"; "@@"; ""; "\\/"; "^"; "^^"; "|"; "|-";
  "|="; "||"; "~>"; ".";
  "\\approx"; "\\geq"; "\\oslash"; "\\sqsupseteq";
  "\\asymp"; "\\gg"; "\\otimes"; "\\star"; "\\bigcirc";
  "\\in"; "\\prec"; "\\subset"; "\\bullet"; "\\intersect";
  "\\preceq"; "\\subseteq"; "\\cap"; "\\land"; "\\propto";
  "\\succ"; "\\cdot"; "\\leq"; "\\sim"; "\\succeq"; "\\circ";
  "\\ll"; "\\simeq"; "\\supset"; "\\cong"; "\\lor"; "\\sqcap";
  "\\supseteq"; "\\cup"; "\\o"; "\\sqcup"; "\\union"; "\\div";
  "\\odot"; "\\sqsubset"; "\\uplus"; "\\doteq"; "\\ominus";
  "\\sqsubseteq"; "\\wr"; "\\equiv"; "\\oplus"; "\\sqsupset"
  ]

(* TODO: check if the sany parser does not change names *)
let prefix_names =
  [ "-"; "~"; "\\lnot"; "\\neg"; "[ ]"; "\\< >";
    "DOMAIN" ; "ENABLED" ; "SUBSET" ; "UNCHANGED" ; "UNION";
  ]

let ternary_names =
  [  "$IfThenElse"  ]

let expand_ternary_name = function
  | "$IfThenElse" -> "IF", "THEN", "ELSE"
  | s -> failwith ("Don't know how to expand infix ternary operator " ^ s )

let extract_binary_args arity name params =
  if (arity = 2) && (mem name infix_names)
  then true else false

let extract_ternary_args arity name params =
  if (arity = 3) && (mem name ternary_names)
  then Some name else None

let match_infix_op term_db = function
  | FMOTA_formal_param fp -> false
  | FMOTA_module m -> false
  | FMOTA_op_def opdef -> (
     match dereference_op_def term_db opdef with
     | O_user_defined_op uop ->
        let uopi = dereference_user_defined_op term_db uop in
        extract_binary_args uopi.arity uopi.name uopi.params
     | O_builtin_op op ->
        extract_binary_args op.arity op.name op.params
     | _ ->
        false
  )
  | FMOTA_op_decl opdecl -> false
  | FMOTA_theorem thm -> false
  | FMOTA_assume assume -> false
  | FMOTA_ap_subst_in _ -> false

let match_ternary_op term_db = function
  | FMOTA_formal_param fp -> None
  | FMOTA_module m -> None
  | FMOTA_op_def opdef -> (
     match dereference_op_def term_db opdef with
     | O_user_defined_op uop ->
        let uopi = dereference_user_defined_op term_db uop in
        extract_ternary_args uopi.arity uopi.name uopi.params
     | O_builtin_op op ->
        extract_ternary_args op.arity op.name op.params
     | _ -> None
  )
  | FMOTA_op_decl opdecl -> None
  | FMOTA_theorem thm -> None
  | FMOTA_assume assume -> None
  | FMOTA_ap_subst_in _ -> None
