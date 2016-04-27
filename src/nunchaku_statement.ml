open Format

type statement =
  | Declaration of string * string
  | Axiom of string
  | Goal of string
  | Comm of string
  | Include of string
  | Else of string

let print_statement ppf sta = match sta with
  | Declaration (n,t) -> fprintf ppf "val %s : %s. @." n t
  | Axiom s -> fprintf ppf "axiom : %s. @." s
  | Goal s -> fprintf ppf "goal : %s. @." s
  | Comm s -> fprintf ppf "# %s @." s
  | Include s -> fprintf ppf "include \"%s\". @." s
  | Else s -> fprintf ppf "%s @." s
