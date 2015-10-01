open Commons
open Expr_ds
open Expr_map

type 'a ptacc = term_db option * 'a

class ['a] expr_parse_theorems : object
inherit ['a ptacc] expr_map
end
