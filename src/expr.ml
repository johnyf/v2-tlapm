(* Copyright (C) 2014 MSR-INRIA JC
 *
 * EXPRESSIONS 
 * 
 * 
 *)


(** The TLA+ builtin operators
JEK: carried over from v1.builtin.ml

 *)
type builtin =
    (* Sets *)
  | STRING | BOOLEAN | SUBSET | UNION | DOMAIN
  | Subseteq | Mem | Notmem | Setminus | Cap
  | Cup
    (* modal *)
  | Prime | StrongPrime | Leadsto | ENABLED | UNCHANGED | Cdot
  | Actplus | Box | Diamond
    (* arithmetic *)
  | Nat | Int | Real | Plus | Minus | Uminus
  | Times | Ratio | Quotient | Remainder | Exp
  | Infinity | Lteq | Lt | Gteq | Gt | Divides
  | Range
    (* sequences *)
  | Seq | Len | BSeq | Cat | Append | Head
  | Tail | SubSeq | SelectSeq
    (* tlc *)
  | OneArg | Extend | Print | PrintT | Assert
  | JavaTime | TLCGet | TLCSet | Permutations
  | SortSeq | RandomElement | Any | ToString
    (* special *)
  | Unprimable | Irregular



type varID = string (* for now... *)


(* an expression is: *)
type expr = 
(* variables, builtins, tuples, products and generic apply-construct (e.g. for operators and builtins *)*)
  | Var     of varID 
  | Builtin of builtin
  | Tuple   of expr list  (*JEK: maybe for the purpose of quantifying over tuples, there should be a special Tuple of varIDs ? *)
  | Product of expr list
  | Apply  of expr * expr list

(* propositional connectives + (non-)equality + if-then-else + case *) 
  | TRUE 
  | FALSE 
  | Neg     of expr
  | Implies of expr * expr
  | Equiv   of expr * expr
  | Conj    of expr list
  | Disj    of expr list
  | Eq      of expr * expr
  | Neq     of expr * expr
  | ITE     of expr * expr * expr 
  | Case    of (expr * expr) list * expr option

(* binders: lambda and quantifiers:  *)
  | Lambda of varID list * expr 
  | Exists of bound list * expr
  | Forall of bound list * expr
  | Choose of bound * expr
  | TempExists of varID list * expr 
  | TempForall of varID list * expr 


(* sets: 
  comprehension        - e.g. { x (\in S) | P(x)}, 
  separation           - e.g. {e(x) | x (\in S)}, 
  explicit enumeration - e.g. {e1, e2, e3} *)
  | SetComp of bound * expr
  | SetSep  of expr * bound
  | SetEnum of expr list

(* functions and records: 
  function definition  - e.g. [<<x>> (\in S) |-> e],
  function application - e.g. f[e1, e2, e3], 
  type-definition      - e.g. [S -> T],
  records              - e.g. [x |-> e1, y |-> e2],
  dot                  - e.g. e.f
  except for functions - e.g. [f EXCEPT ![e1] = e2, ![e3] = e4],
  except for records   - e.g. [r EXCEPT !["x"] = e1, !["y"] = e2] *)
  | FunDef of bound list * expr
  | FunApp of expr * expr list
  | FunTyp of expr * expr
  | Record of (string * expr) list
  | Dot    of expr * string
  | RecordTyp of (string * expr) list
  | FunExcept of expr * (expr * expr ) list  (* JEK: see email => brainstorm *)
  | RecExcept of expr * (string * expr) list


(* definitions: operators, recursive defns, instances  *)
  | LetOp   of string * expr list * expr 
  | LetRec  of string * expr list * expr
  | LetInst of string * 

(* modal and fairness expressions *)
  | BoxSub of expr * expr             (* e.g. [A]_e *)
  | DiaSub of expr * expr             (* e.g. <A>_e *)
(* JEK: e_t.ml in v1 has a constructor temporal Sub here - what is that for? *)
  | WeakFair   of expr * expr
  | StrongFair of expr * expr


(* JEK: Questions:

  Constructors in v1 that I don't know what they represent:
   - Num of string * string
   - At of book

  I'm not sure how to go about Assume-Proofs, what was sequents in v1...
=> brainstorm
 *)


and bound = 
  | VarBound of varID * var_bound_domain option
  | TupBound of Tuple * tup_bound_domain option (* JEK: this does not check that the tuple and the product 
                                   have the same arity: does Sany enforce that? is this an issue here? *)
and var_bound_domain =
  | VarDomain of expr
  | Ditto
and tup_bound_domain =
  | TupDomain of Product
  | Ditto 

