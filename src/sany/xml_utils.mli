open Xmlm

(** this map represents the context, mapping UIDs to
    formal_param_or_module_or_op_decl_or_op_def_or_theorem_or_assume *)
module ContextMap : (Map.S with type key = int)

val formatSignal  : Xmlm.signal -> string
(**
   con : context
   i   : xml input stream
   tgs_funs : list of pairs string * fun

   processes children (left to right) of the current node. the list tgs_fun
   contains pairs of functions (p, f), where the p acts as a guard to apply f,
   i.e. if (p name) is true, we apply f to the child. processing stops when the
   first unrecognized name is encountered.

   Remark: get_children_choice itself only looks at the tags, they have to be
   consumed by the functions applied.
*)
val get_children_choice : ?context:('b ContextMap.t)option -> input -> (((string -> bool) * (input -> 'a)) list) -> 'a list

(** Works like get_optchildren_choice, but fails when more than 1 element would
    be processed *)
val get_optchild_choice : ?context:('b ContextMap.t)option -> input -> (((string -> bool) * (input -> 'a)) list) -> 'a list

(** Works like get_optchildren_choice, but fails when not exactly 1 element would be processed *)
val get_child_choice : ?context:('b ContextMap.t)option -> input -> (((string -> bool) * (input -> 'a)) list) -> 'a

(** Works like get_optchildren_choice, but accepts only a specific tag and hanlder function *)
val get_children : ?context:('b ContextMap.t)option -> input -> string -> (input -> 'a) -> 'a list

(** Works like get_optchild_choice, but accepts only a specific tag and hanlder function *)
val get_optchild : ?context:('b ContextMap.t)option -> input -> string -> (input -> 'a) -> 'a list

(** Works like get_child_choice, but accepts only a specific tag and hanlder function *)
val get_child : ?context:('b ContextMap.t)option -> input -> string -> (input -> 'a) -> 'a

(** function composition of get_children and get_data_in  *)
val get_children_in : ?context:('b ContextMap.t)option -> input -> string -> string -> (input -> 'a) -> 'a list

(** function composition of get_children_choice and get_data_in  *)
val get_children_choice_in : ?context:('b ContextMap.t)option -> input -> string -> ((string-> bool) * (input -> 'a)) list -> 'a list

(** function composition of get_child and get_data_in  *)
val get_child_in : ?context:('b ContextMap.t)option -> input -> string -> string -> (input -> 'a) -> 'a

(** expects a string naming the opening tag. fails if anything else encountered.  *)  
val open_tag : input -> string -> unit

(** expects a string naming the closing tag. fails if anything else encountered.  *)  
val close_tag : input -> string -> unit

(** processes a node given by the name and applies the passed function to the inner input  *)  
val get_data_in : input -> string -> (input -> 'a) -> 'a

(** processes an xml data entry and converts it to an int *)
val read_int : input -> int

(** processes an xml data entry and returns it directly *)
val read_string : input -> string

(** checks if a leaf node of the given name is present. if it is, then the leaf node is consumed. *)
val read_flag : input -> string -> bool
