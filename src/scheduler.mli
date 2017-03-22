(** The scheduler processing the obligations. Right now just serializes
    every call. *)

val scheduler : Settings.settings ->
  Obligation.obligation list -> Toolbox.toolbox_msg Util.IntMap.t
