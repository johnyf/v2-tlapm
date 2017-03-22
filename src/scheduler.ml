open Commons
open Util
open Obligation
open Toolbox

let schedule settings context o =
  List.fold_left (fun l -> function
      | Isabelle -> (o, Isabelle, fun () ->
          (* debug "starting isabelle\n"; *)
          Runners.run_isabelle settings o) :: l
      | Nunchaku -> (o, Nunchaku, fun () ->
          match Runners.run_nunchaku settings o with
          | Some msg -> Some ToBeProved
          | None -> Some Failed
        ) :: l
      | Zenon
      | SMT
      | LS4
      | Tlaps
      | Default -> (o, Default, fun () -> None) :: l
    ) context o.provers

let scheduler settings obligations =
  let default_msg (o:obligation) prover =
    { id = o.id;
      location = o.location;
      status = Failed;
      prover = Some prover;
      meth   = None;
      already_processed = Some false;
      obligation_string = None
    } in
  let tasks = List.fold_left (schedule settings) [] obligations in
  let messages = List.fold_left (fun acc -> function
      | ((o:obligation), prover, task) ->
        let entry = IntMap.get_or o.id acc ~default:(default_msg o prover) in
        match entry with
        | {status = Proved; _ } ->
          (* status is already proved, we don't need to call a backend *)
          acc
        | _ ->
          (* call prover and generate new message *)
          begin
            match task () with
            | Some status ->
              let entry = {
                id = o.id;
                location = o.location;
                status;
                prover = Some prover;
                meth = None;
                already_processed = Some false;
                obligation_string = None
              } in
              IntMap.add o.id entry acc
            | None ->
              (* backend error, ignore *)
              acc
          end
    ) IntMap.empty tasks in
  messages
