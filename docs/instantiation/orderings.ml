type term = | Var of string
            | App of term * term
            | Abs of string * term
            | Chunk of string * int * int

let var s = Var s
let app s t = App (s, t)
let abs x t = Abs (x, t)
let chunk s d m = Chunk (s, d, m)

let rec pp_term_inner fmt = function
  | Var s ->
    Format.fprintf fmt "%s" s
  | Chunk (n,d,m) ->
    Format.fprintf fmt "(chunk %s %d %d)" n d m
  | App (App(r,s), t) ->
    Format.fprintf fmt "(%a %a)" pp_term r pp_term_inner (app s t)
  | App (s, t) ->
    Format.fprintf fmt "(%a %a)" pp_term_inner s pp_term_inner t
  | Abs (x, Abs(y, t)) ->
    Format.fprintf fmt "(λ%s %a)" x pp_term (abs y t)
  | Abs (x, t) ->
    Format.fprintf fmt "(λ%s %a)" x pp_term_inner t
and pp_term fmt = function
  | Var s ->
    Format.fprintf fmt "%s" s
  | Chunk (n,d,m) ->
    Format.fprintf fmt "(chunk %s %d %d)" n d m
  | App (App(r,s), t) ->
    Format.fprintf fmt "%a %a" pp_term r pp_term_inner (app s t)
  | App (s, t) ->
    Format.fprintf fmt "%a %a" pp_term_inner s pp_term_inner t
  | Abs (x, Abs(y, t)) ->
    Format.fprintf fmt "λ%s %a" x pp_term (abs y t)
  | Abs (x, t) ->
    Format.fprintf fmt "λ%s %a" x pp_term_inner t


let max s t = if s > t then s else t

let rec d = function
  | Chunk (_, d, m) -> d
  | Var _ -> 0
  | App (s, t) -> 1 + (max (d s) (d t))
  | Abs (x, t) -> 1 + (d t)

let rec m =
  let combine s t =
    max (m s) (m t) in
  function
  | Chunk (_, d, m) -> m
  | Var _ -> 0
  | App (Abs (x, s), t) ->
    1 + (d s) + (combine (abs x s) t)
  | App (s, t) ->
    combine s t
  | Abs (_, t) ->
    m t

let comp t1 t2 = (m t1) < (m t2)


module TT = struct
  let f = var "f"
  let g = var "g"
  let x = var "x"
  let e = var "e"

  let t1 f g e = app (abs "x" (app f g)) e
  let t2 f g e = app (app (abs "x" f) e) (app (abs "x" g) e)

  let t3 s t = app (abs "x" (abs "y" s)) t
  let t4 s t = abs "y" (app (abs "x" s) t)

  let t5 s t e = app (app (abs "x" (abs "y" s)) t) e
  let t6 s t e = app (abs "y" (app (abs "x" s) t)) e

  let range n =
    let rec aux = function
      | n when n <= 0 -> []
      | n -> n :: (aux (n-1))
    in
    List.rev (aux n)


  let chunks name d m =
    List.map (fun x -> List.map (fun y ->
        if y<=x then [chunk name x y] else [] ) (range m)) (range d)
    |> List.concat |> List.concat

  let test1 f g e =
    let a = t1 f g e in
    let b = t2 f g e in
    (a, b, comp b a)

  let test2 s t =
    let a = t3 s t in
    let b = t4 s t in
    (a, b, comp b a)

  let test3 s t e =
    let a = t5 s t e in
    let b = t6 s t e in
    (a, b, comp b a)

  let line1 d m =
    let es = chunks "e" d m in
    let fs = chunks "f" d m in
    let gs = chunks "g" d m in
    List.map (fun e ->
        List.map (fun f ->
            List.map (fun g ->
                test1 f g e
              ) gs) fs) es
    |> List.concat |> List.concat

  let line2 d m =
    let ss = chunks "s" d m in
    let ts = chunks "t" d m in
    List.map (fun s ->
        List.map (fun t ->
            test2 s t
          ) ts) ss
    |> List.concat

  let line3 d m =
    let ss = chunks "s" d m in
    let ts = chunks "t" d m in
    let es = chunks "e" d m in
    List.map (fun e ->
        List.map (fun t ->
            List.map (fun s ->
                test3 s t e
              ) ss) ts) es
    |> List.concat |> List.concat


  let tests a b =
    ignore(
      List.concat [(line1 a b); (line2 a b); (line3 a b)] |>
      List.filter (function | (_,_,false) -> true | _ -> false) |>
      List.map (function | (t1, t2, _) ->
          Format.printf "@[%a fails %a: %d < %d@]@,"
            pp_term t1 pp_term t2 (m t1) (m t2))) ;
    Format.printf "@."
end
;;

TT.tests 2 1
