let rec parse = function
  | [] -> []
  | a::b::p::([] as xs)
  | a::b::p::""::xs ->
    let open Scanf in
    sscanf a "Button A: X+%d, Y+%d" @@ fun ax ay ->
    sscanf b "Button B: X+%d, Y+%d" @@ fun bx by ->
    sscanf p "Prize: X=%d, Y=%d" @@ fun px py ->
    ((ax, ay), (bx, by), (px, py)) :: parse xs
  | _ -> assert false

let rec gcd n m =
  if m = 0 then n
  else gcd m (n mod m)

let solve ((ax, ay), (bx, by), (px, py)) =
  let k = gcd bx by in
  let xm = by / k and ym = bx / k in
  let a = float (px*xm - py*ym) /.
          float (ax*xm - ay*ym) in
  if a <> floor a then None
  else
    let a = truncate a in
    let b = (px - ax*a) / bx in
    if ay*a + by*b = py then
      Some (3*a + b)
    else
      None

let solve1 ps =
  List.filter_map solve ps
  |> List.fold_left Int.add 0

let solve2 ps =
  let e = 10_000_000_000_000 in
  ps
  |> List.map (fun (a, b, (px, py)) ->
      a, b, (px+e, py+e))
  |> solve1

let data = Aoc2024.parse parse

let () =
  solve1 data |> print_int;
  print_newline ();
  solve2 data |> print_int
