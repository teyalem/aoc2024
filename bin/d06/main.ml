module IP = struct
  type t = int * int

  let compare (a, b) (c, d) =
    match Int.compare a c with
    | 0 -> Int.compare b d
    | v -> v
end

module S = Set.Make(IP)
module M = Map.Make(IP)

let turn_right (dx, dy) =
  ~-dy, dx

let add (a, b) (u, v) =
  a+u, b+v

let (.$()) map (x, y) =
  try map.(y).(x)
  with _ -> '?'

let (.$()<-) map (x, y) v =
  map.(y).(x) <- v

let next map pos delta =
  if map.$(add pos delta) = '#' then
    pos, turn_right delta
  else
    add pos delta, delta

let simulate map spos =
  let rec aux s pos delta =
    if map.$(pos) = '?' then 
      s
    else
      let s = S.add pos s in
      let pos, delta = next map pos delta in
      aux s pos delta
  in
  aux S.empty spos (0, -1)

let solve1 map spos =
  simulate map spos |> S.cardinal

let solve2 map spos =
  let rec aux s pos delta =
    if map.$(pos) = '?' then
      false
    else if M.mem pos s &&
            List.mem delta @@ M.find pos s then
      true
    else
      let s = M.add_to_list pos delta s in
      let pos, delta = next map pos delta in
      aux s pos delta
  in

  let path = simulate map spos in
  let cnt = ref 0 in
  S.iter (fun pos ->
      if map.$(pos) = '.' then begin
        map.$(pos) <- '#';
        if aux M.empty spos (0, -1) then
          incr cnt;
        map.$(pos) <- '.'
      end)
    path;
  !cnt

let data = Aoc2024.read_as_mat ()

let () =
  let spos =
    Array.find_mapi (fun y arr ->
        Array.find_index (fun c -> c = '^') arr
        |> Option.map (fun x -> x, y))
      data
    |> Option.get
  in

  solve1 data spos |> print_int;
  print_newline ();
  solve2 data spos |> print_int;
