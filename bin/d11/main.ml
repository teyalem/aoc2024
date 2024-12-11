let parse str =
  String.trim str
  |> String.split_on_char ' '
  |> List.map int_of_string

let try_split n =
  let s = string_of_int n in
  let l = String.length s in
  if l mod 2 = 0 then
    let h = l/2 in
    let f a l =
      int_of_string @@ String.sub s a l
    in
    let a = f 0 h and b = f h h in
    Some (a, b)
  else None

let blink ns =
  let aux n =
    if n = 0 then [1]
    else match try_split n with
      | Some (a, b) -> [a; b]
      | None -> [n*2024]
  in
  List.concat_map aux ns

let solve1 ns =
  let rec aux i ns =
    if i = 0 then ns
    else
      let ns = blink ns in
      aux (i-1) ns
  in
  aux 25 ns |> List.length

module M = Map.Make(Int)

let solve2 ns =
  let add k n =
    M.update k (function
        | None -> Some n
        | Some a -> Some (a+n))
  in

  let blink m =
    M.fold (fun k n m ->
        if k = 0 then add 1 n m
        else match try_split k with
          | Some (a, b) ->
            add a n m |> add b n
          | None -> add (k*2024) n m)
      m
      M.empty
  in

  let rec aux i state =
    if i = 0 then state
    else
      let state = blink state in
      aux (i-1) state
  in

  let res =
    List.fold_left (fun m n -> add n 1 m)
      M.empty ns
    |> aux 75
  in
  M.fold (fun _ n acc -> acc + n) res 0

let data = Aoc2024.read_all () |> parse

let () =
  solve1 data |> print_int;
  print_newline ();
  solve2 data |> print_int
