module IP = struct
  type _t = int * int
  let compare (a, b) (x, y) =
    match Int.compare a x with
    | 0 -> Int.compare b y
    | v -> v

  let map2 f (a, b) (x, y) =
    f a x, f b y

  let (+) = map2 Int.add
  let (-) = map2 Int.sub
  let ( * ) n (a, b) = n*a, n*b
end

module M = Map.Make(Char)

let antinodes1 a b =
  [ IP.(a + 2*(b - a));
    IP.(b - 2*(b - a)) ]

let scan map =
  let nodes = ref M.empty in
  Array.iteri (fun y arr ->
      Array.iteri (fun x c ->
          if c <> '.' then
            nodes := M.add_to_list c (x, y) !nodes)
        arr)
    map;
  !nodes

let rec pairs = function
  | [] | [_] -> []
  | a :: bs ->
    List.map (fun b -> a, b) bs @
    pairs bs

(* just make enough nodes to check *)
let antinodes2 a b =
  let d = IP.(b - a) in
  List.init 201 (fun i ->
      let i = i - 100 in
      IP.(a + i*d))

let solve antinodes map =
  let sx = Array.length map.(0)
  and sy = Array.length map in
  scan map
  |> M.map (fun ps ->
      pairs ps
      |> List.concat_map (fun (a, b) ->
          antinodes a b)
      |> List.filter (fun (x, y) ->
          0 <= x && x < sx &&
          0 <= y && y < sy))
  |> M.to_list
  |> List.concat_map snd
  |> List.sort_uniq IP.compare
  |> List.length

let data = Aoc2024.read_as_mat ()

let () =
  solve antinodes1 data |> print_int;
  print_newline ();
  solve antinodes2 data |> print_int;
