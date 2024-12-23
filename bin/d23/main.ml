module S = Set.Make(String)
module M = Map.Make(String)

let parse lines =
  lines
  |> List.map (fun s ->
      Scanf.sscanf s "%s@-%s" @@
      fun a b -> a, b)
  |> List.fold_left (fun g (a, b) ->
      g
      |> M.add_to_list a b
      |> M.add_to_list b a)
    M.empty
  |> M.map (fun l -> S.of_list l)

let reduce f seq =
  match Seq.uncons seq with
  | None -> assert false
  | Some (h, rest) -> Seq.fold_left f h rest

let neigh3 g n =
  let aux sets =
    sets
    |> List.concat_map (fun s ->
        S.to_seq s
        |> Seq.map (fun n -> M.find n g)
        |> reduce S.inter
        |> S.elements
        |> List.map (fun n -> S.add n s))
  in
  aux @@ aux [S.singleton n]

let solve1 g =
  M.to_list g
  |> List.map fst
  |> List.filter (fun s -> s.[0] = 't')
  |> List.concat_map (neigh3 g)
  |> List.sort_uniq S.compare
  |> List.length

let neigh_max g n =
  let rec aux sets =
    let next =
      sets
      |> List.concat_map (fun s ->
          S.to_seq s
          |> Seq.map (fun n -> M.find n g)
          |> reduce S.inter
          |> S.elements
          |> List.map (fun n -> S.add n s))
      |> List.sort_uniq S.compare
    in
    if List.is_empty next then sets
    else aux next
  in
  aux [S.singleton n]

let password s =
  S.elements s
  |> String.concat ","

let solve2 g =
  M.to_list g
  |> List.map fst
  |> List.concat_map (neigh_max g)
  |> List.sort (fun a b ->
      Int.compare (S.cardinal b) (S.cardinal a))
  |> List.hd
  |> password

let data = Aoc2024.parse parse

let () =
  solve1 data |> print_int;
  print_newline ();
  solve2 data |> print_string
