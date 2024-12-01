let parse ss =
  ss
  |> List.map (fun str ->
      Scanf.sscanf str "%d %d" (fun a b -> a, b))
  |> List.split

let solve1 (a, b) =
  let sort = List.sort Int.compare in
  let a = sort a and b = sort b in
  List.map2 (fun a b -> abs (b-a)) a b
  |> List.fold_left Int.add 0

let solve2 (a, b) =
  let cnt n =
    List.filter ((=) n) b |> List.length
  in
  List.map (fun n -> n * cnt n) a
  |> List.fold_left Int.add 0

let data = open_in "input.txt" |> In_channel.input_lines |> parse

let () =
  solve1 data |> print_int;
  print_newline ();
  solve2 data |> print_int
