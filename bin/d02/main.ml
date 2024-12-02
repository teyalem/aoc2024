let parse str =
  String.split_on_char ' ' str
|> List.map int_of_string

let safe ns =
  let rec aux = function
    | [] -> []
    | [_] -> []
    | a::b::ns -> b-a :: aux (b::ns)
  in
  let ds = aux ns in
  List.for_all (fun n -> 1 <= n && n <= 3) ds ||
  List.for_all (fun n -> -3 <= n && n <= -1) ds

let solve1 nss =
  List.filter safe nss
  |> List.length

let dampen ns =
  let rec aux buf = function
    | [] -> false
    | n::ns ->
      safe (List.rev_append buf ns) || 
      aux (n::buf) ns
  in
  aux [] ns

let solve2 nss =
  let s, u = List.partition safe nss in
  let u = List.filter dampen u in
  List.length s + List.length u

let data = open_in "input.txt" |> In_channel.input_lines |> List.map parse

let () =
  solve1 data |> print_int;
  print_newline ();

  solve2 data |> print_int
