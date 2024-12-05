module M = Map.Make(Int)

let split xs =
  let rec aux buf = function
    | [] -> assert false
    | x :: xs ->
      if x = "" then
        List.rev buf, xs
      else
        aux (x::buf) xs
  in
  aux [] xs

let parse lines =
  let open Scanf in
  let g, u = split lines in
  g
  |> List.map (fun s ->
      sscanf s "%d|%d" @@ fun a b -> a, b)
  |> List.fold_left (fun m (a, b) ->
      M.add_to_list a b m)
    M.empty
  ,
  List.map (fun s ->
      String.split_on_char ',' s
      |> List.map int_of_string)
    u

let precede m a b =
  let es = M.find_opt a m |> Option.value ~default: [] in
  List.mem b es

let sum_middle xs =
  xs
  |> List.map (fun u ->
      List.(nth u (length u / 2)))
  |> List.fold_left Int.add 0

let rec correct m = function
  | [] -> true
  | x :: xs ->
    List.for_all (precede m x) xs &&
    correct m xs

let solve1 (m, us) =
  us
  |> List.filter (correct m)
  |> sum_middle

let els xs =
  let rec aux buf = function
    | [] -> []
    | x::xs ->
      let rest = aux (x::buf) xs in
      (x, buf @ xs) :: rest
  in
  aux [] xs

let solve2 (m, us) =
  let rec fix xs =
    if List.is_empty xs then []
    else
      let x, xs =
        els xs
        |> List.find (fun (x, xs) ->
            List.for_all (precede m x) xs)
      in
      x :: fix xs
  in
  List.filter (Fun.negate (correct m)) us
  |> List.map fix 
  |> sum_middle

let data = open_in "input.txt" |> In_channel.input_lines |> parse

let () =
  solve1 data |> print_int;
  print_newline ();
  solve2 data |> print_int
