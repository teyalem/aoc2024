let parse str =
  Scanf.sscanf str "%d,%d" @@ fun x y ->
  x, y

module Ip = struct
  type t = int*int
  let compare = compare
end

module S = Set.Make(Ip)

let edges map visited (x, y) =
  [ 1, 0; -1, 0; 0, -1; 0, 1 ]
  |> List.map (fun (dx, dy) -> x+dx, y+dy)
  |> List.filter (fun (x, y) ->
      0 <= x && x < 71 &&
      0 <= y && y < 71 &&
      map.(y).(x) &&
      not visited.(y).(x))

let bfs map =
  let visited = Array.make_matrix 71 71 false in
  let rec aux i ps =
    if S.is_empty ps then
      None
    else if S.exists ((=) (70, 70)) ps then
      Some i
    else begin
      S.iter (fun (x, y) ->
          visited.(y).(x) <- true)
        ps;
      S.elements ps
      |> List.concat_map (edges map visited)
      |> S.of_list
      |> aux (i+1)
    end
  in
  aux 0 @@ S.singleton (0, 0)

let rec take n = function
  | [] -> []
  | x::xs ->
    if n = 0 then []
    else x :: take (n-1) xs

let solve1 bs =
  let map = Array.make_matrix 71 71 true in
  take 1024 bs
  |>  List.iter (fun (x, y) ->
      map.(y).(x) <- false);
  Option.get @@ bfs map

let solve2 bs =
  let map = Array.make_matrix 71 71 true in
  bs
  |> List.find (fun (x, y) ->
      map.(y).(x) <- false;
      bfs map = None)

let data = Aoc2024.parse_lines parse

let () =
  solve1 data |> print_int;
  print_newline ();
  let x, y = solve2 data in
  Printf.printf "%d,%d" x y
