let add (x, y) (a, b) =
  x+a, y+b

let (.$()) map (x, y) =
  map.(y).(x)

let edges map pos =
  let sx = Array.length map.(0)
  and sy = Array.length map in
  let n = map.$(pos) in

  [-1, 0; 1, 0; 0, -1; 0, 1 ]
  |> List.map (add pos)
  |> List.filter (fun (x, y) ->
      0 <= x && x < sx &&
      0 <= y && y < sy &&
      map.$(x, y) = n+1)

let bfs ~unique map pos =
  let rec aux ps =
    let ps =
      List.concat_map (edges map) ps
    in
    if List.exists (fun p -> map.$(p) = 9) ps then
      let ps =
        if unique
        then List.sort_uniq compare ps
        else ps
      in
      List.length ps
    else
      aux ps
  in
  aux [pos]

let bfs1 = bfs ~unique: true
let bfs2 = bfs ~unique: false

let solve bfs map =
  let cnt = ref 0 in
  Array.iteri (fun y ->
      Array.iteri (fun x n ->
          if n = 0 then
            cnt := !cnt + bfs map (x, y)))
    map;
  !cnt

let data =
  Aoc2024.read_as_mat ()
  |> Array.map (Array.map (fun c ->
      Char.(code c - code '0')))

let () =
  solve bfs1 data |> print_int;
  print_newline ();
  solve bfs2 data |> print_int;
