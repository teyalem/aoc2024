let check1 map (x, y) (dx, dy) =
  [| 'X'; 'M'; 'A'; 'S' |]
  |> Array.mapi (fun i c ->
      map.(y+i*dy).(x+i*dx) = c)
  |> Array.for_all Fun.id

let ds = [
  -1, -1; -1, 0; -1, 1;
  0, -1;         0, 1;
  1, -1;  1, 0;  1, 1
]

let solve1 map =
  let cnt = ref 0 in
  for y = 0 to Array.length map - 1 do
    for x = 0 to Array.length map.(0) - 1 do
      List.iter (fun d ->
          if try check1 map (x, y) d
            with _ -> false
          then
            incr cnt)
        ds
    done
  done;
  !cnt

let check2 map (x, y) =
  let f (dx, dy) =
    map.(y+dy).(x+dx) = 'M' &&
    map.(y-dy).(x-dx) = 'S'
  in
  let d = [ -1, -1; 1, 1; 1, -1; -1, 1 ] in
  map.(y).(x) = 'A' &&
  (List.length @@ List.filter f d = 2)

let solve2 map =
  let cnt = ref 0 in
  for y = 0 to Array.length map - 1 do
    for x = 0 to Array.length map.(0) - 1 do
      if try check2 map (x, y)
        with _ -> false
      then
        incr cnt
    done
  done;
  !cnt

let data = Aoc2024.read_as_mat ()

let () =
  solve1 data |> print_int;
  print_newline ();
  solve2 data |> print_int
