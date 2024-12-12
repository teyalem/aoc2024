module M = Map.Make(Int)

let (.$()) map (x, y) =
  try Some map.(y).(x)
  with _ -> None

let (.$()<-) map (x, y) v =
  map.(y).(x) <- v

let add (x, y) (a, b) =
  x+a, y+b

let neigh map pos =
  [ -1, 0; 1, 0; 0, -1; 0, 1 ]
  |> List.map (add pos)
  |> List.filter (fun p ->
      map.$(p) = map.$(pos))

let solve1 map =
  let sx = Array.length map.(0)
  and sy = Array.length map in
  let l = sx*sy in
  let area = Array.make l 0
  and peri = Array.make l 0 in
  let visited = Array.make_matrix sy sx false in


  let dfs cl pos =
    let rec aux pos =
      if visited.$(pos) = Some false then begin
        visited.$(pos) <- true;
        let ns = neigh map pos in
        area.(cl) <- area.(cl) + 1;
        peri.(cl) <- peri.(cl) + 4 - List.length ns;
        List.iter aux ns
      end
    in
    aux pos
  in

  let ci = ref 0 in
  for y = 0 to sy-1 do
    for x = 0 to sx-1 do
      if visited.$(x, y) = Some false then
        begin dfs !ci (x, y); incr ci end
    done
  done;

  Array.map2 (fun a p -> a*p) area peri
  |> Array.fold_left Int.add 0


let corners map pos =
  let n = [ -1, 0; 0, -1; 1, 0; 0, 1 ] in
  List.combine n List.(tl n @ [hd n])
  |> List.filter (fun (a, b) ->
      let pa = map.$(add pos a) = map.$(pos)
      and pb = map.$(add pos b) = map.$(pos) in
      not pa && not pb ||
      pa && pb && map.$(add pos (add a b)) <> map.$(pos))
  |> List.length

let solve2 map =
  let sx = Array.length map.(0)
  and sy = Array.length map in
  let l = sx*sy in
  let area = Array.make l 0
  and side = Array.make l 0 in
  let visited = Array.make_matrix sy sx false in

  let dfs cl pos =
    let rec aux pos =
      if visited.$(pos) = Some false then begin
        visited.$(pos) <- true;
        let ns = neigh map pos in
        area.(cl) <- area.(cl) + 1;
        side.(cl) <- side.(cl) + corners map pos;
        List.iter aux ns
      end
    in
    aux pos
  in

  let ci = ref 0 in
  for y = 0 to sy-1 do
    for x = 0 to sx-1 do
      if visited.$(x, y) = Some false then
        begin dfs !ci (x, y); incr ci end
    done
  done;

  Array.map2 (fun a p -> a*p) area side
  |> Array.fold_left Int.add 0

let data = Aoc2024.read_as_mat ()

let () =
  solve1 data |> print_int;
  print_newline ();
  solve2 data |> print_int
