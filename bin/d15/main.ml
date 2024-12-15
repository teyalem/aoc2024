let parse lines =
  let rec aux buf = function
    | [] -> assert false
    | "" :: xs -> List.rev buf, xs
    | x :: xs -> aux (x::buf) xs
  in
  let map, orders = aux [] lines in
  map
  |> List.map (fun str -> String.to_seq str |> Array.of_seq)
  |> Array.of_list,
  List.concat_map (fun str -> String.to_seq str |> List.of_seq) orders

let delta = function
  | '^' -> 0, -1
  | 'v' -> 0, 1
  | '<' -> -1, 0
  | '>' -> 1, 0
  | _ -> assert false

let rec move1 map (x, y) (dx, dy) =
  let tx, ty = x+dx, y+dy in
  match map.(ty).(tx) with
  | '#' -> false
  | '.' ->
    map.(ty).(tx) <- map.(y).(x);
    map.(y).(x) <-'.';
    true
  | 'O' ->
    if move1 map (tx, ty) (dx, dy) then begin
      map.(ty).(tx) <- map.(y).(x);
      map.(y).(x) <-'.';
      true
    end
    else false
  | _ -> assert false

let solve1 (map, os) =
  let rx, ry =
    Array.find_mapi (fun y arr ->
        Array.find_index (fun x -> x = '@') 
          arr
        |> Option.map (fun x -> x, y))
      map
    |> Option.get
  in
  map.(ry).(rx) <- '.';

  List.fold_left (fun (rx, ry) o ->
      let dx, dy = delta o in
      if move1 map (rx, ry) (dx, dy) then
        rx+dx, ry+dy
      else
        rx, ry)
    (rx, ry)
    os
  |> ignore;

  let cnt = ref 0 in
  Array.iteri (fun y a ->
      Array.iteri (fun x c ->
          if c = 'O' then
            cnt := !cnt + 100*y+x)
        a)
    map;

  !cnt

let expand =
  Array.map (fun a ->
      Array.to_list a
      |> List.concat_map (function
          | '#' -> ['#'; '#']
          | '.' -> ['.'; '.']
          | 'O' -> ['['; ']']
          | '@' -> ['@'; '.']
          | _ -> assert false)
      |> Array.of_list)

let move_set map (x, y) dy =
  let rec aux ps =
    let nps = set ps in
    if List.is_empty nps then ps
    else aux nps @ ps

  and set ps =
    ps
    |> List.concat_map (fun (x, y) ->
        let ty = y+dy in
        let ps =
          match map.(ty).(x) with
          | '.' -> []
          | '[' -> [x; x+1]
          | ']' -> [x-1; x]
          | '#' -> failwith "move_set"
          | _ -> assert false
        in
        List.map (fun x -> x, ty) ps)
  in
  aux [x, y]

let rec move2 map (x, y) (dx, dy) =
  let empty (x, y) = map.(y).(x) = '.' in

  let tx, ty = x+dx, y+dy in
  match map.(y).(x) with
  | '#' -> false
  | '@' ->
    if empty (tx, ty) then begin
      map.(ty).(tx) <- '@';
      map.(y).(x) <- '.';
      true
    end
    else if dx = 0 then
      try
        move_set map (x, y) dy
        |> List.iter (fun (x, y) ->
            if map.(y+dy).(x) = '.' then begin
              map.(y+dy).(x) <- map.(y).(x);
              map.(y).(x) <- '.';
            end);
        true
      with Failure _ -> false
    else begin
      move2 map (tx, ty) (dx, dy) &&
      move2 map (x, y) (dx, dy)
    end

  | '[' ->
    if dx = 0 then
      assert false

    else
      let ex = if dx = 1 then tx+1 else tx in
      if empty (ex, y) then begin
        map.(y).(x) <- '.';
        map.(y).(x+1) <- '.';
        map.(y).(tx) <- '[';
        map.(y).(tx+1) <- ']';
        true
      end
      else begin
        move2 map (ex, y) (dx, dy) &&
        move2 map (x, y) (dx, dy)
      end

  | ']' -> move2 map (x-1, y) (dx, dy)
  | _ -> assert false

let solve2 (map, os) =
  let rx, ry =
    Array.find_mapi (fun y arr ->
        Array.find_index (fun x -> x = '@') 
          arr
        |> Option.map (fun x -> x, y))
      map
    |> Option.get
  in

  List.fold_left (fun (rx, ry) o ->
      let dx, dy = delta o in
      if move2 map (rx, ry) (dx, dy) then
        rx+dx, ry+dy
      else
        rx, ry)
    (rx, ry)
    os
  |> ignore;

  let cnt = ref 0 in
  Array.iteri (fun y a ->
      Array.iteri (fun x c ->
          if c = '[' then
            cnt := !cnt + 100*y+x)
        a)
    map;

  !cnt

let data = Aoc2024.parse parse

let () =
  let map, os = data in
  solve1 (Array.(map copy) map, os)
  |> print_int;
  print_newline ();
  solve2 (expand map, os)
  |> print_int
