let to_array str =
  String.to_seq str
  |> Array.of_seq

let kpad = [|
  "789";
  "456";
  "123";
  " 0A";
|] |> Array.map to_array

let dpad = [|
  " ^A";
  "<v>";
|] |> Array.map to_array

let (.$()) pad (x, y) =
  pad.(y).(x)

let (+$) (x, y) (a, b) =
  x+a, y+b

let edges pad (p, path) =
  let sx = Array.length pad.(0)
  and sy = Array.length pad in

  [ '>', (1, 0); '<', (-1, 0);
    'v', (0, 1); '^', (0, -1) ]
  |> List.map (fun (a, d) -> a, p +$ d)
  |> List.filter_map (fun (a, (x, y as p)) ->
      if 0 <= x && x < sx &&
         0 <= y && y < sy &&
         pad.$(p) <> ' '
      then
        Some (p, a :: path) else None)

let rec rotate buf d = function
  | [] -> List.rev buf
  | (_, e as x) :: xs ->
    if List.hd e = d then
      x :: List.rev_append buf xs
    else rotate (x :: buf) d xs

let bfs pad p d =
  let q = Queue.create () in
  let rec aux () =
    match Queue.take_opt q with
    | None -> assert false
    | Some (p, path) ->
      if pad.$(p) = d then p, path
      else begin
        let dir = match path with [] -> '>' | x :: _ -> x in
        edges pad (p, path)
        |> rotate [] dir
        |> List.iter (fun x -> Queue.add x q);
        aux ()
      end
  in
  Queue.add (p, []) q;
  aux ()

let find_path pad init cs =
  List.fold_left (fun (p, path) c ->
      let p, l = bfs pad p c in
      p, path @ List.rev ('A' :: l))
    (init, [])
    cs
  |> snd

let full_path ks =
  let find_dpad = find_path dpad (2, 0) in
  String.to_seq ks
  |> List.of_seq
  |> find_path kpad (2, 3)
  |> find_dpad
  |> find_dpad

let number ks =
  Scanf.sscanf ks "%dA" Fun.id

let solve1 kss =
  kss
  |> List.map (fun ks ->
      let p = full_path ks in
      Printf.printf "%s\n" (
        List.to_seq p
        |> String.of_seq);
      List.length p, ks)
  |> List.map (fun (x, k) ->
      x * number k)
  |> List.fold_left Int.add 0

let data = Aoc2024.read_lines ()

let () =
  let find_dpad = find_path dpad (2, 0) in
  let p cs =
    Printf.printf "%s\n"
        (List.to_seq cs |> String.of_seq)
  in
  let ks = List.hd data in
  Printf.printf "%s\n" ks;
  let d =
    String.to_seq ks |> List.of_seq
    |> find_path kpad (2, 3)
  in
  p d;
  let d = find_dpad d in
  p d;
  let d = find_dpad d in
  p d;
  Printf.printf "%d\n" @@ List.length d;
  Printf.printf "----------\n"

let () =
  solve1 data |> print_int
