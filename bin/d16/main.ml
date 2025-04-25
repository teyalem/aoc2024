let add (x, y) (a, b) =
  x+a, y+b

let rot (x, y) i =
  ~-i*y, i*x

let (.$()) map (x, y) =
  map.(y).(x)

let edges map (pos, delta, visited) =
  let np = add pos delta in
  [ if map.$(np) <> '#' &&
       not @@ List.mem (np, delta) visited
    then
      Some (np, delta, 1) else None;
    Some (pos, rot delta 1, 1000);
    Some (pos, rot delta ~-1, 1000)
  ]
  |> List.filter_map
  @@ Option.map (fun (p, d, w) -> w, (p, d, (p, d) :: visited))

module M = Map.Make(Int)

let dk map st =
  let q = ref M.empty in

  let add w d = q := M.add_to_list w d !q in
  let rec pop () =
    match M.min_binding_opt !q with
    | None -> None
    | Some (w, ds) ->
      begin match ds with
        | [] -> q := M.remove w !q; pop ()
        | d::ds ->
          q := M.add w ds !q;
          Some (w, d)
      end
  in

  let rec aux () =
    match pop () with
    | None -> assert false
    | Some (cost, (p, d, v)) ->
      if map.$(p) = 'E' then
        cost
      else begin
        edges map (p, d, v)
        |> List.iter (fun (w, x) ->
            add (cost + w) x);
        aux ()
      end
  in

  let s = st, (1, 0) in
  add 0 (fst s, snd s, [s]);
  aux ()

let find_sym map sym =
  map
  |> Array.find_mapi (fun y arr ->
      Array.find_index (fun c -> c = sym) arr
      |> Option.map (fun x -> x, y))
  |> Option.get

let find_start map =
  find_sym map 'S'

let solve1 map =
  let st = find_start map in
  dk map st

let muldk map st =
  let q = ref M.empty in

  let add w d =
    q := M.add_to_list w d !q
  in
  let rec pop () =
    match M.min_binding_opt !q with
    | None -> None
    | Some (w, ds) ->
      begin match ds with
        | [] -> q := M.remove w !q; pop ()
        | d::ds ->
          q := M.add w ds !q;
          Some (w, d)
      end
  in

  let best = ref ~-1 in
  let paths = ref [] in

  let rec aux () =
    match pop () with
    | None -> ()
    | Some (cost, (p, d, v)) ->
      if map.$(p) = 'E' then begin
        if !best = -1 then begin
          best := cost;
          paths := v :: !paths;
          aux ()
        end
        else if cost = !best then begin
          paths := v :: !paths;
          aux ()
        end
      end

      else begin
        edges map (p, d, v)
        |> List.iter (fun (w, x) ->
            add (cost + w) x);
        aux ()
      end
  in

  let s = st, (1, 0) in
  add 0 (fst s, snd s, [s]);
  aux ();
  !paths

let solve2 map =
  let st = find_start map in
  muldk map st
  |> List.concat
  |> List.sort_uniq compare
  |> List.length

let data = Aoc2024.read_as_mat ()

let () =
  solve1 data |> print_int;
  print_newline ();
  solve2 data + 1 |> print_int
