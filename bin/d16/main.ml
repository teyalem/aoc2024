let add (x, y) (a, b) =
  x+a, y+b

let rot (x, y) i =
  ~-i*y, i*x

let (.$()) map (x, y) =
  map.(y).(x)

let edges map (pos, delta) =
  [ if map.$(add pos delta) <> '#' then
      Some (add pos delta, delta, 1)
    else
      None;
    Some (pos, rot delta 1, 1000);
    Some (pos, rot delta ~-1, 1000)
  ]
  |> List.filter_map
  @@ Option.map (fun (p, d, w) -> w, (p, d))

module M = Map.Make(Int)

let dk map st =
  let q = ref M.empty in
  let visited = Hashtbl.create 100 in

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
    | Some (cost, (p, d)) ->
      if Hashtbl.mem visited (p, d) then
        aux ()
      else if map.$(p) = 'E' then
        cost
      else begin
        Hashtbl.replace visited (p, d) true;
        edges map (p, d)
        |> List.iter (fun (w, x) ->
            add (cost + w) x);
        aux ()
      end
  in

  add 0 (st, (1, 0));
  aux ()

let find_start map =
  map
  |> Array.find_mapi (fun y arr ->
      Array.find_index (fun c -> c = 'S') arr
      |> Option.map (fun x -> x, y))
  |> Option.get

let solve1 map =
  let st = find_start map in
  dk map st

let edges map (pos, delta) path =
  let fwd = add pos delta in
  [ if map.$(fwd) <> '#' then
      Some (fwd, delta, 1, fwd::path)
    else
      None;
    Some (pos, rot delta 1, 1000, path);
    Some (pos, rot delta ~-1, 1000, path)
  ]
  |> List.filter_map
  @@ Option.map (fun (p, d, w, path) ->
      w, (p, d), path)

let muldk map ?(best_cost = 0) ?(best_path = []) st =
  let q = ref M.empty in
  let visited = Hashtbl.create 100 in

  let add w d path =
    q := M.add_to_list w (d, path) !q
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

  let all_paths = ref [] in

  let rec aux () =
    match pop () with
    | None -> -1
    | Some (cost, ((p, d), path)) ->
      let x, y = p and dx, dy = d in
      Printf.printf "dk %d (%d, %d) (%d, %d) %c\n" cost x y dx dy map.$(p);

      if Hashtbl.mem visited (p, d) then begin
        if List.mem p best_path &&
           cost <= best_cost
        then
          all_paths := path :: !all_paths;
        aux ()
      end

      else if map.$(p) = 'E' &&
              best_path = []
      then begin
        all_paths := [path];
        cost
      end

      else begin
        Hashtbl.replace visited (p, d) true;
        edges map (p, d) path
        |> List.iter (fun (w, x, path) ->
            add (cost + w) x path);
        aux ()
      end
  in

  add 0 (st, (1, 0)) [st];
  let cost = aux () in

  if best_path = [] then
    cost, List.hd !all_paths
  else
    0, List.concat !all_paths

let solve2 map =
  let st = find_start map in
  let best_cost, best_path = muldk map st in
  muldk map st ~best_cost ~best_path
  |> snd
  |> List.sort_uniq compare
  |> List.length

let data = Aoc2024.read_as_mat ()

let () =
  solve1 data |> print_int;
  print_newline ();
  solve2 data |> print_int
