let (+$) (x, y) (a, b) =
  x+a, y+b

let free_edges map pos =
  let sx = Array.length map.(0)
  and sy = Array.length map in

  [ 0, 1; 0, -1; 1, 0; -1, 0 ]
  |> List.map (fun d -> pos +$ d)
  |> List.filter_map (fun (x, y) ->
      if 0 <= x && x < sx &&
         0 <= y && y < sy then
        Some (x, y)
      else
        None)

let _edges map (pos, csp, cep, cr) =
  free_edges map pos
  |> List.filter_map (fun (x, y) ->
      if map.(y).(x) = '#' then 
        if Option.is_none csp || cr > 0 then
          let csp =
            if Option.is_some csp
            then csp else Some (x, y)
          in
          Some ((x, y), csp, cep, cr-1)
        else None

      else
        let cr =
          if Option.is_some csp && cr > 0
          then cr - 1 else cr
        in

        let cep =
          if Option.is_some csp &&
             Option.is_none cep
          then Some (x, y)
          else None
        in
        Some ((x, y), csp, cep, cr))

module M = Map.Make(Int)

let dk map st en =
  let q = ref M.empty in

  let add w v =
    q := M.add_to_list w v !q
  in
  let rec pop () =
    match M.min_binding_opt !q with
    | None -> None
    | Some (w, vs) ->
      (match vs with
       | [] -> q := M.remove w !q; pop ()
       | v::vs -> q := M.add w vs !q; Some (w, v))
  in

  let visited = Hashtbl.create 100 in

  let rec aux () =
    match pop () with
    | None -> ()
    | Some (w, p) ->
      if p = en then begin
        Hashtbl.replace visited p w;
      end
      else if Hashtbl.mem visited p then
        aux ()
      else begin
        Hashtbl.replace visited p w;
        free_edges map p
        |> List.filter (fun (x, y) ->
            map.(y).(x) <> '#')
        |> List.iter (add (w+1));
        aux ()
      end
  in

  add 0 st;
  aux ();
  visited

let find_sym map sym =
  map
  |> Array.find_mapi (fun y arr ->
      Array.find_index (fun c -> c = sym) arr
      |> Option.map (fun x -> x, y))
  |> Option.get

let dis (x, y) (a, b) =
  abs (a-x) + abs (b-y)

let rec comb seq () =
  match seq () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons (x, seq) ->
    Seq.append
      (Seq.map (fun y -> x, y) seq)
      (comb seq)
      ()

let solve map =
  let st = find_sym map 'S'
  and en = find_sym map 'E' in

  let ps =
    dk map st en
    |> Hashtbl.to_seq
    |> Seq.memoize
  in

  let p1 = ref 0 and p2 = ref 0 in

  comb ps
  |> Seq.filter_map (fun ((p1, a), (p2, b)) ->
      let d = dis p1 p2 in
      if abs (b - a) - d >= 100 then
        Some d else None)
  |> Seq.iter (fun d ->
      if d <= 2 then incr p1;
      if d <= 20 then incr p2);

  !p1, !p2

let data = Aoc2024.read_as_mat ()

let () = Printexc.record_backtrace true

let () =
  let p1, p2 = solve data in
  print_int p1;
  print_newline ();
  print_int p2;
