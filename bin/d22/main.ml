let mix n m =
  n lxor m

let prune n =
  n mod 16777216

let next n =
  let n = prune @@ mix n (n*64) in
  let n = prune @@ mix n (n/32) in
  prune @@ mix n (n*2048)

let nth n seq =
  Seq.drop (n-1) seq
  |> Seq.uncons
  |> Option.get
  |> fst

let solve1 seeds =
  seeds
  |> List.map (fun n ->
      Seq.iterate next n
      |> nth 2001)
  |> List.fold_left Int.add 0

let rec window n seq () =
  match seq () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons (_, nseq) ->
    let h = Seq.take n seq in
    if Seq.length h < n then Seq.Nil
    else Seq.Cons (h, window n nseq)

let stonks seed =
  match
    Seq.iterate next seed
    |> Seq.map (fun n -> n mod 10)
    |> Seq.take 2001
    |> Seq.uncons
  with
  | None -> assert false
  | Some (x, seq) ->
    seq
    |> Seq.scan (fun (p, _) n -> n, n - p)
      (x, 0)
    |> Seq.drop 1
    |> Seq.memoize

let e_banana pat s =
  window 4 s
  |> Seq.find (fun w ->
      Seq.for_all2 (fun (_, d) e -> d = e)
        w (List.to_seq pat))
  |> Option.map (fun s -> nth 3 s |> fst)
  |> Option.value ~default: 0

let e_pat pat sts =
  List.map (e_banana pat) sts
  |> List.fold_left Int.add 0

let solve2 seeds =
  let stonks = List.map stonks seeds in

  let pats seq =
    window 4 seq
    |> Seq.map (fun p ->
        Seq.map snd p |> List.of_seq)
  in

  Printf.printf "collecting pats\n";
  flush stdout;
  let all_pats =
    let h = Hashtbl.create 1000 in
    stonks
    |> List.concat_map (fun s ->
        pats s
        |> List.of_seq
        |> List.map (fun p -> p, s))
    |> List.iter (fun (p, s) ->
        match Hashtbl.find_opt h p with
        | None -> Hashtbl.replace h p [s]
        | Some xs ->
          Hashtbl.replace h p (s :: xs));

    Hashtbl.to_seq h
      (*
    |> Seq.filter (fun (_, o) ->
        List.length o > 100)
         *)
    |> List.of_seq
    |> List.sort (fun (_, a) (_, b) ->
        let l = List.length in
        Int.compare (l b) (l a))
    |> List.filteri (fun i _ -> i < 100)
  in
  Printf.printf "collected pats len: %d\n" @@ List.length all_pats;
  Printf.printf "max len: %d\n" @@ List.length @@ snd @@ List.hd all_pats;
  flush stdout;

  let p, n =
    all_pats
    |> List.map (fun (p, xs) -> p, e_pat p xs)
    |> List.fold_left (fun (ap, ae) (p, e) ->
        if e > ae then p, e
        else ap, ae)
      ([], min_int)
  in

  (match p with
   | [a;b;c;d] ->
     Printf.printf "pat %d,%d,%d,%d\n" a b c d
   | _ -> assert false);

  n

let data = Aoc2024.parse_lines int_of_string

let () =
  solve1 data |> print_int;
  print_newline ();
  solve2 data |> print_int
