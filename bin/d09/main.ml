let parse str =
  String.to_seq str
  |> Seq.map (fun c -> Char.(code c - code '0'))
  |> Seq.memoize

type block = {
  id : int;
  size : int;
}

let build_blocks seq =
  seq
  |> Seq.fold_left (fun (b, lid, bs) n ->
      let id, lid =
        if b then lid+1, lid+1 else -1, lid
      in
      not b, lid,
      { id; size = n } :: bs)
    (true, -1, [])
  |> (fun (_, _, bs) -> bs)

let build_disk seq =
  let l = Seq.fold_left Int.add 0 seq in
  let arr = Array.make l None in

  let i = ref 0
  and id = ref 0
  and b = ref true in
  Seq.iter (fun n ->
      let v = if !b then Some !id else None in
      for _ = 1 to n do
        arr.(!i) <- v;
        incr i
      done;
      b := not !b;
      if !b then incr id)
    seq;
  arr

let compress1 arr =
  let last = Array.length arr - 1 in
  let l = ref 0 and h = ref @@ last in

  while !l < !h do
    while Option.is_some arr.(!l) do
      incr l
    done;

    while Option.is_none arr.(!h) do
      decr h
    done;

    while !l < !h &&
          Option.is_none arr.(!l) &&
          Option.is_some arr.(!h)
    do
      arr.(!l) <- arr.(!h);
      arr.(!h) <- None;
      incr l; decr h
    done;
  done

let solve1 seq =
  let arr = build_disk seq in
  compress1 arr;
  let acc = ref 0 in
  Array.iteri (fun i n ->
      match n with
      | None -> ()
      | Some n -> acc := !acc + i*n)
    arr;
  !acc

let compress2 bs =
  let rec aux tid tsize = function
    | [] -> false, []
    | { id; size } :: bs as orig ->
      let r, rest = aux tid tsize bs in
      if r then
        true, { id; size } :: rest
      else if id <> -1 || size < tsize then
        false, orig
      else
        true,
        { id; size = size-tsize } ::
        { id = tid; size = tsize } ::
        bs
  in

  let rec try_compress = function
    | [] -> []
    | b :: bs ->
      if b.id <> -1 then
        let r, rest = aux b.id b.size bs in
        if r then
          { id = -1; size = b.size } ::
          try_compress rest
        else b :: try_compress bs
      else
        b :: try_compress bs
  in

  try_compress bs |> List.rev

let solve2 seq =
  let bs = build_blocks seq in
  let bs = compress2 bs in

  bs
  |> List.fold_left (fun (acc, lpos) { id; size } ->
      let nlpos = lpos + size in
      if id = -1 then acc, nlpos
      else
        let s = List.init size (fun i -> lpos + i)
                |> List.map (fun k -> k*id)
                |> List.fold_left Int.add 0
        in
        acc + s, nlpos)
    (0, 0)
  |> fst

let data = Aoc2024.parse_lines parse |> List.hd

let () =
  solve1 data |> print_int;
  print_newline ();
  solve2 data |> print_int
