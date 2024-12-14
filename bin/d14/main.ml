let parse str =
  Scanf.sscanf str "p=%d,%d v=%d,%d" @@ fun px py vx vy -> (px, py), (vx, vy)

let sx = 101
let sy = 103

let pos_after t ((px, py), (vx, vy)) =
  let f p v s =
    let r = (p + t*v) mod s in
    (r + s) mod s
  in
  f px vx sx, f py vy sy

let count ps (lx, ly, hx, hy) =
  ps
  |> List.filter (fun (x, y) ->
      lx <= x && x < hx &&
      ly <= y && y < hy)
  |> List.length

let solve1 rs =
  let ps = List.map (pos_after 100) rs in
  [ 0, 0, sx/2, sy/2;
    sx/2+1, 0, sx, sy/2;
    0, sy/2+1, sx/2, sy;
    sx/2+1, sy/2+1, sx, sy ]
  |> List.map (count ps)
  |> List.fold_left Int.mul 1

let solve2 rs =
  let l = List.length rs in
  let aux t =
    List.map (pos_after t) rs
  in

  Seq.ints 1
  |> Seq.map (fun t -> t, aux t)
  |> Seq.filter (fun (_, ps) ->
      List.length @@ List.sort_uniq compare ps = l)
  |> Seq.iter (fun (t, ps) ->
      let mat = Array.make_matrix sy sx '.' in
      List.iter (fun (x, y) -> mat.(y).(x) <- '#') ps;

      let open Printf in
      printf "t = %d:\n" t;
      Array.iter (fun arr ->
          Array.iter (printf "%c") arr;
          printf "\n")
        mat;
      flush stdout;

      ignore @@ read_line ())

let data = Aoc2024.parse_lines parse

let () =
  solve1 data |> print_int;
  print_newline ();
  solve2 data
