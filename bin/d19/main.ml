let parse lines =
  let rec aux buf = function
    | [] -> assert false
    | "" :: xs -> List.rev buf, xs
    | x :: xs -> aux (x::buf) xs
  in
  let towels, pats = aux [] lines in
  String.split_on_char ',' (List.hd towels)
  |> List.map String.trim,
  pats

let calc towels pat =
  let l = String.length pat in
  let dp = Array.make l 0 in

  for i = 0 to l-1 do
    dp.(i) <-
      towels
      |> List.filter_map (fun t ->
          let tl = String.length t in
          let p = i - tl in
          if p+1 >= 0 &&
             t = String.sub pat (p+1) tl
          then
            if p = -1 then Some 1
            else if p >= 0 then Some dp.(p)
            else None
          else
            None)
      |> List.fold_left Int.add 0;
  done;

  dp.(l-1)

let solve1 (ts, ps) =
  ps
  |> List.filter (fun p ->
      calc ts p > 0)
  |> List.length

let solve2 (ts, ps) =
  ps
  |> List.map (fun p ->
      calc ts p)
  |> List.fold_left Int.add 0

let data = Aoc2024.parse parse

let () =
  solve1 data |> print_int;
  print_newline ();
  solve2 data |> print_int
