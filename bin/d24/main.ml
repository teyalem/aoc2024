let parse_bit s =
  Scanf.sscanf s "%s@: %d" (fun a b -> a, b)

let parse_wire s =
  Scanf.sscanf s "%s %s %s -> %s" @@ fun a o b p -> p, (o, a, b)

let parse lines =
  let rec aux buf = function
    | [] -> List.rev buf, []
    | x :: xs ->
      if x = "" then List.rev buf, xs
      else aux (x::buf) xs
  in
  let b, w = aux [] lines in
  List.map parse_bit b, List.map parse_wire w

let calc b w =
  let h = Hashtbl.create 100 in
  let rec aux p =
    if Hashtbl.mem h p then
      Hashtbl.find h p
    else begin
      let o, a, b = List.assoc p w in
      let a, b = aux a, aux b in
      let r =
        match o with
        | "AND" -> a land b
        | "OR" -> a lor b
        | "XOR" -> a lxor b
        | _ -> assert false
      in
      Hashtbl.replace h p r;
      r
    end
  in
  List.iter (fun (b, v) -> Hashtbl.replace h b v) b;
  List.iter (fun (p, _) -> ignore @@ aux p) w;
  h

let number h =
  Hashtbl.to_seq h
  |> Seq.filter (fun (p, _) -> p.[0] = 'z')
  |> List.of_seq
  |> List.fold_left (fun acc (p, b) ->
      let n = Scanf.sscanf p "z%d" Fun.id in
      acc + b lsl n)
    0

let solve1 (b, w) =
  calc b w
  |> number

let print h =
  let open Printf in
  printf "digraph {\n";
  Hashtbl.iter (fun k v ->
      printf "%s -> %s\n" k v)
    h;
  printf "}\n"

let solve2 (_, w) =
  let h = Hashtbl.create 1000 in
  List.iter (fun (p, (_, a, b)) ->
      Hashtbl.add h a p;
      Hashtbl.add h b p)
    w;
  print h

let data = Aoc2024.parse parse

let () =
  solve1 data |> print_int;
  print_newline ();
  solve2 data
