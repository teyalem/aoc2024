open Scanf

type t = {
  mutable a : int;
  mutable b : int;
  mutable c : int;
  mutable pc : int;
}

let parse_prog str =
  sscanf str "Program: %s@!" @@ fun ns ->
  String.split_on_char ',' ns
  |> List.map int_of_string
  |> Array.of_list

let parse lines =
  let rec aux buf = function
    | [] -> assert false
    | "" :: xs -> List.rev buf, xs
    | x :: xs -> aux (x::buf) xs
  in
  let r, p = aux [] lines in
  (match r with
  | [a;b;c] ->
    sscanf a "Register A: %d" @@ fun a ->
    sscanf b "Register B: %d" @@ fun b ->
    sscanf c "Register C: %d" @@ fun c ->
    { a; b; c; pc = 0 }
  | _ -> assert false),
  parse_prog @@ List.hd p

let combo t = function
  | (0|1|2|3) as n -> n
  | 4 -> t.a
  | 5 -> t.b
  | 6 -> t.c
  | _ -> assert false

let com report t prog =
  let next () = t.pc <- t.pc + 2 in
  let combo () = combo t prog.(t.pc+1) in
  let div () =
    t.a / (1 lsl combo ())
  in
  match prog.(t.pc) with
  | 0 -> t.a <- div (); next ()
  | 1 -> t.b <- t.b lxor prog.(t.pc+1); next()
  | 2 -> t.b <- combo () mod 8; next ()
  | 3 ->
    if t.a = 0 then next ()
    else t.pc <- prog.(t.pc+1)
  | 4 -> t.b <- t.b lxor t.c; next ()
  | 5 -> report @@ combo () mod 8; next ()
  | 6 -> t.b <- div (); next ()
  | 7 -> t.c <- div (); next ()
  | _ -> assert false

let run t prog =
  let out = ref [] in
  let report n =
    out := n :: !out
  in
  let rec aux () =
    try
      com report t prog;
      aux ()
    with _ -> ()
  in
  aux ();
  List.rev !out

let solve1 (t, prog) =
  let o = run t prog in
  List.map (string_of_int) o
  |> String.concat ","

(* hand-disassembled bfs *)
let solve2 (_, prog) =
  let cand a n =
    List.init 8 Fun.id
    |> List.filter (fun i ->
        let a = a*8+i in
        let b = i lxor 1 in
        let c = a lsr b in
        let b = (b lxor 5) lxor c in
        b mod 8 = n)
  in

  let rec bfs a = function
    | [] -> Some a
    | n :: ns ->
      cand a n
      |>  List.find_map (fun i ->
          Printf.printf "acc: %d n: %d\n" a i;
          bfs (a*8 + i) ns)
  in

  Array.to_list prog
  |> List.rev
  |> bfs 0
  |> Option.get

let data = Aoc2024.parse parse

let () =
  solve1 data |> print_endline;
  solve2 data |> print_int
