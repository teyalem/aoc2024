let parse str =
  Scanf.sscanf str "%d: %s@!" @@ fun n ns ->
  let ns = String.split_on_char ' ' ns
         |> List.map int_of_string
  in
  n, ns

let calc fs ns =
  let rec aux buf = function
    | [] -> buf
    | n :: ns ->
      let buf =
        List.concat_map (fun k ->
            List.map (fun f -> f k n) fs)
          buf
      in
      aux buf ns
  in
  match ns with
  | [] -> assert false
  | n :: ns -> aux [n] ns

let concat n m =
  string_of_int n ^ string_of_int m
  |> int_of_string

let calc1 = calc [Int.add; Int.mul]
let calc2 = calc [Int.add; Int.mul; concat]

let solve calc ps =
  ps
  |> List.filter_map (fun (n, ns) ->
      if List.mem n (calc ns) then Some n
      else None)
  |> List.fold_left Int.add 0

let data = open_in "input.txt" |> In_channel.input_lines |> List.map parse

let () =
  solve calc1 data |> print_int;
  print_newline ();
  solve calc2 data |> print_int;
