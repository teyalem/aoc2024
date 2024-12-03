let calc s =
  Scanf.sscanf s "mul(%d,%d)" @@ fun a b -> a*b

let parse str =
  let re = Re.compile @@ Re.Posix.re "mul\\([0-9]+,[0-9]+\\)" in
  Re.matches re str
  |> List.map calc

let parse2 str =
  let re = Re.compile @@ Re.Posix.re "mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\)" in
  Re.matches re str
  |> List.fold_left (fun (b, acc) s ->
      match s with
      | "do()" -> true, acc
      | "don't()" -> false, acc
      | s ->
        b, (if b then calc s + acc else acc))
    (true, 0)
  |>  snd

let data = open_in "input.txt" |> In_channel.input_all

let () =
  parse data
  |> List.fold_left Int.add 0
  |> print_int;
  print_newline ();

  parse2 data |> print_int
