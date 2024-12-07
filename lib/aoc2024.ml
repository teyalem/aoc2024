let read_lines () =
  open_in "input.txt" |> In_channel.input_lines 

let parse f =
  read_lines () |> f

let parse_lines f =
  read_lines () |> List.map f

let read_as_mat () =
  read_lines ()
  |> List.map (fun s ->
      String.to_seq s |> Array.of_seq)
  |> Array.of_list
