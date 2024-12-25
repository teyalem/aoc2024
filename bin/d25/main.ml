let split sep xs =
  List.fold_right (fun x acc ->
      if x = sep then
        [] :: acc
      else match acc with
        | [] -> [[x]]
        | l :: tl -> (x::l) :: tl)
    xs
    []

let is_lock mat =
  Array.for_all (fun c -> c = '#') mat.(0)

let parse lines =
  split "" lines
  |> List.map (fun mat ->
      List.map (fun s ->
          String.to_seq s |> Array.of_seq)
        mat
      |> Array.of_list)
  |>  List.partition is_lock

let fit l k =
  Array.for_all2
    (Array.for_all2 (fun c d ->
         c <> d || (c = '.' && d = '.')))
    l k

let solve1 (ls, ks) =
  Seq.product
    (List.to_seq ls)
    (List.to_seq ks)
  |> Seq.filter (fun (l, k) -> fit l k)
  |> Seq.length

let data = Aoc2024.parse parse

let () =
  solve1 data |> print_int
