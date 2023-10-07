let slurp file =
  let ch = open_in file in
  let s = really_input_string ch (in_channel_length ch) in
  close_in_noerr ch;
  s

let sum_chunks chunk =
  Str.split (Str.regexp "\n") chunk
  |> List.map int_of_string
  |> List.fold_left ( + ) 0

let get_max xs = List.fold_left max min_int xs

let take3 = function
  | a::b::c::_ -> [a; b; c]
  | _ -> []

let get_max_3 xs =
  List.sort compare xs
  |> List.rev
  |> take3
  |> List.fold_left ( + ) 0

let all_chunks data =
  List.map sum_chunks @@ Str.split (Str.regexp "\n\n") data


let () =
  let data = slurp "input.txt" in
  let chunks = all_chunks data in
  Printf.printf "Sum max elf: %d\n" (get_max chunks);
  Printf.printf "Sum max 3 elves: %d\n" (get_max_3 chunks)

(* dune exec  --no-print-directory ./day01.exe *)
