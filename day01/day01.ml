let slurp file =
  let ch = open_in file in
  let s = really_input_string ch (in_channel_length ch) in
  close_in_noerr ch;
  s

let sum_chunks chunk =
  Str.split (Str.regexp "\n") chunk
  |> List.map int_of_string
  |> List.fold_left ( + ) 0


let all_chunks data =
  let compare_rev x y =
    -1 * (compare x y) in
  Str.split (Str.regexp "\n\n") data
  |> List.map sum_chunks
  |> List.sort compare_rev

let rec take_n n xs =
  if n < 1 then []
  else match xs with
    | h::t -> h::take_n (n - 1) t
    | _ -> []

let get_max_n n xs =
  take_n n xs
  |> List.fold_left ( + ) 0


let () =
  let data = slurp "input.txt" in
  let chunks = all_chunks data in
  Printf.printf "Sum max elf: %d\n" (get_max_n 1 chunks);
  Printf.printf "Sum max 3 elves: %d\n" (get_max_n 3 chunks);
  assert ((get_max_n 1 chunks) = 72511)

(* dune exec  --no-print-directory ./day01.exe *)
