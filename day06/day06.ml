let slurp file =
  let ch = open_in file in
  let s = really_input_string ch (in_channel_length ch) in
  close_in_noerr ch;
  s

module SET = Set.Make(Char)

let explode s = List.init (String.length s) (String.get s)

let set_of_str s =
  let set = SET.empty in
  List.fold_right SET.add (explode s) set

let rec find_first_group_of len s idx =
  if String.length s < len then
    None
  else
    let subs = String.sub s 0 len in
    if SET.cardinal (set_of_str subs) = len then
      Some (subs, idx)
    else
      find_first_group_of len (String.sub s 1 (String.length s - 1)) (idx + 1)

let find_first_group_of_4 = find_first_group_of 4
let find_first_group_of_14 = find_first_group_of 14

let solve f data len =
  match f data len with
  | Some (s, idx) -> Printf.printf "%s at %d\n" s idx
  | None -> Printf.printf "No group of consecutive distinct characters found"

let () =
  Printf.printf "Day 6\n";
  let data = slurp "input.txt" in
  solve find_first_group_of_4 data 4;
  solve find_first_group_of_14 data 14

  (* assert (true = true) *)

(* dune exec  --no-print-directory ./day06.exe *)
