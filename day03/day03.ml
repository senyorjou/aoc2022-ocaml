let slurp file =
  let ch = open_in file in
  let s = really_input_string ch (in_channel_length ch) in
  close_in_noerr ch;
  s

let values = "0abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

(* part 1 *)

let sacks s =
  let cut = (String.length s) / 2 in
  let s1 = String.sub s 0 cut in
  let s2 = String.sub s cut cut in
  (s1, s2)

let get_repeated_on_sacks (s1, s2) =
  let l1 = List.init (String.length s1) (String.get s1) in
  List.filter (fun x -> String.contains s2 x) l1
  |> List.hd
  |> String.index values

let count_per_elf data =
  List.map sacks data
  |> List.map get_repeated_on_sacks
  |> List.fold_left (+) 0

(* part 2 *)

let get_repeated_on_elves (s1, s2, s3) =
  List.init (String.length s1) (String.get s1)
  |> List.filter (fun x -> String.contains s2 x)
  |> List.filter (fun x -> String.contains s3 x)
  |> List.hd
  |> String.index values

let rec part3 = function
  | a::b::c::tl -> (a,b,c):: part3 tl
  | _ -> []

let count_per_3_elves data =
  part3 data
  |> List.map get_repeated_on_elves
  |> List.fold_left (+) 0

let () =
  let data = Str.split (Str.regexp "\n") (slurp "input.txt") in
  Printf.printf "Priorities per sack %d \n" (count_per_elf data);
  Printf.printf "Priorities per 3 elves %d \n" (count_per_3_elves data)

(* dune exec  --no-print-directory ./day03.exe *)
