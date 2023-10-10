let slurp file =
  let ch = open_in file in
  let s = really_input_string ch (in_channel_length ch) in
  close_in_noerr ch;
  s


let parse_pairs pair =
  match (String.split_on_char '-' pair) with
  | [left; right] -> (int_of_string left, int_of_string right)
  | _ -> assert false

let parse_line line =
  match (String.split_on_char ',' line) with
  | [left; right] -> (parse_pairs left, parse_pairs right)
  | _ -> assert false

let contains ((l1, r1), (l2, r2)) =
  (l1 <= l2 && r2 <= r1) || (l2 <= l1 && r1 <= r2)

let overlaps ((l1, r1), (l2, r2)) =
  l1 <= r2 && r1 >= l2

let check_cond data f =
  List.length @@ List.filter f data


let () =
  let data = Str.split (Str.regexp "\n") (slurp "input.txt") in
  let parsed = List.map parse_line data in
  let check = check_cond parsed in
  Printf.printf "Num of contained %d\n" (check contains);
  Printf.printf "Num of overlapped %d\n" (check overlaps)

(* dune exec  --no-print-directory ./day04.exe *)
