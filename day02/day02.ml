let slurp file =
  let ch = open_in file in
  let s = really_input_string ch (in_channel_length ch) in
  close_in_noerr ch;
  s


(* A Rock, B Paper, C Scissors:  1, 2, 3 *)
(* X Rock, Y Paper, Z Scissors *)
(* X Loose, Y Draw, Z Win: 0, 3, 6 *)

let strategy_1 = function
  | "A X" -> 1 + 3
  | "A Y" -> 2 + 6
  | "A Z" -> 3 + 0
  | "B X" -> 1 + 0
  | "B Y" -> 2 + 3
  | "B Z" -> 3 + 6
  | "C X" -> 1 + 6
  | "C Y" -> 2 + 0
  | "C Z" -> 3 + 3
  | _ -> 0

let strategy_2 = function
  | "A X" -> 3 + 0
  | "A Y" -> 1 + 3
  | "A Z" -> 2 + 6
  | "B X" -> 1 + 0
  | "B Y" -> 2 + 3
  | "B Z" -> 3 + 6
  | "C X" -> 2 + 0
  | "C Y" -> 3 + 3
  | "C Z" -> 1 + 6
  | _ -> 0


let play data strategy =
   List.map strategy data
   |> List.fold_left ( + ) 0


let () =
  let data = Str.split (Str.regexp "\n") @@ slurp "input.txt" in
  let played = play data in
  Printf.printf "Play with strategy 1: %d\n" (played strategy_1);
  Printf.printf "Play with strategy 2: %d\n" (played strategy_2)


(* dune exec  --no-print-directory ./day02.exe *)
