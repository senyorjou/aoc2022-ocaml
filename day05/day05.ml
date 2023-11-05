let slurp file =
  let ch = open_in file in
  let s = really_input_string ch (in_channel_length ch) in
  close_in_noerr ch;
  s


let load_store store =
  Hashtbl.add store 1 ["N"; "Z"];
  Hashtbl.add store 2 ["D"; "C"; "M"];
  Hashtbl.add store 3 ["P"]


let rec take_n n xs =
  if n < 1 then []
  else match xs with
    | h::t -> h::take_n (n - 1) t
    | _ -> []

let rec rem_n n xs =
  if n < 1 then xs
  else match xs with
    | _::t -> rem_n (n - 1) t
    | _ -> []

let move_crates_9001 store qty a b =
  let a_list = Hashtbl.find store a in
  let b_list = Hashtbl.find store b in
  let new_a = rem_n qty a_list in
  let new_b = List.append (take_n qty a_list) b_list in
  Hashtbl.replace store a new_a;
  Hashtbl.replace store b new_b

let move_crates_9000 store qty a b =
  let a_list = Hashtbl.find store a in
  let b_list = Hashtbl.find store b in
  let new_a = rem_n qty a_list in
  let new_b = List.append (List.rev (take_n qty a_list)) b_list in
  Hashtbl.replace store a new_a;
  Hashtbl.replace store b new_b


let parse_move s =
  let moves = String.split_on_char ' ' s in
  let num = int_of_string (List.nth moves 1) in
  let a = int_of_string (List.nth moves 3) in
  let b = int_of_string (List.nth moves 5) in
  (num, a, b)


let load_moves input =
  Str.split (Str.regexp "\n") input
  |> List.map parse_move

let engage_crane_9000 store moves =
  List.map (fun (qty, a, b) -> (move_crates_9000 store qty a b)) moves

let engage_crane_9001 store moves =
  List.map (fun (qty, a, b) -> (move_crates_9001 store qty a b)) moves

let rec last = function
  | x::[] -> x
  | _::xs -> last xs
  | []    -> failwith "no element"

let get_cranes_top store =
  List.init 3 (fun x -> x + 1)
  |> List.map (fun x -> Hashtbl.find store x)
  |> List.map List.hd
  |> List.fold_left ( ^ ) ""


let () =
  let store = Hashtbl.create 3 in
  load_store store;

  let raw_moves  = last (Str.split (Str.regexp "\n\n") (slurp "input-test.txt")) in

  let moves = load_moves raw_moves in
  let _ = engage_crane_9000 store moves in
  Printf.printf "Crates: %s\n" (get_cranes_top store);
  load_store store;
  let _ = engage_crane_9001 store moves in
  Printf.printf "Crates: %s\n" (get_cranes_top store);



(* dune exec  --no-print-directory ./day05.exe *)
