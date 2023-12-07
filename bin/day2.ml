let split_and_trim char str =
  let str_list = String.split_on_char char str in
  let rec aux str_list new_list =
    match str_list with
    | [] -> new_list
    | hd :: rest -> aux rest (String.trim hd :: new_list)
  in
  aux str_list []

let get_game_id str = int_of_string (List.nth (String.split_on_char ' ' str) 1)

let split_cubes str =
  let rec aux cubes new_list =
    match cubes with
    | [] -> new_list
    | hd :: rest -> aux rest (String.split_on_char ' ' hd :: new_list)
  in
  aux (split_and_trim ',' str) []

let split_subsets subsets =
  let rec aux subsets new_list =
    match subsets with
    | [] -> new_list
    | hd :: rest -> aux rest (split_cubes hd :: new_list)
  in
  aux subsets []

let is_color_valid color num =
  match color with
  | "blue" when num <= 14 -> true
  | "red" when num <= 12 -> true
  | "green" when num <= 13 -> true
  | _ -> false

let rec is_set_valid set =
  match set with
  | [] -> true
  | hd :: rest
    when is_color_valid (List.nth hd 1) (int_of_string (List.nth hd 0)) ->
      is_set_valid rest
  | _ -> false

(* let () = Printf.printf "%i\n" (get_game_id "Game 4") *)

let rec is_game_valid subset_list =
  match subset_list with
  | [] -> true
  | hd :: rest when is_set_valid hd -> is_game_valid rest
  | _ -> false

let sum_ids input =
  let rec aux sum_games input =
    match input with
    | [] -> sum_games
    | hd :: rest
      when let game = split_and_trim ':' hd in
           let subsets = split_and_trim ';' (List.nth game 0) in
           let subset_list = split_subsets subsets in
           is_game_valid subset_list ->
        aux (sum_games + get_game_id (List.nth (split_and_trim ':' hd) 1)) rest
    | _ :: rest -> aux sum_games rest
  in
  aux 0 input

let input = Advent.read_lines "day2.txt"
(* let str = *)
(*   "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red" *)

(* let game = split_and_trim ':' str *)
(* let subsets = split_and_trim ';' (List.nth game 0) *)

let part1_total = sum_ids input
let () = Printf.printf "The answer to part 1 is %i\n" part1_total

(* let game_id = get_game_id (List.nth game 0) *)
(* let subset_list = split_subsets subsets *)
(* let result = is_game_valid subset_list *)
(* let () = Printf.printf "Game ID: %i\n" (get_game_id (List.nth game 1)) *)
(* let () = Printf.printf "Valid? %s\n" (string_of_bool result) *)
