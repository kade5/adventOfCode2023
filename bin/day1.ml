(* let () = print_endline (List.hd input) *)

let zero = Char.code '0'
let nine = Char.code '9'
let is_digit digit = zero <= digit && nine >= digit
let is_int char = is_digit (Char.code char)
let char_to_int chr = Char.code chr - zero
let explode str = List.init (String.length str) (String.get str)
let chars_to_int x y = (char_to_int x * 10) + char_to_int y

let numbers =
  [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]

let num_to_int str =
  match str with
  | "one" -> 1
  | "two" -> 2
  | "three" -> 3
  | "four" -> 4
  | "five" -> 5
  | "six" -> 6
  | "seven" -> 7
  | "eight" -> 8
  | "nine" -> 9
  | _ -> 0

let find_number str =
  let rec aux num_list value =
    match num_list with
    | [] -> 0
    | hd :: _ when Advent.contains value hd -> num_to_int hd
    | _ :: rest -> aux rest value
  in
  aux numbers str

let rec get_digit char_list =
  match char_list with
  | [] -> assert false
  | hd :: rest -> if is_int hd then hd else get_digit rest

let rec get_left_digit str pos =
  let num = find_number (String.sub str 0 (pos + 1)) in
  match str with
  | "" -> 0
  | str when is_int (String.get str pos) -> char_to_int (String.get str pos)
  | _ when num <> 0 -> num
  | str -> get_left_digit str (pos + 1)

let rec get_right_digit str pos =
  let num = find_number (String.sub str pos (String.length str - pos)) in
  match str with
  | "" -> 0
  | str when is_int (String.get str pos) -> char_to_int (String.get str pos)
  | _ when num <> 0 -> num
  | str -> get_right_digit str (pos - 1)

let get_number chr_list =
  chars_to_int (get_digit chr_list) (get_digit (List.rev chr_list))

let get_number_p2 str =
  let num =
    (get_left_digit str 0 * 10) + get_right_digit str (String.length str - 1)
  in
  Printf.printf "The String is %s and the number is %i\n" str num;
  num

let rec group_nums input result =
  match input with
  | [] -> result
  | hd :: rest -> group_nums rest (get_number (explode hd) :: result)

let rec group_nums_p2 input result =
  match input with
  | [] -> result
  | hd :: rest -> group_nums_p2 rest (get_number_p2 hd :: result)

(* let () = Printf.printf "%i\n" (get_left_digit "xtwone3four" 0) *)
(* let () = Printf.printf "%s\n" (String.sub "treb7uchet" 9 (String.length "treb7uchet" - 9)) *)
(* let () = Printf.printf "%i\n" (get_right_digit "xtwone3four" 10) *)

let input = Advent.read_lines "day1.txt"
let sum = List.fold_left ( + ) 0 (group_nums input [])
let () = Printf.printf "Answer to part 1 is %i\n" sum
let sum2 = List.fold_left ( + ) 0 (group_nums_p2 input [])
let () = Printf.printf "Answer to part 2 is %i\n" sum2
