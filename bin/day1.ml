(* let () = print_endline (List.hd input) *)

let zero = Char.code '0'
let nine = Char.code '9'
let is_digit digit = zero <= digit && nine >= digit
let is_int char = is_digit (Char.code char)
let char_to_int chr = Char.code chr - zero
let explode str = List.init (String.length str) (String.get str)
let chars_to_int x y = (char_to_int x * 10) + char_to_int y

let rec get_digit char_list =
  match char_list with
  | [] -> assert false
  | hd :: rest -> if is_int hd then hd else get_digit rest

let get_number chr_list =
  chars_to_int (get_digit chr_list) (get_digit (List.rev chr_list))

let rec group_nums input result =
  match input with
  | [] -> result
  | hd :: rest -> group_nums rest (get_number (explode hd) :: result)

let input = Advent.read_lines "day1.txt"
let sum = List.fold_left ( + ) 0 (group_nums input [])
let () = Printf.printf "Answer to part 1 is %i\n" sum
