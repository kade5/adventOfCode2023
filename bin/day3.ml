open Printf
open Bool
open List

(* let input = "617*......" *)
(* let num_list = String.split_on_char '.' input *)
let filter_blank = function "" -> false | _ -> true

(* let reduce_list = filter filter_blank num_list *)
(* let is_digit = function '0' .. '9' -> true | _ -> false *)

let is_digits str =
  try
    ignore (int_of_string str);
    false
  with Failure _ -> true

let filter_input input =
  let rec aux new_list input =
    match input with
    | [] -> new_list
    | hd :: rest ->
        aux
          ((hd |> String.split_on_char '.' |> filter filter_blank
          |> filter is_digits)
          :: new_list)
          rest
  in
  aux [] input

let extract_integer str =
  try int_of_string ((Str.global_replace (Str.regexp "[^0-9]") "") str)
  with _ -> 0

let sum_row num_list =
  let rec aux total num_list =
    match num_list with
    | [] -> total
    | hd :: rest -> aux (total + extract_integer hd) rest
  in
  aux 0 num_list

let sum_rows input =
  let rec aux total input =
    match input with [] -> total | hd :: rest -> aux (total + sum_row hd) rest
  in
  aux 0 input

let input = "test/day3.txt" |> Advent.read_lines |> filter_input
let total = sum_rows input
let () = printf "The answer to part 1 is %i\n" total
