let read_lines file =
  In_channel.with_open_text file In_channel.input_all
  |> Str.(split (regexp "\n"))

let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try
    ignore (Str.search_forward re s1 0);
    true
  with Not_found -> false
