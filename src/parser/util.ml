

let str_crt (l : char list) : string =
  let buf = Buffer.create (List.length l) in
  List.iter (Buffer.add_char buf) l;
  Buffer.contents buf

let int_crt (l : char list) : int =
  let sv = str_crt l in
  int_of_string sv

let float_crt (l : char list) : float =
  let sv = str_crt l in
  float_of_string sv
