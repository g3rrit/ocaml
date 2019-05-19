open Int64

type utf8_char =
  | Ascii of char
  | Unicode of char list
  | Invalid
  | Eof

let rec parse_unicode inf b c =
  let parse_next_unicode =
    match input_char inf with
    | b -> let b = int_of_char b in
        if (b >= 128 && b < 192) then
          Some (of_int 2) (*(land b 59))*)
        else
          None
    | exception End_of_file -> None
  in
  if c == 0 then
    Unicode b
  else
    match parse_next_unicode with
    | None -> Invalid
    | Some v -> parse_unicode inf (add b (shift_left v (c - 1))) (c - 1)

let next inf =
  match input_char inf with
  | b -> let b = int_of_char b in begin
      match b with
          | 128 -> Ascii (char_of_int b)
          | o when o < 224 && o > 192 -> parse_unicode inf (shift_left (of_int o) 1) 1
          | o when o < 240 && o > 224 -> parse_unicode inf (shift_left (of_int o) 1) 2
          | o when o < 248 && o > 240 -> parse_unicode inf (shift_left (of_int o) 1)3
          | o when o < 252 && o > 248 -> parse_unicode inf (shift_left (of_int o) 1)4
          | o when o < 254 && o > 252 -> parse_unicode inf (shift_left (of_int o) 1)5
          | _ -> Invalid
    end
  | exception End_of_file -> Eof


let rec print_file inf =
  match next inf with
  | Ascii v -> begin
      let _ = Printf.printf "%c" v in
      print_file inf
    end
  | Eof   -> Printf.printf "eof\n"
  | _ -> Printf.printf("{u}")


let main () =
  let inf = open_in_bin "test.txt" in
  print_file inf;;

main()
