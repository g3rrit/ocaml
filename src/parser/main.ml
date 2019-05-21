open Printf

let print_file () =
  let inf = open_in_bin "test.txt" in
  let inp = Input.create inf in
  let rec print_all i =
      match Input.next i with
      | Char c -> printf "%c" c; print_all i
      | Eof -> ()
  in
  print_all inp

let print_token () =
  let inf = open_in_bin "test.txt" in
  let inp = Input.create inf in
    let rec print_all i =
      match Token_parser.next i with
      | Tid s -> printf "id: %s\n" s; print_all i
      | Tiid s -> printf "iid: %s\n" s; print_all i
      | Tother -> printf "other\n"; print_all i
      | Tint n -> printf "int: %d\n" n; print_all i
      | Tfloat n -> printf "float: %f\n" n; print_all i
      | Teof -> printf "eof\n"
      | _ -> printf "error\n"; print_all i
    in
    print_all inp

(*

let print_next inp =
  match Input.next inp with
  | Char c -> printf "%c" c
  | Eof -> ()
*)


let () =
  (*let inp = Input.create (open_in_bin "test.txt") in*)
  printf "Starting Parser\n";
  print_token ()
