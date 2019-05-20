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

(*

let print_next inp =
  match Input.next inp with
  | Char c -> printf "%c" c
  | Eof -> ()
*)


let () =
  (*let inp = Input.create (open_in_bin "test.txt") in*)
  printf "Starting Parser\n";
  print_file ()
