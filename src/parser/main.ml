
   (*
let print_file () =
  let inf = open_in_bin "test.txt" in
  let inp = new Input.input inf in
  let rec print_all i =
      match i#next with
      | Input.Char c -> printf "%c" c; print_all i
      | Input.Eof -> ()
  in
  print_all inp
*)

let print_token () =
  let inf = Stdio.In_channel.create "test.txt" in
  let inp = new Input.t inf in
    let rec print_all i =
      let token = Token_parser.next i in
      Stdio.printf "%s\n" (Token_parser.to_string token);
      if token = Teof then () else print_all i
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
  Stdio.printf "Starting Parser\n";
  print_token ()
