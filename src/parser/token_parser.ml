open Base

type token =
  | Tid of String
  | Tiid of String
  | Iint of int
  | Tfloat of float
  | Tstr of String
  | Top
  | Eof

let is_char c =
  (c >= 64 && c <= 90) || (c >= 97 && c <= 122) || c = 95

let is_alpha_num c =
  (is_char c) || (c >= 48 && c <= 57)

let is_space c =
  (c = 32)


let next inp =
  let rec parse_id b inp =
    match Input.next inp with
    | Char c -> match c with
                | c when is_alpha_num c -> parser_id (c @ b) inp
                | _ -> []
  in
