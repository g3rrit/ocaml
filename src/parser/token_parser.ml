open Base
open Util

type token =
  | Tid of string
  | Tiid of string
  | Tint of int
  | Tfloat of float
  | Tstr of string
  | Top
  | Tother
  | Teof

(* Helper Functions *)

let ccode = Char.to_int

let is_char c =
  let cc = ccode c in (cc >= 64 && cc <= 90) || (cc >= 97 && cc <= 122) || cc = 95

let is_num c =
  let cc = ccode c in (cc >= 48 && cc <= 57)

let is_alpha_num c =
  (is_char c) || (is_num c)


let is_space c =
  let cc = ccode c in (cc = 32)

let is_hash c =
  let cc = ccode c in (cc = 35)

let is_ignore c =
  let cc = ccode c in cc = 10 || cc = 11 || cc = 13 || cc = 32

let is_dot c =
  let cc = ccode c in cc = 46

(* match with in_char *)

let is_in_with f inc =
  match inc with
  | Input.Char c -> f c
  | _ -> false

let is_dot_in inc =
  is_in_with is_dot inc

(* ---------- *)


let rec next inp =
  let rec parse_when f b inp =
    match Input.peek inp with
    | Input.Char c -> begin
        match c with
        | c when f c ->
           Input.consume inp;
           parse_when f (b @ [c]) inp
        | _ -> b
      end
    | Input.Eof -> b
  in
  let parse_id c inp =
    Tid (str_crt (parse_when is_alpha_num [c] inp))
  in
  let parse_iid inp =
    Tiid (str_crt (parse_when is_alpha_num [] inp))
  in
  let parse_int_or_float c inp =
    let b = parse_when is_num [c] inp in begin
        if is_dot_in (Input.peek inp) then begin
            Input.consume inp;
            Tfloat (float_crt (parse_when is_num (b @ ['.']) inp))
          end
        else
          Tint (int_crt b)
      end
  in
  match Input.next inp with
  | Input.Char c -> begin
     match c with
     | c when is_ignore c -> next inp
     | c when is_char c -> parse_id c inp
     | c when is_hash c -> parse_iid inp
     | c when is_num c -> parse_int_or_float c inp
     | _ -> Tother
    end
  | Input.Eof -> Teof
