open Base
open Util

type token =
  | Tid of string
  | Tiid of string
  | Tint of int
  | Tfloat of float
  | Tchar of char
  | Tstr of string
  | Top_em (*    !    33   exclamation mark *)
  | Top_ds (*    $    36   dollar sign *)
  | Top_pc (*    %    37   percent sign *)
  | Top_am (*    &    38   ampersand *)
  | Top_lp (*    (    40   left paranthesis *)
  | Top_rp (*    )    41   right paranthesis *)
  | Top_as (*    *    42   asterix *)
  | Top_ps (*    +    43   plus sign *)
  | Top_cm (*    ,    44   comma *)
  | Top_ms (*    -    45   minus sign *)
  | Top_dp (*    .    46   decimal point *)
  | Top_sl (*    /    47   slash *)
  | Top_cn (*    :    58   colon *)
  | Top_se (*    ;    59   semicolon *)
  | Top_lt (*    <    60   less-than sign *)
  | Top_eq (*    =    61   equal sign *)
  | Top_gt (*    >    62   greater-than sign *)
  | Top_qm (*    ?    63   question mark *)
  | Top_at (*    @    64   commercial sign *)
  | Top_ls (*    [    91   left square bracket *)
  | Top_bs (*    \    92   backslash *)
  | Top_rs (*    ]    93   right square bracket *)
  | Top_ci (*    ^    94   circumflex *)
  | Top_us (*    _    95   underscore *)
  | Top_lb (*    {    123  left brace *)
  | Top_vb (*    |    124  vertical bar *)
  | Top_rb (*    }    125  right brace *)
  | Top_td (*    ~    126  tilde *)
  | Tother
  | Teof

(* Helper Functions *)

let ccode = Char.to_int

let is_valid c =
  let cc = ccode c in (cc >= 32) && (cc <= 126)

let is_char c =
  let cc = ccode c in (cc >= 64 && cc <= 90) || (cc >= 97 && cc <= 122) || cc = 95

let is_num c =
  let cc = ccode c in (cc >= 48 && cc <= 57)

let is_alpha_num c =
  (is_char c) || (is_num c)

let is_ignore c =
  let cc = ccode c in cc = 10 || cc = 11 || cc = 13 || cc = 32

let ccompare p c =
  let cc = ccode p in
  let pp = ccode c in
  cc = pp

(* match with in_char *)

let is_in_with f inc =
  match inc with
  | Input.Char c -> f c
  | _ -> false

let ccompare_in c inc =
  is_in_with (ccompare c) inc

(* ---------- *)

(* exceptions *)

exception Invalid_token of string

let or_else inp f ex =
   match inp#peek with
  | Input.Char c -> (if f c then (inp#consume; c) else raise ex)
  | Input.Eof -> raise ex


let rec next inp =
  let rec parse_when f b inp =
    match inp#peek with
    | Input.Char c -> begin
        match c with
        | c when f c ->
           inp#consume;
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
  let parse_char inp =
    let c = or_else inp is_valid (Invalid_token "expected valid char") in
    let nc = begin
        if ccompare '\\' c then
          let c = or_else inp is_valid (Invalid_token "expected valid char") in begin
              match c with
              | 't' -> Char.unsafe_of_int 9
              | 'n' -> Char.unsafe_of_int 13
              | _ -> raise (Invalid_token "expected escaped char")
            end
        else
          c
      end
    in
    Util.ign (or_else inp (ccompare '\'') (Invalid_token "expected apostroph char"));
    Tchar nc
  in
  let parse_int_or_float c inp =
    let b = parse_when is_num [c] inp in begin
        if ccompare_in '.' (inp#peek) then begin
            inp#consume;
            Tfloat (float_crt (parse_when is_num (b @ ['.']) inp))
          end
        else
          Tint (int_crt b)
      end
  in
  match inp#next with
  | Input.Char c -> begin
     match c with
     | c when is_ignore c     -> next inp
     | c when is_char c       -> parse_id c inp
     | c when ccompare '#' c  -> parse_iid inp
     | c when is_num c        -> parse_int_or_float c inp
     | c when ccompare '\'' c -> parse_char inp
     | c when ccompare '!'  c -> Top_em
     | c when ccompare '$' c  -> Top_ds
     | c when ccompare '%' c  -> Top_pc
     | c when ccompare '&' c  -> Top_am
     | c when ccompare '(' c  -> Top_lp
     | c when ccompare ')' c  -> Top_rp
     | c when ccompare '*' c  -> Top_as
     | c when ccompare '+' c  -> Top_ps
     | c when ccompare ',' c  -> Top_cm
     | c when ccompare '-' c  -> Top_ms
     | c when ccompare '.' c  -> Top_dp
     | c when ccompare '/' c  -> Top_sl
     | c when ccompare ':' c  -> Top_cn
     | c when ccompare ';' c  -> Top_se
     | c when ccompare '<' c  -> Top_lt
     | c when ccompare '=' c  -> Top_eq
     | c when ccompare '>' c  -> Top_gt
     | c when ccompare '?' c  -> Top_qm
     | c when ccompare '@' c  -> Top_at
     | c when ccompare '[' c  -> Top_ls
     | c when ccompare '\\' c  -> Top_bs
     | c when ccompare ']' c  -> Top_rs
     | c when ccompare '^' c  -> Top_ci
     | c when ccompare '_' c  -> Top_us
     | c when ccompare '{' c  -> Top_lb
     | c when ccompare '|' c  -> Top_vb
     | c when ccompare '}' c  -> Top_rb
     | c when ccompare '~' c  -> Top_td
     | _ -> Tother
    end
  | Input.Eof -> Teof


let to_string t =
  match t with
  | Tid s -> Printf.sprintf "id: %s" s
  | Tiid s -> Printf.sprintf "iid: %s" s
  | Tint n -> Printf.sprintf "int: %d" n
  | Tfloat n -> Printf.sprintf "float: %f" n
  | Tchar c -> Printf.sprintf "char: [%c]" c
  | Tstr s -> Printf.sprintf "string: %s" s
  | Top_em -> Printf.sprintf "Top_em (!)"
  | Top_ds -> Printf.sprintf "Top_ds ($)"
  | Top_pc -> Printf.sprintf "Top_pc (%%)"
  | Top_am -> Printf.sprintf "Top_am (&)"
  | Top_lp -> Printf.sprintf "Top_lp (()"
  | Top_rp -> Printf.sprintf "Top_rp ())"
  | Top_as -> Printf.sprintf "Top_as (*)"
  | Top_ps -> Printf.sprintf "Top_ps (+)"
  | Top_cm -> Printf.sprintf "Top_cm (,)"
  | Top_ms -> Printf.sprintf "Top_ms (-)"
  | Top_dp -> Printf.sprintf "Top_dp (.)"
  | Top_sl -> Printf.sprintf "Top_sl (/)"
  | Top_cn -> Printf.sprintf "Top_cn (:)"
  | Top_se -> Printf.sprintf "Top_se (;)"
  | Top_lt -> Printf.sprintf "Top_lt (<)"
  | Top_eq -> Printf.sprintf "Top_eq (=)"
  | Top_gt -> Printf.sprintf "Top_gt (>)"
  | Top_qm -> Printf.sprintf "Top_qm (?)"
  | Top_at -> Printf.sprintf "Top_at (@)"
  | Top_ls -> Printf.sprintf "Top_ls ([)"
  | Top_bs -> Printf.sprintf "Top_bs (\\)"
  | Top_rs -> Printf.sprintf "Top_rs (])"
  | Top_ci -> Printf.sprintf "Top_ci (^)"
  | Top_us -> Printf.sprintf "Top_us (_)"
  | Top_lb -> Printf.sprintf "Top_lb ({)"
  | Top_vb -> Printf.sprintf "Top_vb (|)"
  | Top_rb -> Printf.sprintf "Top_rb (})"
  | Top_td -> Printf.sprintf "Top_td (~)"
  | Tother -> Printf.sprintf "other"
  | Teof -> Printf.sprintf "eof"
