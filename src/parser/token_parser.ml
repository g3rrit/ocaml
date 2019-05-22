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


let print t =
  match t with
  | Tid s -> Stdio.printf Stdio.stdio "id: %s\n" s
  | Tiid s -> Printf.printf "iid: %s\n" s
  | Tint n -> Printf.printf "int: %d\n" n
  | Tfloat n -> Printf.printf "float: %f\n" n
  | Tchar c -> Printf.printf "char: [%c]\n" c
  | Tstr s -> Printf.printf "string: %s\n" s
  | Top_em -> Printf.printf "Top_em (!)\n"
  | Top_ds -> Printf.printf "Top_ds ($)\n"
  | Top_pc -> Printf.printf "Top_pc (%)\n"
  | Top_am -> Printf.printf "Top_am (&)\n"
  | Top_lp -> Printf.printf "Top_lp (()\n"
  | Top_rp -> Printf.printf "Top_rp ())\n"
  | Top_as -> Printf.printf "Top_as (*)\n"
  | Top_ps -> Printf.printf "Top_ps (+)\n"
  | Top_cm -> Printf.printf "Top_cm (,)\n"
  | Top_ms -> Printf.printf "Top_ms (-)\n"
  | Top_dp -> Printf.printf "Top_dp (.)\n"
  | Top_sl -> Printf.printf "Top_sl (/)\n"
  | Top_cn -> Printf.printf "Top_cn (:)\n"
  | Top_se -> Printf.printf "Top_se (;)\n"
  | Top_lt -> Printf.printf "Top_lt (<)\n"
  | Top_eq -> Printf.printf "Top_eq (=)\n"
  | Top_gt -> Printf.printf "Top_gt (>)\n"
  | Top_qm -> Printf.printf "Top_qm (?)\n"
  | Top_at -> Printf.printf "Top_at (@)\n"
  | Top_ls -> Printf.printf "Top_ls ([)\n"
  | Top_bs -> Printf.printf "Top_bs (\\)\n"
  | Top_rs -> Printf.printf "Top_rs (])\n"
  | Top_ci -> Printf.printf "Top_ci (^)\n"
  | Top_us -> Printf.printf "Top_us (_)\n"
  | Top_lb -> Printf.printf "Top_lb ({)\n"
  | Top_vb -> Printf.printf "Top_vb (|)\n"
  | Top_rb -> Printf.printf "Top_rb (})\n"
  | Top_td -> Printf.printf "Top_td (~)\n"
  | Tother -> Printf.printf "other\n"
  | Teof -> Printf.printf "eof\n"
