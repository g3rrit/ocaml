

module Token = Token_parser

exception Foo of unit

let rec next_node inp =
  let rec parse_list inp lst b_tok =
    if b_tok = inp#peek then
      begin
        inp#consume;
        lst
      end
    else
      parse_list inp (lst @ [next_node inp]) b_tok
  in
  let parse_id inp id =

  in
  match inp#next with
  | Token.Tid id -> parse_id inp id
  | Token.Top_lp -> parse_list_node inp [] Token.Top_rp
  | _ -> raise (Foo ())
