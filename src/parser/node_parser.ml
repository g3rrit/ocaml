

module Token = Token_parser

exception Foo of unit

let rec next inp =
  let rec parse_list inp b_tok =
    if b_tok = inp#peek then
      begin
        inp#consume;
        Node.null_node
      end
    else
      Node.list_ty#crt [(parse_list inp b_tok); (next inp)]
  in
  let parser_id inp id =
    new Node.id_node id
  in
  match inp#next with
  | Token.Tid id -> parse_id inp id
  | Token.Top_lp -> parse_list_node inp Token.Top_rp
  | _ -> raise (Foo ())
