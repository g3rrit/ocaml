
type t = Token_parser.t -> Node.t list

let rec create (p : t list) : t =

module Table = Table.Make(struct type val_t = t end)
