open Base
open Stdio

type in_char =
  | Char of char
  | Eof

let next_char inf =
  match In_channel.input_char inf with
  | Some b -> Char b
  | None -> Eof


class input _inf = object(self)
  val inf : In_channel.t = _inf
  val mutable cur : in_char = next_char _inf

  method next : in_char =
    let nc = cur in
    cur <- next_char inf;
    nc

  method peek : in_char =
    cur

  method consume : unit =
    let _ = self#next in ()
end
