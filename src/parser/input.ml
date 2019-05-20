open Base
open Stdio

type in_char =
  | Char of char
  | Eof

let next_char inf =
  match In_channel.input_char inf with
  | Some b -> Char b
  | None -> Eof


type input =
  {
    inf   : In_channel.t;
    n     : in_char ref;
  }

let create inf =
  {
    inf = inf;
    n   = ref (next_char inf)
  }


let next inp =
  let nc = !(inp.n) in
  (inp.n) := (next_char inp.inf);
  nc
