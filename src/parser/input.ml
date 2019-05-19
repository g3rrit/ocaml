type in_char =
  | Char of char
  | Eof

let next_char inf =
  match input_char inf with
  | b -> Char b
  | exception End_of_file -> Eof


type input =
  {
    inf   : in_channel;
    toa   : char Stack.t;
    froma : char Stack.t;
  }

let create inf =
  {
    inf = inf;
    toa = Stack.create ();
    froma = Stack.create ();
  }

let next inp =
  let nc = match Stack.pop inp.froma with
    | c -> Char c
    | exception Stack.Empty -> next_char inp.inf
  in
  match nc with
  | Eof -> Eof
  | Char c -> begin
      Stack.push c inp.toa;
      Char c
    end


let rec rewind inp n =
  match n with
  | 0 -> ()
  | n -> begin
      let c = Stack.pop inp.toa in
      Stack.push c inp.froma;
      rewind inp (n - 1)
    end
