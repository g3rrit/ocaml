
class virtual node = object
  method virtual foo: int -> int
end

class anode = object
  inherit node
  val num0 = 0
  method foo a = a * a + num0
end

class bnode = object
  inherit node
  val num1 = 1
  method foo a = a + a + num1
end

let get (a: int) : node =
  match a with
  | a when a < 10 -> new anode
  | a when a > 10 -> new bnode

let () =
  Printf.printf "anode %d\n" ((get 5) # foo 4);
  Printf.printf "bnode %d\n" ((get 15) # foo 4)
