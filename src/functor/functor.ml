

class virtual node = object
  method virtual foo : int -> int
end

module type Node_list = sig
  val l : node list
end

module type Parser = sig
  val foo : int -> int
end

module Make_parser(Nl : Node_list) : Parser = struct
  let node_list = Nl.l
  let foo a =
    List.fold_left (fun x y -> x + (y a)) 0 node_list
end

class add = object
  method foo a = a + a
end

class mul = object
  method foo a = a * a
end

module P1 = Make_parser(struct let l = [new add; new mul] end)
let () =
  Printf.printf "val: %d\n" P1.foo 2
