
class virtual node = object
  method virtual hash : int
end

module type Node_list = sig
  val nodes : node list
end

module Node_maker (Nl : Node_list) = struct
  let nl = Nl.nodes

  let class_hash = 10

  exception Type_mismatch of unit

  class t m = object
    inherit node
    method hash = class_hash

    val member : node list =
      List.map2 (fun a b -> if a#hash != b#hash then raise (Type_mismatch ()) else a) m nl

  end
end

let () =
  let T0 = Node_maker(struct let nodes = [object method hash = 1 end; object method hash = 2 end] end)
  in ()
