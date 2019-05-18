
module type EQ = sig
  type t
  val eq: t -> t -> bool
end

module Graph (e: EQ) = struct
  type g = Node g list
  let new = Node []
end
