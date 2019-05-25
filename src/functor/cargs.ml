

class virtual t = object
  method virtual addi : int -> int
end

module Make (Container : sig type val_t end) = struct
  type vt = t
end


module Foo = Make(struct type val_t = t end)
