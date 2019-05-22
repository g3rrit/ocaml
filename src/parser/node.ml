
class virtual node = object
  method virtual parse : Input.t -> Symbol.table -> node
end


class fun_node = object
  inherit node

  method parse input st =

end
