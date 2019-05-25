

module Make = functor () ->
  struct
    let t = ref 10
    let add () =
      t := !t + 1
  end


module Foo = Make()
