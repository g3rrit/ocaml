open Base

module Smap = Map.Make(String)

module Make (Content : sig type val_t end) =
  functor () ->
  struct
    type t = Content.val_t
    let map = ref Smap.empty

    let add (id : string) (v : val_t) : unit =
      map := Smap.add id v !map

    let get (id : string) : val_t option =
      Smap.find map id
  end
