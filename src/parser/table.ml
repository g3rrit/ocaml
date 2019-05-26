open Base

let empty = Map.empty (module String)

module Make (Content : sig type val_t end) =
  functor () ->
  struct
    type t = Content.val_t
    let map = ref empty

    let add (id : string) (v : t) : unit =
      map := Map.set (!map) ~key:id ~data:v

    let get (id : string) : t option =
      Map.find !map id
  end
