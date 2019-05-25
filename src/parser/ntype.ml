exception Type_ex of string

class node _ty _mem = object(self)
  val mem : (string * node) list = _mem
  val ty = _ty
  method get (id : string) : node =
    let(res, _) = List.find (fun a -> let (a, _) = a in a = id) self#mem in res
end

class ty _id _mem = object(self)
  val mem : (string * ty) list = m
  val id : string = _id
  method size = 10 (*depends on member *)

  method crt (args : node list) -> node =
    new node self
      (map2
         (fun a b ->
           let (x, y) = a in
           if y != b#ty
           then raise (Type_ex "unable to construct type | different type expected")
           else (x, b))
         self#mem args)
end


module Table = Table.Make(struct type val_t = t end)

(* intrinsic types *)

let ref_ty = new ty "ref" []

let list_ty = new ty "list" [("next", list_ty); ("val", ref_ty)]
