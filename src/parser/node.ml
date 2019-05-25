open Base

class virtual t = object
  method virtual get (id : string) : t
end

module Table = Tabl.Make(struct type val_t = t end)
