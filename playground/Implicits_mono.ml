
module type Type0 = sig
  type t
end



module type Container = sig
  type t
  type item
end

implicit module Char = struct
  type t = char
end


implicit module Array_Container {Item : Type0} = struct
  type t = Item.t array
  type item = Item.t
end


module X : sig
  val create : {Self : Container} -> int -> Self.item -> Self.t
end = struct
  let create {Self : Container} n x =
    failwith "no"
end

open X


let array_container = create 5 'x'




