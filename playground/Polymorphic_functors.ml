
module type Monad2 = sig
  type ('a, 'x) t
  val return : 'a -> ('a, 'x) t
  val bind : ('a -> ('b, 'x) t) -> ('a, 'x) t -> ('b, 'x) t
  val join : (('a, 'e) t, 'e) t -> ('a, 'e) t
  val all : ('a, 'e) t list -> ('a list, 'e) t
end

module Monad2 = struct
  module type Basic = sig
    type ('a, 'e) t
    val return : 'a -> ('a, 'x) t
    val bind : ('a -> ('b, 'x) t) -> ('a, 'x) t -> ('b, 'x) t
  end

  module Make (B : Basic) : Monad2 with type ('a, 'x) t := ('a, 'x) B.t = struct
    let bind   = B.bind
    let return = B.return

    let (>>=) t f = bind f t

    let join t = t >>= fun t' -> t'

    let all =
      let rec loop vs = function
      | [] -> return (List.rev vs)
      | t :: ts -> t >>= fun v -> loop (v :: vs) ts
      in
      fun ts -> loop [] ts
  end
end


module type Monad1 = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : ('a -> 'b t) -> 'a t -> 'b t
  val join : 'a t t -> 'a t
  val all : 'a t list -> 'a list t
end


module Monad1 = struct
  module type Basic = sig
    type 'a t
    val return : 'a -> 'a t
    val bind : ('a -> 'b t) -> 'a t -> 'b t
  end

  module Make (B : Basic) : Monad1 with type 'a t := 'a B.t = struct
    module Basic2 = struct
      type ('a, 'x) t = 'a B.t
      include (B : Basic with type 'a t := 'a B.t)
    end
    include Monad2.Make(Basic2)
  end
end


