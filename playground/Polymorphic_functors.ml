
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


(* -- // -- *)


module type Iterable1 = sig
  type 'a t
  val each : ('a -> unit) -> 'a t -> unit
end


module Iterable1 = struct
  module type Basic = sig
    type 'a t
    type 'a state

    val init : 'a t -> 'a state
    val next : ('a -> 'a state -> 'r) -> 'r -> 'a state -> 'a t -> 'r
  end

  module Make (B : Basic) : Iterable1 with type 'a t := 'a B.t = struct
    type 'a t = 'a B.t

    let each f self =
      let rec go s =
        B.next (fun a s' -> f a) () s self in
      go (B.init self)
  end
end


module type Iterable0 = sig
  type t
  type item

  val each : (item -> unit) -> t -> unit
end


module Iterable0 = struct
  module type Basic = sig
    type t
    type item
    type state

    val init : t -> state
    val next : (item -> state -> 'r) -> 'r -> state -> t -> 'r
  end

  module Make (B : Basic) : Iterable0 with type t := B.t and type item := B.item = struct
    type t = B.t

    (*XXX*)
    let each f self =
      let rec go s =
        B.next (fun a s' -> f a) () s self in
      go (B.init self)
  end
end


module Iterable_list = Iterable1.Make(struct
    type nonrec 'a t = 'a list
    type 'a state = 'a list

    let init self = self

    let next f r s self =
      match s with
      | [] -> r
      | a :: s' -> f a s'
  end)

module Iterable_string = Iterable0.Make(struct
    type t = string
    type item = char
    type state = int

    let init a = 0

    let next f r state self =
      if state = String.length self then r
      else f (String.unsafe_get self state) (state + 1)
  end)

