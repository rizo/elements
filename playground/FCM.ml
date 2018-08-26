
open Printf


module type F = sig
  type t
  val f : t -> unit
end

module type G = sig
  type t
  val g : t -> unit
end

module type A = sig
  type t

  val make : int -> t

  include F with type t := t
  include G with type t := t
end

module A : A = struct
  type t = int

  let make x = x

  let f self = printf "f: %d\n" self
  let g self = printf "g: %d\n" self
end

module B : sig
  type t

  val make : char -> t

  include F with type t := t
  include G with type t := t
end = struct
  type t = char

  let make x = x

  let f self = printf "f: %c\n" self
  let g self = printf "g: %c\n" self
end

let a = (module A : A)

let use_f (type a) (fable : (module F with type t = a)) (x : a) =
  let module F = (val fable) in
  F.f x

let use_f' (fable : (module F)) =
  42





