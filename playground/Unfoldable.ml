open Kernel

(* Interface for polymorphic unary types that can be unfolded. *)
module type Unfoldable = sig
  type 'a t

  val unfold  : ('s -> ('a * 's) option) -> 's -> 'a t

  val singleton : 'a -> 'a t

  val replicate : int -> 'a -> 'a t
  (** [replicate n x] produces a container by repeating [x] n times.

      Examples:

      {[
        assert (Array.replicate 2 "a" == [|"a"; "a"; "a"|])
      ]} *)
end


module Unfoldable = struct
  module type Basic = sig
    type 'a t

    val unfold  : ('s -> ('a * 's) option) -> 's -> 'a t
  end

  module Make(B : Basic) : Unfoldable with type 'a t := 'a B.t = struct
    include B

    let replicate n x =
      let step i =
        if i <= 0 then None
        else Some (x, i - 1) in
      unfold step n

    let singleton x = replicate 1 x
  end
end


module Unfoldable_list = Unfoldable.Make(struct
    type 'a t = 'a list

    let unfold f init =
      let rec loop s r =
        match f s with
        | Some (a, s') -> loop s' (a :: r)
        | None -> r in
      loop init []
  end)


(* -- // -- *)

module type Unfoldable' = sig
  type 'a t

     (* step : ('s -> ('a -> 's -> 'r  ) -> 'r   -> 'r  ) *)
  val unfold : ('s -> ('a -> 's -> 'a t) -> 'a t -> 'a t) -> 's -> 'a t

  val singleton : 'a -> 'a t
  val replicate : int -> 'a -> 'a t
end

module Unfoldable' = struct
  module type Basic = sig
    type 'a t

    val unfold : ('s -> ('a -> 's -> 'a t) -> 'a t -> 'a t) -> 's -> 'a t
  end

  module Make(B : Basic) : Unfoldable' with type 'a t := 'a B.t = struct
    include B

    let replicate n x =
      let step i k r =
        if i <= 0 then r
        else k x (i - 1) in
      unfold step n

    let singleton x = replicate 1 x
  end
end

module Unfoldable_list' = Unfoldable'.Make(struct
    type 'a t = 'a list

    let unfold f init =
      let rec loop s r =
        f s (fun a s' -> loop s' (a :: r)) r in
      loop init []
  end)

module Unfoldable_array' = Unfoldable'.Make(struct
    type 'a t = 'a array

    let unfold f init =
      let rec loop s r =
        f s (fun a s' -> loop s' Array.(r ++ [|a|])) r in
      loop init [||]
  end)


(* -- // -- *)

module type Unfoldable'' = sig
  type 'a t
  type 'a accumulator

     (* step : ('s -> ('a -> 's -> 'r  ) -> 'r   -> 'r  ) *)
  val unfold : ('seed -> ('a -> 'seed -> 'a accumulator) -> 'a accumulator -> 'a accumulator) -> 'seed -> 'a t

  val singleton : 'a -> 'a t
  val replicate : int -> 'a -> 'a t
end

module Unfoldable'' = struct
  module type Basic = sig
    type 'a t
    type 'a accumulator

    val unfold : ('seed -> ('a -> 'seed -> 'a accumulator) -> 'a accumulator -> 'a accumulator) -> 'seed -> 'a t
  end

  module Make(B : Basic) : Unfoldable'' with type 'a t := 'a B.t = struct
    include B

    let replicate n x =
      let step i k r =
        if i <= 0 then r
        else k x (i - 1) in
      unfold step n

    let singleton x = replicate 1 x
  end
end

module Unfoldable_list'' = Unfoldable''.Make(struct
    type 'a t = 'a list

    type 'a accumulator = 'a list

    let unfold f init =
      let rec loop s r =
        f s (fun a s' -> loop s' (a :: r)) r in
      loop init []
  end)

module Unfoldable_array'' = Unfoldable''.Make(struct
    type 'a t = 'a array

    type 'a accumulator = 'a list

    let unfold f (init : 's) =
      let rec loop s r =
        f s (fun a s' -> loop s' (a :: r)) r in
      loop init [] |> Array.of_list
  end)


