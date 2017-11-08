

module type Into = sig
  type 'a t
  type 'a state

  val init : 'a t -> 'a state

  val cons : 'a -> 'a state -> 'a state

  val stop : 'a state -> 'a t
end


module type Iter = sig
  type 'a t
  type 'a state

  val init : 'a t -> 'a state

  val step : 'a t -> ('a -> 'a state -> 'r) -> 'r -> 'a state -> 'r
end


module Collection = struct
  module Make(I : Iter)(O: Into) = struct
    let filter predicate self =
      let rec go input output =
        I.step self (fun a input' -> O.cons a output) output input
      in
      O.stop (go (I.init self) (O.init self))
  end
end


