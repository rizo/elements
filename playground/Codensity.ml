

(* newtype Codensity f a = Codensity (forall r. (a -> f r) -> f r) *)
(* [Codensity f] is a Monad, regardless of what f is! *)
module Codensity(F : sig type 'a t end) = struct
  type 'a t = { run : 'r . ('a -> 'r F.t) -> 'r F.t }

  (* fmap f (Codensity m) = Codensity (\k -> m (k . f)) *)
  let map f m =
    { run = fun k -> m.run (k << f) }

  (* return x = Codensity (\k -> k x) *)
  let return x = { run = (fun k -> k x) }

  (* m >>= k = Codensity (\c -> runCodensity m (\a -> runCodensity (k a) c)) *)
  (* let (>>=) m k = *)
  (*   { run = (fun c -> m.run ()) } *)
end


module Cont : sig
  type ('r, 'a) t = ('a -> 'r) -> 'r

  val ( >>= ) : ('r, 'a) t -> ('a -> ('r, 'b) t) -> ('r, 'b) t

  val return : 'a -> ('r, 'a) t

  val lift : 'r -> ('r, 'a) t

  val cont : ('r -> 'r) -> ('r, unit) t

  val run : ('r, 'a) t -> ('a -> 'r)	-> 'r

  val eval : ('r, 'r) t -> 'r
end = struct
  type ('x, 'a) t = ('a -> 'x) -> 'x

  let ( >>= ) m f x = m (fun a -> f a x)

  let return x f = f x

  let lift x _ = x

  let cont f g = f (g ())

  let run m f = m f

  let eval m = m (fun x -> x)
end

module Cont_example = struct
  open Cont

  let ex1 =
   return 1 >>= fun a ->
   return 10 >>= fun b ->
   return (a + b)

  let test1 = run ex1 Int.to_string


  let ex2 =
    return 1 >>= fun a ->
    (fun fred -> fred 10) >>= fun b ->
    return (a + b)

  let test2 = run ex2 Int.to_string


  let ex3 =
    return 1 >>= fun a ->
    (fun fred -> "escape") >>= fun b ->
    return (a + b)

  let test3 = run ex3 Int.to_string
end



