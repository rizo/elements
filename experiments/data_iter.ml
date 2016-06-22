
open Base

type 'a iter = Iter : 's * ('s -> ('a * 's) option) -> 'a iter

module type Iter = sig
  type 'a t = 'a iter
  val each   : ('a -> unit) -> 'a t -> unit
  val fold   : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  val length : 'a t -> int
  val map    : ('a -> 'b) -> 'a t -> 'b t
end

module Iter : Iter = struct
  type 'a t = 'a iter

  exception StopIteration

  let fold f init (Iter (s0, next)) =
    let rec loop acc s =
      match next s with
      | None -> acc
      | Some (a, s') -> loop (f acc a) s' in
    loop init s0

  let map f (Iter (s0, next)) =
    let next' s =
      match next s with
      | Some (a, s') -> Some (f a, s')
      | None -> None in
    Iter (s0, next')

  let each f self =
    fold (fun () a -> f a) () self

  let length t =
    fold (fun acc _ -> acc + 1) 0 t
end

module Iterable = struct
  module type Sig = sig
    type 'a t
    val iter : 'a t -> 'a iter
  end

  module type Ext = sig
    type 'a t
    val each   : ('a -> unit) -> 'a t -> unit
    val fold   : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
    val length : 'a t -> int
    val map    : ('a -> 'b) -> 'a t -> 'b iter
  end

  module Make(M : Sig) : (Ext with type 'a t := 'a M.t) = struct
    let fold f init iterable =
      Iter.fold f init (M.iter iterable)

    let map f iterable =
      Iter.map f (M.iter iterable)

    let each f iterable =
      Iter.each f (M.iter iterable)

    let length iterable =
      Iter.length (M.iter iterable)
  end
end


(* Implementations *)

(* List *)

module type List = sig
  type 'a t = 'a list

  (* List specific functions... *)

  include Iterable.Ext with type 'a t := 'a t
end

module List : List = struct
  type 'a t = 'a list

  (* List specific functions... *)
  (* ... *)

  (* Iterable *)

  let rec iter self =
    let next s =
      match s with
      | [] -> None
      | x :: xs -> Some (x, xs) in
    Iter (self, next)

  include Iterable.Make(struct
      type nonrec 'a t = 'a t
      let iter = iter
    end)
end


(* Option *)

module type Option = sig
  type 'a t = 'a option

  (* Option specific functions... *)

  include Iterable.Ext with type 'a t := 'a t
end

module Option : Option = struct
  type 'a t = 'a option

  (* Option specific functions... *)
  (* ... *)

  (* Iterable *)

  let rec iter self =
    let next s =
      match s with
      | None   -> None
      | Some x -> Some (x, None) in
    Iter (self, next)

  include Iterable.Make(struct
      type nonrec 'a t = 'a t
      let iter = iter
    end)
end

