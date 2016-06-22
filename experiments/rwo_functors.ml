
(* Foldable *)

module Foldable = struct

  module type Sig = sig
    type 'a t
    val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  end

  module type Extension = sig
    type 'a t
    val iter    : ('a -> unit) -> 'a t -> unit
    val length  : 'a t -> int
    val count   : ('a -> bool) -> 'a t -> int
    val for_all : ('a -> bool) -> 'a t -> bool
    val exists  : ('a -> bool) -> 'a t -> bool
  end

  module Extend(M : Sig) : (Extension with type 'a t := 'a M.t) = struct
    open M

    let iter f t =
      fold (fun () a -> f a) () t

    let length t =
      fold (fun acc _ -> acc + 1) 0 t

    let count f t =
      fold (fun count x -> count + if f x then 1 else 0) 0 t

    exception Short_circuit

    let for_all f c =
      try iter (fun x -> if not (f x) then raise Short_circuit) c; true
      with Short_circuit -> false

    let exists f c =
      try iter (fun x -> if f x then raise Short_circuit) c; false
      with Short_circuit -> true
  end
end

(* FQueue *)

module type FQueue = sig
  type 'a t

  val empty : 'a t

  val enqueue : 'a t -> 'a -> 'a t

  val dequeue : 'a t -> ('a * 'a t) option

  val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
end

module FQueue : FQueue = struct
  type 'a t = 'a list * 'a list

  let empty = ([],[])

  let enqueue (in_list, out_list) x =
    (x :: in_list,out_list)

  let dequeue (in_list, out_list) =
    match out_list with
    | hd :: tl -> Some (hd, (in_list, tl))
    | [] ->
      match List.rev in_list with
      | [] -> None
      | hd :: tl -> Some (hd, ([], tl))

  let fold f init (in_list, out_list) =
    let after_out = List.fold_left f init out_list in
    List.fold_right (fun x acc -> f acc x) in_list after_out
end

module type ExtFQueue = sig
  type 'a t
  include FQueue             with type 'a t := 'a t
  include Foldable.Extension with type 'a t := 'a t
end

module ExtFQueue : ExtFQueue = struct
  include FQueue
  include Foldable.Extend(FQueue)
end

(* InChan *)

module type IChan = sig
  type 'a t

  val init : string -> 'a t

  val fold : ('a -> string -> 'a) -> 'a -> string t -> 'a
end

module IChan : IChan = struct
  type 'a chan = in_channel
  type 'a t = { c : 'a . 'a chan }

  let init name = { c = open_in name }

  let rec fold f init chan =
    match try Some (input_line chan.c) with End_of_file -> None with
    | Some line -> fold f (f init line) chan
    | None      -> init
end

module type ExtIChan = sig
  type 'a t
  include IChan              with type 'a t := 'a t
  include Foldable.Extension with type 'a t := 'a t
end


