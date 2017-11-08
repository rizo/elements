
module S = String

let bracket init (free : 'a -> unit) f =
  let x = init () in
  try
    let r = f x in
    free x;
    r
  with exn ->
    free x;
    raise exn

module Int = struct
  type t = int
end

module String = struct
  type t = string
end


module Iter = struct
  type ('a, 'cursor, 'r) t =
      {
        init : unit -> 'cursor;
        next : 'r . ('a -> 'cursor -> 'r) -> 'r -> 'cursor -> 'r;
        free : 'cursor -> unit;
      }

  type ('a, 'accumulator, 'r) fold =
      {
        init : unit -> 'accumulator;
        reduce : 'a -> 'accumulator -> 'accumulator;
        extract : 'accumulator -> 'r;
      }

  let map f iter =
    let next yield r s =
      iter.next (fun a s' -> yield (f a) s') r s in
    { iter with next }

  let from_list l =
    let next yield r s =
      match s with
      | [] -> r
      | a :: s' -> yield a s' in
    { init = (fun () -> l); next; free = ignore; }

  let into f i =
    let rec loop r s =
      i.next (fun a s' -> loop (f.reduce a r) s') r s in
    f.extract (bracket i.init i.free (loop (f.init ())))
end

open Iter

let into_list =
  { init = (fun () x -> x);
    reduce = (fun a s -> fun xs -> s (a :: xs));
    extract = (fun f -> f []) }

module type Iterable = sig
  type t
  type item
  type cursor

  val cursor : t -> cursor
  val next : t -> (item -> cursor -> 'r) -> 'r -> cursor -> 'r
end

module type Reducible = sig
  type t
  type item
  type accumulator

  val accumulator : unit -> accumulator
  val reduce : item -> accumulator -> accumulator
  val extract : accumulator -> t
end


  let each = List.iter

module List (Item : sig type t end) = struct
  type t = Item.t list
  type cursor = t
  type item = Item.t

  let cursor self = self

  let next self f r cursor =
    match cursor with
    | [] -> r
    | a :: s' -> f a s'


  type accumulator = t

  let accumulator () = []
  let reduce a acc = a :: acc
  let extract acc = List.rev acc
end

module String_Reducible = struct
  type t = string
  type item = string
  type accumulator = string

  let accumulator () = ""
  let reduce a acc = a ^ acc
  let extract acc = acc
end


module String_Reducible_Char = struct
  type t = string
  type item = char
  type accumulator = string

  let accumulator () = ""
  let reduce a acc = S.make 1 a ^ acc
  let extract acc = acc
end


module File = struct
  type t = out_channel

  type accumulator = out_channel

  let accumulator () = stdout
  let reduce a acc = output_string acc a; acc
  let extract acc = close_out acc
end

let run f i =
  let rec loop r s =
    i.next (fun a s' -> loop (f.reduce a r) s') r s in
  f.extract (bracket i.init i.free (loop (f.init ())))


let iter (type a) (type i) (type c)
    (module Self : Iterable with type t = a and type item = i and type cursor = c) (self : a) =
  { init = (fun () -> Self.cursor self);
    next = (fun yield r s -> Self.next self yield r s);
    free = ignore }


let into (type a) (type i) (module Self : Reducible with type t = a and type item = i) self iter =
  let f = { init = Self.accumulator;
      reduce = Self.reduce;
      extract = Self.extract } in
  run f iter

let string = (module String_Reducible_Char : Reducible with type t = string and type item = char)


let _ =
  iter (module List(Int)) [1; 2; 3]
  |> Iter.map ((+) 1)
  |> Iter.map (fun x -> '0')
  |> into string ""
  |> print_endline

