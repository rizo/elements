
module type Iterable = sig
  type 'a t
  type 'a cursor

  val cursor : 'a t -> 'a cursor
  val next : 'a t -> ('a -> 'a cursor -> 'r) -> 'r -> 'a cursor -> 'r
end

module type Reducer = sig
  type 'a t
  type 'a accumulator

  val accumulator : unit -> 'a accumulator
  val reduce : 'a -> 'a accumulator -> 'a accumulator
  val extract : 'a accumulator -> 'a t
end

module Iterable_list = struct
  type 'a t = 'a list
  type 'a cursor = 'a t

  let cursor self = self
  let next self f r cursor =
    match cursor with
    | [] -> r
    | a :: cursor' -> f a cursor'
end


module A = struct

  let rec enum_chan iter file_path =
    let rec go iter chan =
      try
        let a = input_line chan in
        match iter with
        | `Ready _ -> close_in chan; iter
        | `Await k -> go (k (Some a)) chan
      with End_of_file -> iter in
    go iter (open_in file_path)

  let run iter =
    match iter with
    | `Ready r -> Some r
    | `Await k ->
      match k None with
      | `Ready r -> Some r
      | `Await _ -> None

  let sum =
    let rec step acc input =
      match input with
      | Some a -> `Await (step (acc + a))
      | None -> `Ready acc
    in `Await (step 0)

  let map f =
    fun reduce -> fun a acc ->
      reduce (f a) acc

  let filter f =
    fun reduce -> fun a acc ->
      if f a then reduce a acc else acc
end

