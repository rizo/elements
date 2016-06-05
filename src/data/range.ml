
open Base

module type InputRange = sig
  type 'a t

  val is_empty : 'a t -> bool
  val front : 'a t -> 'a
  val pop_front : 'a t -> unit
end

module ArrayRange = struct
  type 'a t = 'a array

  let input = ref (Array.make 100000 42000)

  let is_empty () = Array.length !input = 0

  let front () = Array.get !input 0

  let pop_front () =
    input := Array.sub !input 1 (Array.length !input - 1)

  let fold f z =
    let r = ref z in
    while not (is_empty ()) do
      r := f !r (front ());
      pop_front ()
    done;
    !r
end

