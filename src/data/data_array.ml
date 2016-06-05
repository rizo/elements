
open Base

include Array

external unsafe_get: 'a array -> int -> 'a = "%array_unsafe_get"
external len : 'a array -> int = "%array_length"

let all p a =
  let rec loop p a i j =
    i = j || (p (get a i) && loop p a (i + 1) j) in
  loop p a 0 (length a)

let copy_and_add a x =
  let a_len = length a in
  let new_a = make (a_len + 1) x in
  blit a 0 new_a 0 a_len;
  new_a

let each f a =
  for i = 0 to length a - 1 do
    f (unsafe_get a i)
  done

let iter a =
  let next i =
    if i < len a
    then Some (Array.unsafe_get a i, i + 1)
    else None in
  (0, next)

