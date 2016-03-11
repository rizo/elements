
open Base

include Array

let all p a =
  let rec loop p a i j =
    i = j || (p (get a i) && loop p a (i + 1) j) in
  loop p a 0 (length a)

let copy_and_add a x =
  let a_len = length a in
  let new_a = make (a_len + 1) x in
  blit a 0 new_a 0 a_len;
  new_a

