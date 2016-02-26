
include Array

module Labels = StdLabels.Array

let add a x =
  let a_len = length a in
  let new_a = make (a_len + 1) x in
  blit a 0 new_a 0 a_len;
  new_a

