
include String

let len = String.length

let split ?(on=' ') str =
  let rec indices acc i =
    try
      let i = succ (String.index_from str i on) in
      indices (i::acc) i
    with Not_found ->
      (String.length str + 1) :: acc
  in
  let is = indices [0] 0 in
  let rec aux acc = function
    | last::start::tl ->
      let w = String.sub str start (last - start - 1) in
      aux (w::acc) (start::tl)
    | _ -> acc
  in
  aux [] is

