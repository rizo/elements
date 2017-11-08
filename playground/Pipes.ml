
type ('a, 'b, 'r) pipe =
  | Await of ('a -> ('a, 'b, 'r) pipe)
  | Yield of ('b  * ('a, 'b, 'r) pipe)
  | Ready of 'r


type ('a, 'b, 'k) tube =
  { run : 'r . await: (('a -> 'k) -> 'r)
            -> yield: ('b -> 'k -> 'r)
            -> 'r }

let await f   = { run = fun ~await ~yield -> await f   }
let yield x k = { run = fun ~await ~yield -> yield x k }


let rec (>-) p f =
    p.run ~await:(fun f' -> await (fun a -> (f' a) >- f))
          ~yield:(fun v k -> k >< f v)

and (><) a b =
  let rec go b' =
    b'.run ~await:(fun f -> a >- f)
           ~yield:(fun v k -> yield v (go k))
  in go b

(* type ('a, 's) iter = 's * ('s -> ('a * 's) option) *)
(* ('a -> 's -> 'r) -> 'r -> 's -> 'r *)


module type S = sig
  val to_file : string -> () -> 'r
  val of_file : string -> () -> 'r
end

let consumer = to_file “a”
let producer = to_file “b”


