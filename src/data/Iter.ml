
open Base

let foldl f acc (s0, next) =
  let rec loop acc s =
    match next s with
    | None         -> acc
    | Some (a, s') -> loop (f acc a) s' in
  loop acc s0


(* XXX *)
type 'a vec = 'a PersistentVector.t
type 'a seq
type 'a set

let next (init, f) = f init

module type Iterable = sig
  type 'a t
  val iter : 'a t -> ('a, 's) iter
end

module type S = sig
  type 'a t

  val sum : int t -> int

  val sum : 'a t -> int

  val product : int t -> int

  val any : ('a -> bool) -> bool t -> bool

  val all : ('a -> bool) -> bool t -> bool

  val last : 'a t -> 'a option

  val and_ : bool t -> bool

  val or_ : bool t -> bool

  val max : ?by: ('a -> 'b -> bool) -> 'a t -> 'a option

  val min : ?by: ('a -> 'b -> bool) -> 'a t -> 'a option

  val to_list : 'a t -> 'a list

  val to_rev_list : 'a t -> 'a list

  val to_seq : 'a t -> 'a seq

  val to_vec : 'a t -> 'a vec

  val to_array : 'a t -> 'a array

  val to_set : 'a t -> 'a set

  (* Requires fold_until *)

  val head : 'a t -> 'a option
  val tail : 'a t -> 'a t

  val reduce : ('a -> 'a -> 'a) -> 'a t -> 'a

  val elem : ?eq: ('a -> 'a -> bool) -> 'a -> 'a t

  val find : ('a -> bool) -> 'a t -> 'a option

  val get : int -> 'a t -> 'a option

  val find:            ('a -> bool) -> 'a t -> 'a  option
  val find_index:      ('a -> bool) -> 'a t -> int option
  val find_index_list: ('a -> bool) -> 'a t -> int list

  val elem:            'a -> 'a t -> bool
  val elem_index:      'a -> 'a t -> int option
  val elem_index_list: 'a -> 'a t -> int list

end

module Make(M : Iterable) = struct

  let fold f acc this =
    let (init, next) = M.iter this in
    let rec loop acc s =
      match next s with
      | None -> acc
      | Some (a, s') -> loop (f acc a) s' in
    loop acc init

  let reduce f this =
    let (s, next) = M.iter this in
    let rec loop acc s =
      match next s with
      | None -> acc
      | Some (a, s') -> loop (f acc a) s' in
    Option.map (uncurry loop) (next s)

  let fold_while p f acc this =
    let (init, next) = M.iter this in
    let rec loop acc s =
      match next s with
      | None -> acc | Some (a, _) when p a -> acc
      | Some (a, s') -> loop (f acc a) s' in
    loop acc init

  let sum this =
    fold (+) 0 this

  let product this =
    fold ( * ) 1 this

  let any pred this =
    fold (fun r a -> r || pred a) false this

  let all pred this =
    fold (fun r a -> r && pred a) true this

  let last this =
    fold (fun _ a -> Some a) None this

  let and_ this =
    fold (fun r a -> r && a) true this

  let or_ this =
    fold (fun r a -> r || a) true this

  let max ?(by = compare) this =
    fold (function Some a -> fun b -> if compare a b = GT then Some a else Some b
                 | None   -> fun b -> Some b) None this

  let min ?(by = compare) this =
    fold (function Some a -> fun b -> if compare a b = LT then Some a else Some b
                 | None   -> fun b -> Some b) None this

  let to_list this =
    List.rev (fold snoc [] this)

  let to_rev_list this =
    fold snoc [] this

  let to_seq this =
    fail "not implemented"

  let to_vec this =
    fail "not implemented"

  let to_array this =
    fail "not implemented"

  let to_set this =
    fail "not implemented"

  (* Requires fold_until *)

  let head this =
    let (init, next) = M.iter this in
    Option.map fst (next init)

  let elem ?(eq = (=)) a this = fail "not implemented"

  let find p this =
    fail "not implemented"
end

