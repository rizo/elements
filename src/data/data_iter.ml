
open Base
module Option = Data_option

module Iterable = struct

  module Finite = struct
    module type Input = sig
      type 'a t
      val input : 'a t -> 'a option
    end

    module type Forward = sig
      type 'a t
      val next : 's -> ('a * 's) option
    end

    module type Bidirectional = sig
      type 'a t
      include Forward with type 'a t := 'a t
      val back : 'a t -> 'a option
    end

    module type Indexed = sig
      type 'a t
      val length : 'a t -> int
      val get    : 'a t -> int -> 'a option
    end

    module type Output = sig
      type 'a t
      val output : 'a t -> 'a -> 'a t
    end
  end

  module Infinite = struct
    module type Input = sig
      type 'a t
      val input : 'a t -> 'a
    end

    module type Forward = sig
      type 'a t
      val next : 'a t -> 'a
    end

    module type Bidirectional = sig
      type 'a t
      include Forward with type 'a t := 'a t
      val prev : 'a t -> 'a
    end

    module type Indexed = sig
      type 'a t
      val get : 'a t -> int -> 'a
    end

    module type Output = sig
      type 'a t
      val output : 'a t -> 'a option -> 'a t
    end
  end
end

module Finite_indexed(Iterable : Iterable.Finite.Indexed) = struct
  let next iterable : ('a, int) iter =
    let step i =
      match Iterable.get iterable i with
      | Some a -> Some (a, i + 1)
      | None   -> None in
    (0, step)
end


module String = Data_string

(*
 * Iterables
 *)

module type Input_iterable = sig
  type t
  val input : t -> 'a option
end

module type Output_iterable = sig
  type t
  val output : t -> 'a option -> t
end

module type Forward_iterable = sig
  type 'a t
  type state
  val init : state
  val next : 'a t -> state -> ('a * state) option
end

module type Backward_iterable = sig
  type 'a t
  type state
  val init : state
  val prev : 'a t -> state -> ('a * state) option
end

module type Fixed_iterable = sig
  type _ t
  val len : 'a t -> int
  val unsafe_get : 'a t -> int -> 'a
end


(*
 * Iterators
 *)


module type Forward = sig
  type 'a t

  type state

  val sum : int t -> int

  val len : 'a t -> int

  val product : int t -> int

  val any : ('a -> bool) -> 'a t -> bool

  val all : ('a -> bool) -> 'a t -> bool

  val last : 'a t -> 'a option

  val and_ : bool t -> bool

  val or_ : bool t -> bool

  val max : ?by: ('a -> 'a -> ordering) -> 'a t -> 'a option

  val min : ?by: ('a -> 'a -> ordering) -> 'a t -> 'a option

  val to_list : 'a t -> 'a list

  val to_rev_list : 'a t -> 'a list

  val head : 'a t -> 'a option

  (* val tail : 'a t -> 'a t *)

  val fold : ('r -> 'a -> 'r) -> 'r -> 'a t -> 'r

  val reduce : ('a -> 'a -> 'a) -> 'a t -> 'a option

  val elem : ?eq: ('a -> 'a -> bool) -> 'a -> 'a t

  val find : ('a -> bool) -> 'a t -> 'a option

  (* val get : int -> 'a t -> 'a option *)

  val find:            ('a -> bool) -> 'a t ->  'a option
  val find_index:      ('a -> bool) -> 'a t -> int option
  val find_index_list: ('a -> bool) -> 'a t -> int list

  val elem:            'a -> 'a t -> bool
  val elem_index:      'a -> 'a t -> int option
  val elem_index_list: 'a -> 'a t -> int list

  val enum : 'a t -> (int * 'a, state) iter

  val map : ('a -> 'b) -> 'a t -> ('b, state) iter
  val each : ('a -> unit) -> 'a t -> unit

  val filter : ('a -> bool) -> 'a t -> ('a, state) iter
end

module type Fixed = sig
  type 'a t
  val len : 'a t -> int
  val get : 'a t -> int -> 'a option
  val join : string -> string t -> string
  val is_empty : 'a t -> bool
end


module type Backward = sig
  type 'a t
end


(*
 * Functors
 *)

module Make_forward(Iterable : Forward_iterable) = struct
  include Iterable

  let fold f acc iterable =
    let rec loop acc s =
      match Iterable.next iterable s with
      | None         -> acc
      | Some (a, s') -> loop (f acc a) s' in
    loop acc Iterable.init

  let reduce f iterable =
    let rec loop acc s =
      match Iterable.next iterable s with
      | None -> acc
      | Some (a, s') -> loop (f acc a) s' in
    Option.map (uncurry loop) (Iterable.next iterable Iterable.init)

  let fold_while f acc iterable =
    let rec loop acc s =
      match Iterable.next iterable s with
      | None -> acc
      | Some (a, s') ->
        begin match f acc a with
          | `Done r -> r
          | `Continue r -> loop r s'
        end in
    loop acc Iterable.init

  let each f iterable =
    let rec loop s =
      match Iterable.next iterable s with
      | None         -> ()
      | Some (a, s') -> let () = f a in loop s' in
    loop Iterable.init

  let sum this =
    fold (+) 0 this

  let sum this =
    fold (+) 0 this

  let len this =
    fold (fun r _ -> r + 1) 0 this

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
    fold (function Some a -> fun b -> if by a b = GT then Some a else Some b
                 | None   -> fun b -> Some b) None this

  let min ?(by = compare) this =
    fold (function Some a -> fun b -> if by a b = LT then Some a else Some b
                 | None   -> fun b -> Some b) None this

  let to_list this =
    List.rev (fold snoc [] this)

  let to_rev_list this =
    fold snoc [] this

  let head iterable =
    Option.map first (Iterable.next iterable Iterable.init)

  let find p this =
    fold_while
      (fun r a ->
         if p a then `Done (Some a)
         else `Continue r)
      None this

  let find_index p this =
    second begin
      fold_while
        (fun (i, none) a ->
           if p a then `Done (i, Some i)
           else `Continue (i + 1, none))
        (0, None) this
    end

  let find_index_list p this =
    List.rev @ second begin
      fold
        (fun (i, r) a ->
           if p a then (i + 1, i :: r)
           else (i + 1, r))
        (0, []) this
    end

  let elem x this =
    is_some (find (fun a -> a = x) this)

  let elem_index x this =
    find_index (fun a -> a = x) this

  let elem_index_list x this =
    find_index_list (fun a -> a = x) this

  let take n iterable =
    let next' (i, s) =
      if i <= 0 then None
      else match Iterable.next iterable s with
        | Some (a, s') -> Some (a, (i - 1, s'))
        | None -> None in
    ((n, Iterable.init), next')

  (* let get_ n this = *)
  (* second begin *)
  (* fold_while *)
  (* (fun (i, none) a -> *)
  (* if i = n then `Done (i, Some a) *)
  (* else `Continue (i + 1, none)) *)
  (* (0, None) this *)
  (* end *)

  let map f iterable =
    let next' s =
      match Iterable.next iterable s with
      | Some (a, s') -> Some (f a, s')
      | None -> None in
    (Iterable.init, next')

  let filter p iterable =
    let rec next' s =
      match Iterable.next iterable s with
      | Some (a, s') ->
        if p a then Some (a, s')
        else Iterable.next iterable s'
      | None -> None in
    (Iterable.init, next')

  let enum this =
    let i = ref (-1) in
    map (fun a -> incr i; (!i, a)) this
end

module Make_backward(Iterable : Backward_iterable) = struct
  include Iterable

  (* ... *)
end


module Make_fixed(Iterable : Fixed_iterable)
  (* : sig *)
    (* include    Fixed with type 'a t := 'a Iterable.t *)
    (* include  Forward with type 'a t := 'a Iterable.t *)
    (* include Backward with type 'a t := 'a Iterable.t *)
  (* end *)
= struct

  include Make_forward(struct
      type 'a t = 'a Iterable.t
      type state = int

      let init = 0

      let next iterable =
        let l = Iterable.len iterable in
        fun i ->
          if i < l
          then Some (Iterable.unsafe_get iterable i, i + 1)
          else None
    end)

  let fold f init iterable =
    let r = ref init in
    for i = 0 to Iterable.len iterable - 1 do
      r := f !r (Iterable.unsafe_get iterable i)
    done;
    !r

  let is_empty iterable = Iterable.len iterable = 0

  let get v n =
    let l = len v in
    if n >= l then None
    else
      let n = if n < 0 then l - n else n in
      Some (Iterable.unsafe_get v n)

  let join sep iterable =
    if is_empty iterable then ""
    else
      let fst_str = Iterable.unsafe_get iterable 0 in
      let fst_len = String.len fst_str in
      let sep_len = String.len sep in
      let len_acc = fold (fun len_acc s -> len_acc + String.len s) 0 iterable in
      let res_len = (sep_len * (Iterable.len iterable - 1)) + len_acc in
      let res_str = Bytes.create res_len in
      String.unsafe_blit fst_str 0 res_str 0 fst_len;
      let res_pos = ref fst_len in
      for i = 1 to Iterable.len iterable - 1 do
        let str     = Iterable.unsafe_get iterable i in
        let str_len = String.len str in
        String.unsafe_blit sep 0 res_str !res_pos sep_len;
        res_pos := !res_pos + sep_len;
        String.unsafe_blit str 0 res_str !res_pos str_len;
        res_pos := !res_pos + str_len
      done;
      Bytes.unsafe_to_string res_str
end


