
(** This module attempts to provide an uniform interface for {e collections}.

    The supported collections are:

    - Array
    - Bitmap
    - Bytes
    - Hashmap
    - Iterator
    - List
    - Map
    - Queue
    - Set
    - Stack
    - Stream
    - String
    - Vector
*)

open Kernel
open Control

(** A container type with the ability to fold on itself.

    The [Foldable] interface describes the operations that sequentially iterate
    over the elements in a parameterised unary type ['a t] and, starting with
    an initial value [init], combine the elements togather, using a provided
    function [f], into a single result. *)
module type Foldable = sig
  type 'a t

  val foldl : ('a -> 'r -> 'r) -> 'r -> 'a t -> 'r
  (** [foldl f init self] uses [f] to sequentially, from left to right, combine
      the elements of the container [self] with an accumulator value [init].

      {[
        assert (Array.foldl (+) 0 [|1; 2; 3|] == 6);
        assert (List.foldl List.add [] [1; 2; 3] == [3; 2; 1]);
      ]} *)

  val foldr : ('a -> 'r -> 'r) -> 'r -> 'a t -> 'r
  (** [foldr f init self] is the same as [foldl] but performs the folding from
      right to left.

      {[
        assert (Array.foldr (+) 0 [|1; 2; 3|] == 6);
        assert (List.foldr List.add [] [1; 2; 3] == [1; 2; 3]);
      ]} *)

  val foldk : ('a -> 'r -> ('r -> 'r) -> 'r) -> 'r -> 'a t -> 'r
  (** [foldk f init self] is the same as [foldl] but with early termination
      support. The function [f] is given a continuation argument [('r -> 'r)]
      that [f] can call to keep folding with an intermediate accumulator, or
      return the accumulator to immediately stop without consuming more
      elements.

      {[
        let count_until_0 =
          List.foldk
            (fun x count continue ->
               if x == 0 then count
               else continue (count + 1))
            0
            [5; 22; 10; 0; 4; 3; 14; 72; 92]
        in assert (count_until_0 == 3);
      ]} *)
end


module Foldable = struct
  module type Base = sig
    type 'a t

    val foldl : ('a -> 'r -> 'r) -> 'r -> 'a t -> 'r

    val foldr : ('a -> 'r -> 'r) -> 'r -> 'a t -> 'r

    val foldk : ('a -> 'r -> ('r -> 'r) -> 'r) -> 'r -> 'a t -> 'r
  end

  module Make(B : Base) : Foldable with type 'a t := 'a B.t = struct
    include B

    let find predicate =
      foldk
        (fun a b continue ->
           if predicate a then Some a
           else continue b)
        None

    (*
     ┌──────────┬──────────┬─────────────┬──────────┬──────────┬────────────┐
     │ Name     │ Time/Run │     mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
     ├──────────┼──────────┼─────────────┼──────────┼──────────┼────────────┤
     │ Baseline │ 103.17us │       2.00w │          │          │     65.16% │
     │ Foldk    │ 158.35us │ 150_010.43w │    4.58w │    4.58w │    100.00% │
     │ View     │  89.51us │       2.00w │          │          │     56.52% │
     └──────────┴──────────┴─────────────┴──────────┴──────────┴────────────┘
     *)
    let find_index predicate self =
      let state =
        foldk
          (fun a (i, res) k ->
             if predicate a then (i, Some i)
             else k (i + 1, res))
          (0, None)
          self in
      second state
  end
end



(* Should this be called sequential? For both array and list these functions
 * need to iterate the whole collection item by item, potentially stopping
 * early.
 *  - Complexity: O(n)
 *)
module type Iterable = sig
  type 'a t

  val find: ('a -> bool) -> 'a t -> 'a option
  (** [find predicate self] returns the first leftmost element from [self]
      matching a given [predicate], or [None] if there is no such element.

      {[
        assert (List.find (fun a -> a < 0) [42; 21; 53; -2; 32] == Some (-2));
        assert (List.find (fun a -> a < 0) [42; 32; 21; 56; 34] == None);
      ]} *)

  val find_index : ('a -> bool) -> 'a t ->  int option
  (** [find_index predicate self] returns the index of the first leftmost
      element from [self] matching a given [predicate], or [None] if there is
      no such element.

      {[
        assert (List.find (fun a -> a < 0) [42; 21; 53; -2; 32] == Some 3);
        assert (List.find (fun a -> a < 0) [42; 32; 21; 56; 34] == None);
      ]} *)

  val find_indices : ('a -> bool) -> 'a t -> int list
  (** [find_indices p self] returns indices of all the elements from [self]
      matching the predicate [p]. *)

  val index : ?equal:('a -> 'a -> bool) -> 'a -> 'a t -> int option
  (** [index x self] searches for the item [x] in [self] and returns its index.

      {[
        assert (List.index 'b' ['a'; 'b'; 'c'; 'd'; 'e'] == Some 1);
        assert (List.index 'x' ['a'; 'b'; 'c'; 'd'; 'e'] == None);
      ]} *)

  val indices : ?equal:('a -> 'a -> bool) -> 'a -> 'a t -> int list
  (** [indices x self] searches for the item [x] in [self] and returns all its
      indices.

      {[
        assert (List.indices 'b' ['a'; 'b'; 'c'; 'b'; 'e'] == [1; 3]);
        assert (List.indices 'x' ['a'; 'b'; 'c'; 'd'; 'e'] == []);
      ]} *)

  val all : ('a -> bool) -> 'a t -> bool
  (** [all pred self] is [true] if all the elements from [self] match the
      predicate [pred]. *)

  val any : ('a -> bool) -> 'a t -> bool
  (** [any ppred self] is [true] if at least one element from [self] matches the
      predicate [pred]. *)

  val reduce : ('a -> 'a -> 'a) -> 'a t -> 'a option

  val min : ?by:('a -> 'a -> order) -> 'a t -> 'a option

  val max : ?by:('a -> 'a -> order) -> 'a t -> 'a option

  val sum : int t -> int
  (** [sum self] sums all integers in [self]. *)

  val product : int t -> int
  (** [product self] multiplies all integers in [self]. *)

  val to_list : 'a t -> 'a list

  val each : ('a -> unit) -> 'a t -> unit

  val contains : 'a -> 'a t -> bool
  (** [contains x self] is equivalent to [is_some (find ((=) x) self)].

      {[
        assert (contains 'x' ['a'; 'b'; 'x'] = true);
        assert (contains 'x' ['a'; 'b'; 'd'] = false);
      ]} *)


  (* Foldable? *)
  val count : ('a -> bool) -> 'a t -> int
  (** [count predicate self] is [length (find predicate self)] but computed in
      one go.

      {[
        assert (count (fun a -> a < 0) [1; -2; 3; -4; 5; 6] == 2);
      ]} *)

  val fold_while : ('a -> 'b -> [< `Continue of 'b | `Stop of 'b ]) -> 'b -> 'a t -> 'b
  (** [fold_while predicate f b self] is like [fold] but stops the execution when
      [predicate] returns [false] on an item from [self].

      {[
        assert (List.fold_while (fun a b -> if a <= 3 then Continue a + b
                                            else Stop b) 0 [1; 2; 3; 4] = 6);
      ]} *)

  (* include Foldable with type 'a t := 'a t *)
end


module Iterable = struct
  module type Base = sig
    type 'a t
    type 'a state

    val init : 'a t -> 'a state
    val next : ('a -> 'a state -> 'r) -> 'r -> 'a state -> 'a t -> 'r
  end

  module Make(B : Base) : Iterable with type 'a t := 'a B.t = struct
    let fold_state f r0 s0 self =
      let rec go r s =
        B.next (fun a -> go (f a r)) r s self in
      go r0 s0

    let fold f r self =
      fold_state f r (B.init self) self

    let fold_while f r0 self =
      let rec go r s =
        B.next (fun a s' ->
            match f a r with
            | `Continue r' -> go r' s'
            | `Stop r' -> r')
          r s self in
      go r0 (B.init self)

    let each f self =
      let rec go s =
        B.next (fun a s' -> f a) () s self in
      go (B.init self)

    let reduce f self =
      B.next (fun r0 s0 -> Some (fold_state f r0 s0 self)) None (B.init self) self

    let max ?by iter =
      let cmp = by or Kernel.compare in
      let max' x y =
        match cmp x y with 1 -> x | _ -> y in
      reduce max' iter

    let min ?by iter =
      let cmp = by or Kernel.compare in
      let min' x y =
        match cmp x y with -1 -> x | _ -> y in
      reduce min' iter

    let find predicate self =
      let rec go s =
        B.next
          (fun a s' ->
             if predicate a then Some a
             else go s')
          None s self in
      go (B.init self)

    let contains x self =
      Option.is_some (find ((==) x) self)

    let find_index predicate self =
      let rec go i s =
        B.next
          (fun a s' ->
             if predicate a then Some i
             else go (i + 1) s')
          None s self in
      go 0 (B.init self)

    let index ?equal x self =
      let equal = equal or Kernel.equal in
      find_index (equal x) self

    let find_indices predicate self =
      let rec go i r s =
        B.next
          (fun a s' ->
             if predicate a then go (i + 1) (i :: r) s'
             else go (i + 1) r s')
          r s self in
      Stdlib.List.rev (go 0 [] (B.init self))

    let indices ?equal x self =
      let equal = equal or Kernel.equal in
      find_indices (equal x) self

    let count predicate self =
      let rec go n s =
        B.next
          (fun a s' ->
             if predicate a then go (n + 1) s'
             else go n s')
          n s self in
      go 0 (B.init self)

    let sum self = fold ( + ) 0 self

    let product self =
      let rec go r s =
        B.next
          (fun a s' ->
             if Int.(a == 0) then 0
             else go (a * r) s')
          r s self in
      go 1 (B.init self)

    let all p self =
      let rec go s =
        B.next
          (fun a s' ->
             if p a then go s'
             else false)
          true s self in
      go (B.init self)

    let any p self =
      let rec go s =
        B.next
          (fun a s' ->
             if p a then go s'
             else true)
          false s self in
      go (B.init self)

    let to_list_reversed self =
      fold (fun x xs -> x :: xs) [] self

    let to_list self =
      Stdlib.List.rev (to_list_reversed self)
  end
end


module type Iterable0 = sig
  type t
  type item

  val find: (item -> bool) -> t -> item option
  val find_index : (item -> bool) -> t ->  int option
  val find_indices : (item -> bool) -> t -> int list
  val index : ?equal:(item -> item -> bool) -> item -> t -> int option
  val indices : ?equal:(item -> item -> bool) -> item -> t -> int list
  val all : (item -> bool) -> t -> bool
  val any : (item -> bool) -> t -> bool
  val reduce : (item -> item -> item) -> t -> item option
  val min : ?by:(item -> item -> order) -> t -> item option
  val max : ?by:(item -> item -> order) -> t -> item option
  val to_list : t -> item list
  val each : (item -> unit) -> t -> unit
  val contains : item -> t -> bool
  val count : (item -> bool) -> t -> int
  val fold_while : (item -> 'b -> [< `Continue of 'b | `Stop of 'b ]) -> 'b -> t -> 'b
end


module Iterable0 = struct
  module type Base = sig
    type t
    type item
    type state

    val init : t -> state
    val next : (item -> state -> 'r) -> 'r -> state -> t -> 'r
  end

  module Make(B : Base) : Iterable0 with type t := B.t and type item := B.item = struct
    let fold_state f r0 s0 self =
      let rec go r s =
        B.next (fun a -> go (f a r)) r s self in
      go r0 s0

    let fold f r self =
      fold_state f r (B.init self) self

    let fold_while f r0 self =
      let rec go r s =
        B.next (fun a s' ->
            match f a r with
            | `Continue r' -> go r' s'
            | `Stop r' -> r')
          r s self in
      go r0 (B.init self)

    let each f self =
      let rec go s =
        B.next (fun a s' -> f a) () s self in
      go (B.init self)

    let reduce f self =
      B.next (fun r0 s0 -> Some (fold_state f r0 s0 self)) None (B.init self) self

    let max ?by iter =
      let cmp = by or Kernel.compare in
      let max' x y =
        match cmp x y with 1 -> x | _ -> y in
      reduce max' iter

    let min ?by iter =
      let cmp = by or Kernel.compare in
      let min' x y =
        match cmp x y with -1 -> x | _ -> y in
      reduce min' iter

    let find predicate self =
      let rec go s =
        B.next
          (fun a s' ->
             if predicate a then Some a
             else go s')
          None s self in
      go (B.init self)

    let contains x self =
      Option.is_some (find ((==) x) self)

    let find_index predicate self =
      let rec go i s =
        B.next
          (fun a s' ->
             if predicate a then Some i
             else go (i + 1) s')
          None s self in
      go 0 (B.init self)

    let index ?equal x self =
      let equal = equal or Kernel.equal in
      find_index (equal x) self

    let find_indices predicate self =
      let rec go i r s =
        B.next
          (fun a s' ->
             if predicate a then go (i + 1) (i :: r) s'
             else go (i + 1) r s')
          r s self in
      Stdlib.List.rev (go 0 [] (B.init self))

    let indices ?equal x self =
      let equal = equal or Kernel.equal in
      find_indices (equal x) self

    let count predicate self =
      let rec go n s =
        B.next
          (fun a s' ->
             if predicate a then go (n + 1) s'
             else go n s')
          n s self in
      go 0 (B.init self)

    let all p self =
      let rec go s =
        B.next
          (fun a s' ->
             if p a then go s'
             else false)
          true s self in
      go (B.init self)

    let any p self =
      let rec go s =
        B.next
          (fun a s' ->
             if p a then go s'
             else true)
          false s self in
      go (B.init self)

    let to_list_reversed self =
      fold (fun x xs -> x :: xs) [] self

    let to_list self =
      Stdlib.List.rev (to_list_reversed self)
  end
end


module Indexable = struct
  module type Base = sig
    type 'a t

    val length : 'a t -> int
    val unsafe_get : int -> 'a t -> 'a
  end
end
(* contains_index? *)

module type Container = sig
  type 'a t

  val is_empty : 'a t -> bool
  (** [is_empty self] is [true] if [self] contains no elements.

      {[
        assert (List.is_empty [] == true);
        assert (String.is_empty "abc" == false);
      ]} *)

  val length : 'a t -> int
  (** [length self] counts the number of items in the list. *)

  val get : int -> 'a t -> 'a option
  (** [get n self] gets the [n]th element from [self] or None if [n] exceeds
      the length of [self].

      {b Note:} Negative indices are interpreted counting from the end of the
      collection.

      {[
        assert (get 1 [] = None);
        assert (get 1 ["a"; "b"; "c"] = Some "b");
        assert (get (-1) ["a"; "b"; "c"] = "c");
      ]} *)

  val first : 'a t -> 'a option

  val second : 'a t -> 'a option

  val last : 'a t -> 'a option
  (** [head self] is the first item of [self] or [None] if [self] is empty.

      {[
        assert (List.head [1; 2; 3] = Some 1);
        assert (List.head [] = None);
      ]} *)
end


module Container = struct
  module With_indexable(B : Indexable.Base) : Container with type 'a t := 'a B.t = struct
    let is_empty self =
      match B.length self with
      | 0 -> true
      | n -> false

    let length = B.length

    let get i self =
      if i < B.length self then
        Some (B.unsafe_get i self)
      else
        None

    let first self  = get 0 self
    let second self = get 1 self

    let last self =
      get (length self - 1) self
  end

  module With_iterable(B : Iterable.Base) : Container with type 'a t := 'a B.t = struct
    let is_empty self =
      B.next (fun _ _ -> false) true
        (B.init self) self

    let length self =
      let rec go count s =
        B.next (fun _ s' -> go (count + 1) s') count s self in
      go 0 (B.init self)

    let get n self =
      let rec go i state =
        B.next
          (fun a state' ->
             if Int.(i == n) then Some a
             else go (i + 1) state')
          None
          state self in
      go 0 (B.init self)

    let first self  = get 0 self
    let second self = get 1 self

    let last self =
      let rec go x s =
        B.next (fun a s' -> go (Some a) s') x s self in
      go None (B.init self)
  end
end





module Indexable0 = struct
  module type Base = sig
    type t
    type item

    val length : t -> int
    val unsafe_get : int -> t -> item
  end
end


module type Container0 = sig
  type t
  type item

  val is_empty : t -> bool
  (** [is_empty self] is [true] if [self] contains no elements.

      {[
        assert (List.is_empty [] == true);
        assert (String.is_empty "abc" == false);
      ]} *)

  val length : t -> int
  (** [length self] counts the number of items in the list. *)

  val get : int -> t -> item option
  (** [get n self] gets the [n]th element from [self] or None if [n] exceeds
      the length of [self].

      {b Note:} Negative indices are interpreted counting from the end of the
      collection.

      {[
        assert (get 1 [] = None);
        assert (get 1 ["a"; "b"; "c"] = Some "b");
        assert (get (-1) ["a"; "b"; "c"] = "c");
      ]} *)

  val first : t -> item option

  val second : t -> item option

  val last : t -> item option
  (** [head self] is the first item of [self] or [None] if [self] is empty.

      {[
        assert (List.head [1; 2; 3] = Some 1);
        assert (List.head [] = None);
      ]} *)
end


module Container0 = struct
  module With_indexable(B : Indexable0.Base) : Container0 with type t := B.t and type item := B.item = struct
    let is_empty self =
      match B.length self with
      | 0 -> true
      | n -> false

    let length = B.length

    let get i self =
      if i < B.length self then
        Some (B.unsafe_get i self)
      else
        None

    let first self  = get 0 self
    let second self = get 1 self

    let last self =
      get (length self - 1) self
  end

  module With_iterable(B : Iterable.Base) : Container with type 'a t := 'a B.t = struct
    let is_empty self =
      B.next (fun _ _ -> false) true
        (B.init self) self

    let length self =
      let rec go count s =
        B.next (fun _ s' -> go (count + 1) s') count s self in
      go 0 (B.init self)

    let get n self =
      let rec go i state =
        B.next
          (fun a state' ->
             if Int.(i == n) then Some a
             else go (i + 1) state')
          None
          state self in
      go 0 (B.init self)

    let first self  = get 0 self
    let second self = get 1 self

    let last self =
      let rec go x s =
        B.next (fun a s' -> go (Some a) s') x s self in
      go None (B.init self)
  end
end

