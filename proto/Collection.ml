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
open Iter.Public

type 'a iter = 'a Iter.t


(* Should this be called sequential? For both array and list these functions
 * need to iterate the whole collection item by item, potentially stopping
 * early.
 * contains_index?
 *  - Complexity: O(n) *)
module type Iterable1 = sig
  type 'a t

  val iter : 'a t -> 'a iter

  val each : ('a -> unit) -> 'a t -> unit

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

  (* TODO: Should this return an iter? *)
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

  val find_min : ?by:('a -> 'a -> order) -> 'a t -> 'a option

  val find_max : ?by:('a -> 'a -> order) -> 'a t -> 'a option

  val sum : int t -> int
  (** [sum self] sums all integers in [self]. *)

  val product : int t -> int
  (** [product self] multiplies all integers in [self]. *)

  val contains : 'a -> 'a t -> bool
  (** [contains x self] is equivalent to [is_some (find ((=) x) self)].

      {i Note}: This function relies on the polymorphic equality function {!Kernel.equal}.

      {[
        assert (contains 'x' ['a'; 'b'; 'x'] = true);
        assert (contains 'x' ['a'; 'b'; 'd'] = false);
      ]} *)


  val count : ('a -> bool) -> 'a t -> int
  (** [count predicate self] is [length (find predicate self)] but computed in
      one go.

      {[
        assert (count (fun a -> a < 0) [1; -2; 3; -4; 5; 6] = 2);
      ]} *)

  val fold : ('a -> 'r -> 'r) -> 'r -> 'a t -> 'r

  val fold_while : ('a -> 'b -> [< `Continue of 'b | `Stop of 'b ]) -> 'b -> 'a t -> 'b
  (** [fold_while predicate f b self] is like [fold] but stops the execution when
      [predicate] returns [false] on an item from [self].

      {[
        assert (List.fold_while (fun a b -> if a <= 3 then Continue a + b
                                  else Stop b) 0 [1; 2; 3; 4] = 6);
      ]} *)

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


module Iterable1 = struct
  module type Basic = sig
    type 'a t
    type 'a cursor

    val cursor : 'a t -> 'a cursor
    val next : 'a t -> ('a -> 'a cursor -> 'r) -> 'r -> 'a cursor -> 'r
  end

  module type Indexed = sig
    type 'a t

    val length : 'a t -> int
    val unsafe_get : int -> 'a t -> 'a
  end

  (* TODO: Rename to `With_Iterable`, Add `Make` for something with `iter`. *)
  (* TODO: Consider adding `stop` to Iterable. *)
  module Make(B : Basic) : Iterable1 with type 'a t := 'a B.t = struct
    let iter self =
      Iter { init = (fun () -> B.cursor self);
             next = (fun yield r s -> B.next self yield r s);
             stop = ignore }

    let each f self                 = Iter.each f (iter self)
    let fold f r self               = Iter.fold f r (iter self)
    let fold_while f r self         = Iter.fold_while f r (iter self)
    let reduce f self               = Iter.reduce f (iter self)
    let find predicate self         = Iter.find predicate (iter self)
    let find_max ?by self           = Iter.find_max ?by (iter self)
    let find_min ?by self           = Iter.find_min ?by (iter self)
    let contains x self             = Iter.contains x (iter self)
    let index ?equal x self         = Iter.index ?equal x (iter self)
    let find_index predicate self   = Iter.find_index predicate (iter self)
    let find_indices predicate self = Iter.find_indices predicate (iter self)
    let indices ?equal x self       = Iter.indices ?equal x (iter self)
    let count predicate self        = Iter.count predicate (iter self)
    let sum self                    = Iter.sum (iter self)
    let product self                = Iter.product (iter self)
    let all predicate self          = Iter.all predicate (iter self)
    let any predicate self          = Iter.any predicate (iter self)

    (* Container functions, indexed iterables override these. *)
    let is_empty self = Iter.is_empty (iter self)
    let length self   = Iter.length (iter self)
    let get n self    = Iter.get n (iter self)
    let first self    = Iter.first (iter self)
    let second self   = Iter.second (iter self)
    let last self     = Iter.last (iter self)
  end

  module With_Indexed (B : Indexed) : Iterable1 with type 'a t := 'a B.t = struct
    include Make(struct
        type 'a t = 'a B.t
        type 'a cursor = int

        let cursor _self = 0
        let next self f r cursor =
          if cursor = B.length self then r
          else f (B.unsafe_get cursor self) (cursor + 1)
      end)

    (* Container functions, overrides iterable definition. *)
    let is_empty self =
      match B.length self with
      | 0 -> true
      | _ -> false

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
end


module type Iterable0 = sig
  type t
  type item

  val iter : t -> item iter
  val find : (item -> bool) -> t -> item option
  val find_index : (item -> bool) -> t ->  int option
  val find_indices : (item -> bool) -> t -> int list
  val find_min : ?by:(item -> item -> order) -> t -> item option
  val find_max : ?by:(item -> item -> order) -> t -> item option
  val index : ?equal:(item -> item -> bool) -> item -> t -> int option
  val indices : ?equal:(item -> item -> bool) -> item -> t -> int list
  val all : (item -> bool) -> t -> bool
  val any : (item -> bool) -> t -> bool
  val reduce : (item -> item -> item) -> t -> item option
  val each : (item -> unit) -> t -> unit
  val contains : item -> t -> bool
  val count : (item -> bool) -> t -> int
  val fold_while : (item -> 'b -> [< `Continue of 'b | `Stop of 'b ]) -> 'b -> t -> 'b
  val fold : (item -> 'r -> 'r) -> 'r -> t -> 'r

  val is_empty : t -> bool
  val length : t -> int
  val get : int -> t -> item option
  val first : t -> item option
  val second : t -> item option
  val last : t -> item option
end


module Iterable0 = struct
  module type Basic = sig
    type t
    type item
    type cursor

    val init : t -> cursor
    val next : t -> (item -> cursor -> 'r) -> 'r -> cursor -> 'r
  end

  module type Indexed = sig
    type t
    type item

    val length : t -> int
    val unsafe_get : int -> t -> item
  end

  module Make(B : Basic) : Iterable0 with type t := B.t and type item := B.item = struct
    let iter self =
      Iter { init = (fun () -> B.init self);
             next = (fun yield r s -> B.next self yield r s);
             stop = ignore }

    let each f self                 = Iter.each f (iter self)
    let fold f r self               = Iter.fold f r (iter self)
    let fold_while f r self         = Iter.fold_while f r (iter self)
    let reduce f self               = Iter.reduce f (iter self)
    let find predicate self         = Iter.find predicate (iter self)
    let find_max ?by self           = Iter.find_max ?by (iter self)
    let find_min ?by self           = Iter.find_min ?by (iter self)
    let contains x self             = Iter.contains x (iter self)
    let index ?equal x self         = Iter.index ?equal x (iter self)
    let find_index predicate self   = Iter.find_index predicate (iter self)
    let find_indices predicate self = Iter.find_indices predicate (iter self)
    let indices ?equal x self       = Iter.indices ?equal x (iter self)
    let count predicate self        = Iter.count predicate (iter self)
    let all predicate self          = Iter.all predicate (iter self)
    let any predicate self          = Iter.any predicate (iter self)

    (* Container functions, indexed iterables override these. *)
    let is_empty self = Iter.is_empty (iter self)
    let length self   = Iter.length (iter self)
    let get n self    = Iter.get n (iter self)
    let first self    = Iter.first (iter self)
    let second self   = Iter.second (iter self)
    let last self     = Iter.last (iter self)
  end

  module With_Indexed (B : Indexed) : Iterable0 with type t := B.t and type item := B.item = struct
    include Make(struct
        type t = B.t
        type item = B.item
        type cursor = int

        let init _self = 0
        let next self f r cursor =
          if cursor = B.length self then r
          else f (B.unsafe_get cursor self) (cursor + 1)
      end)

    (* Container functions, overrides iterable definition. *)
    let is_empty self =
      match B.length self with
      | 0 -> true
      | _ -> false

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
end


(** An interface for types that can be constructed by reducing elements.

    A {e reducible} provides an [init]ial accumulator value, a [reduce]
    function and an [extract] function. This way a reducible defines
    an implementation of a {e fold} that can produce a container type ['a t]. *)

module type Collectable1 = sig
  type 'a t

  val empty : 'a t
  val make : int -> (int -> 'a) -> 'a t
  val singleton : 'a -> 'a t
  val replicate : int -> 'a -> 'a t
  val collect : ?into: 'a t -> 'a iter -> 'a t
end


module Collectable1 = struct
  module type Basic = sig
    type 'a t
    type 'a accumulator

    val empty : 'a t
    val accumulator : 'a t -> 'a accumulator
    val reduce : 'a -> 'a accumulator -> 'a accumulator
    val extract : 'a accumulator -> 'a t
  end

  module Make(B : Basic) : Collectable1 with type 'a t := 'a B.t = struct
    let empty = B.empty

    let unfold f seed =
      let rec loop s acc =
        f (fun a s' -> loop s' (B.reduce a acc)) acc s in
      B.extract (loop seed (B.accumulator B.empty))

    let make n f =
      let step k r count =
        if count <= 0 then r
        else k (f count) (count - 1) in
      unfold step n

    let replicate n x = make n (always x)

    let singleton x = replicate 1 x

    let collect ?into:(start = B.empty) (Iter iter) =
      let rec loop acc s =
        iter.next (fun a s' -> loop (B.reduce a acc) s') acc s in
      bracket iter.init iter.stop
        (fun s -> B.extract (loop (B.accumulator start) s))
  end
end



module type Collectable0 = sig
  type t
  type item

  val empty : t
  val make : int -> (int -> item) -> t
  val singleton : item -> t
  val replicate : int -> item -> t
  val collect : ?into: t -> item iter -> t
end


module Collectable0 = struct
  module type Basic = sig
    type t
    type item
    type accumulator

    val empty : t
    val accumulator : t -> accumulator
    val reduce : item -> accumulator -> accumulator
    val extract : accumulator -> t
  end

  module Make(B : Basic) : Collectable0 with type t := B.t and type item := B.item = struct
    let empty = B.empty

    let unfold f seed =
      let rec loop s acc =
        f (fun a s' -> loop s' (B.reduce a acc)) acc s in
      B.extract (loop seed (B.accumulator B.empty))

    let make n f =
      let step k r count =
        if count <= 0 then r
        else k (f count) (count - 1) in
      unfold step n

    let replicate n x = make n (always x)

    let singleton x = replicate 1 x

    let collect ?into:(start = B.empty) (Iter iter) =
      let rec loop acc s =
        iter.next (fun a s' -> loop (B.reduce a acc) s') acc s in
      bracket iter.init iter.stop
        (fun s -> B.extract (loop (B.accumulator start) s))
  end
end


(* Public Defaults *)
module type Iterable = Iterable1
module Iterable = Iterable1

module Collectable = Collectable1
module type Collectable = Collectable1

