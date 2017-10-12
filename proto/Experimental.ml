
open Kernel
open Control


(* Continuation-based Sequences *)

type 'a sequence = {
  sequence : 'r . 'r -> ('a -> 'a sequence -> 'r) -> 'r
} [@@unboxed]

module Sequence = struct
  type 'a t = 'a sequence

  let rec list input =
    let sequence empty yield =
      match input with
      | [] -> empty
      | a :: rest -> yield a (list rest) in
    { sequence }

  (* let rec array input = *)
  (*   let sequence empty yield = *)
  (*     if Array.length input == 0 then empty *)
  (*     else *)
  (*     | a :: rest -> yield a (list rest) in *)
  (*   { sequence } *)

  let rec fold f r { sequence } =
    sequence r (fun a rest -> fold f (f a r) rest)

  let cons x xs =
    { sequence = fun empty yield -> yield x xs }

  let empty =
    { sequence = fun empty yield -> empty }

  let rec map f input =
    input.sequence empty (fun a rest -> cons (f a) (map f rest))
end


(* unfoldr :: (b -> Maybe (a, b)) -> b -> [a] *)


module Sequentialk = struct
  module type Base = sig
    type 'a t
    val destruct : 'r -> ('a -> 'a t -> 'r) -> 'a t -> 'r
    (** [destruct empty f self] is [empty] if the sequence contains no elements
        or [f] applied to the head and the tail of [self] otherwise.

        {[
          assert ([1; 2; 3] |> destruct None (fun x xs -> Some x) = Some 1)
        ]} *)
  end
end


(*

   {e Question:} Should sequential containers be ordered? The name "sequence"
                 implies ordering maybe it is not correct.
   {e Question:} Do fold{l,r} require ordering?
 *)
module Sequential = struct
  module type Base = sig
    type 'a t

    val empty : 'a t

    val conjoin : 'a -> 'a t -> 'a t
    (** [conjoin x self] is a sequential container produced by adding the
        element [x] to [self].

        The [List] implementation for [conjoin] adds an element as a head of
        another list. This operation is also called {e cons} and is equivalent
        to [x :: xs] in OCaml.

        {e Warning.} The addition of the element may happen at different places
        depending on the concrete type.

        {[
          assert (List.conjoin 'x' ['a'; 'b'; 'c'] = ['x'; 'a'; 'b'; 'c'])
        ]} *)

    val disjoin : ('a -> 'a t -> 'b) -> 'b -> 'a t -> 'b
    (** [disjoin empty f self] is [empty] if the sequence contains no elements
        or [f] applied to an item and the rest of [self] otherwise.

        Intuitively this operation is equivalent to pattern-matching on [self].

        {e Warning.} The order of the disjoined items provided to [f] may
        differ depending on the concrete type.

        The following example illustrates how to use [view] for the [List]
        implementation to get the {e head} of the list:

        {[
          assert ([1; 2; 3] |> List.disjoin (fun x xs -> Some x) None = Some 1)
        ]} *)
  end

  module Make(B : Base) = struct
    include B

    let rec fold f r self =
      disjoin (fun x xs -> fold f (f x r) xs) r self

    let reverse self =
      fold conjoin empty self

    let rec fold_right f r self =
      fold f r (reverse self)

    let reduce f self =
      disjoin (fun a self' -> Some (fold f a self')) None self

    let length self =
      fold (fun _ count -> count + 1) 0 self

    let last self =
      fold (fun a _ -> Some a) None self

    let rec find predicate self =
      disjoin
        (fun x xs ->
           if predicate x then Some x
           else find predicate xs)
        None
        self
  end
end

module type Ordered = sig
  type 'a t
  val reverse : 'a t -> 'a t
  (** [reverse self] is [self] with the same items but in a reversed order.

      {[
        assert (List.reverse [] = []);
        assert (List.reverse [1] = [1]);
        assert (List.reverse [1; 2; 3] = [3; 2; 1]);
      ]} *)
end


module Reducible = struct
  (* let reduce f self = *)
  (*   view *)
  (*     (fun x xs -> Some (fold_left f x xs)) *)
  (*     None *)
  (*     self *)

  (* let max ?by self = *)
  (*   let compare = by or Kernel.compare in *)
  (*   let max' x y = *)
  (*     if compare x y == Comparable.greater *)
  (*     then x *)
  (*     else y in *)
  (*   reduce max' self *)

  (* let min ?by self = *)
  (*   let compare = by or Kernel.compare in *)
  (*   let min' x y = *)
  (*     if compare x y == Comparable.less *)
  (*     then x *)
  (*     else y in *)
  (*   reduce min' self *)
end



(* The slice type is meant to improve performance. On the other hand the direct
   operations are the most common ones. And iterators can be used to avoid
   memory alloc

   {e FAQ}

   Why is there no `slice` type? *)
module type Sliceable = sig
  type 'a t

  val slice : ?by: int -> int -> int -> 'a t -> 'a t

  val take : int -> 'a t -> 'a t
  (** [take n self] takes first [n] items from [self].

      @raise Invalid_argument if [n < 0].

      {[
        assert (take 3 [1; 2; 3; 4; 5; 6] == [1; 2; 3]);
      ]} *)

  (* XXX *)
  val take_every : int -> 'a t -> 'a t

  val take_last : int -> 'a t -> 'a t

  val take_while : ('a -> bool) -> 'a t -> 'a t

  val drop : int -> 'a t -> 'a t
  (** [drop n self] removes first [n] items from [self].

      @raise Invalid_argument if [n < 0].

      {[
        assert (drop 3 [1; 2; 3; 4; 5; 6] == [4; 5; 6]);
      ]} *)

  val drop_while : ('a -> bool) -> 'a t -> 'a t
end


module type Sortable = sig
  type 'a t

  val sort : 'a t -> 'a t

  val sort_by : ('a -> 'a -> int) -> 'a t -> 'a t

  val sort_on : ('a -> 'b) -> 'a t -> 'a t
end

(*
 - Decide between: append, extend, concat, combine, chain
 - function like head, tail, foldr, last, nth, reverse imply order
 - [get] and [insert] should live in Indexed or something similar.
 - (++) should be in buildable or something similar.
 - review [find], [contains], [exists], [count], [is_member].
 - review [next], [view], [cursor], etc.
 - [init] sounds too imperative (create, make?)
 - Sliceable
 - String.join : string -> string t -> string
 - A collection is [Indexed] xor [Sequential]
 *)

module type Collection = sig
  type 'a t


  val make : int -> (int -> 'a) -> 'a t
  (** [make n f] is a collection of length [n] with each item produced by
      applying [f] to its index.

      {[
        assert (List.make (-5) identity == [4; 3; 2; 1; 0]);
        assert (String.make 26 (fun i -> char (i + 65)) == "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
      ]} *)


  val add : 'a -> 'a t -> 'a t
  (** [add x self] is a collection formed by adding the item [x] to
      the collection [self] at its end.

      Note: The [add] function on lists is commonly known as {e cons}.

      {[
        assert (List.add 0 [1; 2; 3] = [0; 1; 2; 3]);
        assert (List.add 42 [] = [42]);
      ]} *)

  val chunks : int -> 'a t -> 'a t t
  (** [chunks n self] splits [self] into chunks of size [n] producing
      a nested collection.

      {e Note:} the last chunk may have less then [n] elements.

      @raise Invalid_argument, if [n <= 0]

      {[
        assert (chunks 3 [1; 2; 3; 4; 5; 6; 7] == [[1; 2; 3]; [4; 5; 6]; [7]]);
        assert (chunks 1 [1; 2; 3; 4; 5; 6; 7] == [[1]; [2]; [3]; [4]; [5]; [6]; [7]]);
        assert (raises ~only:(Invalid_argument "chunk size <= 0")
                  (fun () -> chunks 0 [1; 2; 3]));
      ]} *)

  val concat : 'a t list -> 'a t
  (** [concat list] is a collection produced by sequentially concatenating all
      sub-collections contained in [list]. Tail recursive over outer and inner
      lists.

      {[
        assert (concat [[1; 2; 3]; [4; 5; 6]] == [1; 2; 3; 4; 5; 6]);
        assert (concat [[]; [3]; [42; 101; 50]; []] == [3; 42; 101; 50]);
      ]} *)

  val dedup : ?by: ('a -> 'a -> bool) -> 'a t -> 'a t
  (** [dedug ?by:equal self] deduplicate the collection [self], {e i.e.} remove
      duplicate elements, by applying the equality function [equal]. *)

  val each : ('a -> unit) -> 'a t -> unit

  val ends_with : 'a t -> 'a t -> bool
  (* add suffix, prefix *)

  val filter : ('a -> bool) -> 'a t -> 'a t

  val filter_map : ('a -> 'b option) -> 'a t -> 'b t

  (* Iterable *)

  (* Sequential O(n), early termination *)
  val find: ('a -> bool) -> 'a t -> 'a option
  (** [find p self] returns the first leftmost element from [self] matching the
      predicate [p], or [None] if there is no such element. *)

  (* Sequential O(n), early termination *)
  val find_index : ('a -> bool) -> 'a t ->  int option
  (** [find p self] returns the index of the first leftmost element from [itr]
      matching the predicate [p], or [None] if there is no such element. *)

  (* Sequential O(n), full scan *)
  val find_indices : ('a -> bool) -> 'a t -> int t
  (** [find_indices p self] returns indices of all the elements from [self]
      matching the predicate [p]. *)

  (* Applies to both Sequential and Indexed *)
  (* Needs to iterate on SOME items *)
  (* Tree implementations can be faster than O(n) *)
  (* Alternative names: first_index *)
  val index : 'a -> 'a t -> int option
  (** [index x self] returns the index of the first leftmost element from [self]
      equal to [x], or [None] if there is no such element. *)
  (** [index x self] searches for the item [x] in [self] and returns its index.

      {[
        assert (List.index 'b' ['a'; 'b'; 'c'; 'd'; 'e'] == Some 1);
        assert (List.index 'x' ['a'; 'b'; 'c'; 'd'; 'e'] == None);
      ]} *)

  (* Needs to iterate on ALL items *)
  (* Alternative names: all_indices *)
  val indices : 'a -> 'a t -> int t
  (* val indices: 'a -> 'a t -> int t *)
  (** [indices x self] returns indices of all the elements from [self] equal to
      [x]. *)
  (** [indices x self] searches for the item [x] in [self] and returns all its
      indices.

      {[
        assert (List.indices 'b' ['a'; 'b'; 'c'; 'b'; 'e'] == [1; 3]);
        assert (List.indices 'x' ['a'; 'b'; 'c'; 'd'; 'e'] == []);
      ]} *)

  val reduce : ('a -> 'a -> 'a) -> 'a t -> 'a option
  (** [reduce f self] reduces [self] to a single value using [f] to combine every
      element with the previous result, starting with the first element. Returns
      [None] is the foldable is empty. *)

  val min : ?by:('a -> 'a -> order) -> 'a t -> 'a option

  val max : ?by:('a -> 'a -> order) -> 'a t -> 'a option

  val contains : 'a -> 'a t -> bool
  (** [contains x self] is equivalent to [is_some (find ((=) x) self)].

      {[
        assert (contains 'x' ['a'; 'b'; 'x'] = true);
        assert (contains 'x' ['a'; 'b'; 'd'] = false);
      ]} *)

  val head : 'a t -> 'a option
  (** [head self] is the first item of [self] or [None] if [self] is empty.

      {[
        assert (List.head [1; 2; 3] = Some 1);
        assert (List.head [] = None);
      ]} *)


  (* Foldable? *)
  val count : ('a -> bool) -> 'a t -> int
  (** [count predicate self] is [length (find predicate self)] but computed in
      one go.

      {[
        assert (count (fun a -> a < 0) [1; -2; 3; -4; 5; 6] == 2);
      ]} *)

  val fold_while : ('a -> bool) -> ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b
  (** [fold_while predicate f b self] is like [fold] but stops the execution when
      [predicate] returns [false] on an item from [self].

      {[
        assert (List.fold_while (fun a -> a <= 3) (+) 0 [1; 2; 3; 4] = 6);
        assert (List.fold_while (fun a -> a != 4) [] [1; 2; 3; 4; 5] = [3; 2; 1])
      ]} *)


  val flat_map : ('a -> 'b t) -> 'a t -> 'b t


  (* Container *)
  (* Indexed XOR Sequential *)
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

  val group : 'a t -> 'a list t

  val group_by : ('a -> 'a -> bool) -> 'a t -> 'a list t

  (* Should this take [?equal b b]? *)
  val group_on : ('a -> 'b) -> 'a t -> 'a list t


  val insert : 'a -> int -> 'a t -> 'a t
  (** [insert i x self] inserts the item [x] at index [i]. *)

  val intersparse : 'a -> 'a t -> 'a t
  (** [intersperse item self] `intersperses' the [item] between the items of
      [self].

      {[
        assert (String.intersperse "abcd" ',' == "a,b,c,d")
      ]} *)

  (* Container *)
  val is_empty : 'a t -> bool
  (** [is_empty self] is [true] if [self] contains no elements.

      {[
        assert (List.is_empty [] == true);
        assert (List.is_empty [1; 2; 3] == false);
      ]} *)

  (* Indexed | Sequential | Foldable | Container!! *)
  val length : 'a t -> int
  (** [length self] counts the number of items in the list.

      {e Complexity:} O(n) *)

  (* Foldable | Indexed | Sequential | Ordered *)
  val last : 'a t -> 'a option

  val merge : ('a -> 'b -> 'c option) -> 'a t -> 'b t -> 'c t

  val of_array : 'a array -> 'a t

  val of_list : 'a list -> 'a t

  val pairwise : 'a t -> ('a * 'a) t

  val partition : ('a -> bool) -> 'a t -> 'a t * 'a t

  val powerset : 'a t -> 'a t t


  val reject : ('a -> bool) -> 'a t -> 'a t

  val indexed : ?from: int -> 'a t -> (int * 'a) t
  (** [indexed ?from:n self] adds an index (starting from [n] to each item in
      [self].

      {[
        assert (indexed ~from:101 ['A'; 'B'; 'C']
                = [(101, 'A'); (102, 'B'); (103, 'C')])
      ]} *)

  val scan : ('r -> 'a -> 'r) -> 'r -> 'a t -> 'r t

  val scan_right : ('r -> 'a -> 'r) -> 'r -> 'a t -> 'r t

  val singleton : 'a -> 'a t
  (** [singleton x] is a collection with a single item [x]. *)

  (* O(n) where n is number to split at *)
  val split_at : int -> 'a t -> 'a t * 'a t

  val split_while : ('a -> bool) -> 'a t -> 'a t * 'a t

  val starts_with : 'a t -> 'a t -> bool

  val tail : 'a t -> 'a t option
  (** [tail self] is the collection without the first item or [None] if [self] is empty.

      {[
        assert (List.tail [1; 2; 3] = Some [2; 3]);
        assert (List.tail [] = None);
      ]} *)

  val to_array : 'a t -> 'a array

  (* Foldable *)
  val to_list : 'a t -> 'a list

  val view : ('a -> 'a t -> 'b) -> 'b -> 'a t -> 'b
  (** [view f empty self] is [empty] if [self] is empty, and [f] applied to the
      head and the tail of [self] otherwise.

      {[
        assert (List.view (fun x xs -> Some x) None [1; 2; 3] = Some 1);
        assert (List.view (fun x xs -> Some x) None [] = None);
      ]} *)

  val uniq : 'a t -> 'a t

  val uniq_by : ('a -> 'a -> bool) -> 'a t -> 'a t

  val unzip : ('a * 'b) t -> 'a t * 'b t

  val zip : 'a t -> 'b t -> ('a * 'b) t

  val zip_with : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  include Equatable1  with type 'a t := 'a t
  include Comparable1 with type 'a t := 'a t
  (* include Functor     with type 'a t := 'a t *)
  (* include Foldable    with type 'a t := 'a t *)
  (* include Monoid      with type 'a t := 'a t *)
  include Ordered     with type 'a t := 'a t
  include Sortable    with type 'a t := 'a t
end

