
open Base

(** Persistent bit-partitioned Vector Trie.

    {b Examples:}

{[
let v1 = vec [] in
let v2 = Vec.add v1 "Lorem" in
let v3 = Vec.append_list v2 ["ipsum"; "dolor"; "sit"; "amet"] in

assert (v2 = vec ["Lorem"]);
assert (Vec.join v3 " " = "Lorem ipsum dolor sit amet");
assert (Vec.show Str.pp v1 = {|["Lorem"; "ipsum"; "dolor"; "sit"; "amet"]|});
]} *)

type 'a t
(** A persistent vector based on array mapped tries with elements of type ['a]. *)

val empty : 'a t
(** [Vec.empty] is the empty vector.

    {b Complexity:} {e O(1)}

    {b Examples:}

{[
let v1 = Vec.empty in

assert (Vec.len v1 = 0);
]} *)


val singleton : 'a -> 'a t
(** [Vec.singleton a] is a vector with a single element [a].

    {b Complexity:} {e O(1)}

    {b Examples:}

{[
let v1 = Vec.singleton "hello" in

assert (Vec.len v1 = 1);
assert (Vec.get v1 0 = Some "hello")
]} *)


(* val is_empty : 'a t -> bool *)
(** [Vec.is_empty v] is [True] if the vector [v] is empty.

    {b Complexity:} {e O(1)}

    {b Examples:}

{[
let v1 = vec [] in
let v2 = Vec.empty in
let v3 = vec [0; 1; 2; 3; 4] in

assert (Vec.is_empty v1);
assert (Vec.is_empty v2);
assert (not (Vec.is_empty v3))
]} *)


val len : 'a t -> int
(** [Vec.len v] returns the number of elements in the vector [v].

    {b Complexity:} {e O(1)}

    {b Examples:}

{[
let v1 = Vec.empty in
let v2 = vec [0; 1; 2; 3; 4] in

assert (Vec.len v1 = 0);
assert (Vec.len v2 = 5)
]} *)


val get : 'a t -> int -> 'a option
(** [Vec.get v i] returns [Some] element of the vector [v] at the index [i], or
    [None] if the index is out of bounds.

    {b Complexity:} {e O(log32(n)) ~ O(1)}

    {b Examples:}

{[
let v1 = vec ['a'; 'b'; 'c'; 'd'] in

assert (Vec.get v1 1 = Some 'b');
assert (Vec.get v1 4 = None)
]} *)


val unsafe_get : 'a t -> int -> 'a
(** [Vec.unsafe_get i v] returns the element of the vector [v] at the index
    [i].

    @raise Invalid_argument if the index is out of bounds.

    {b Complexity:} {e O(log32(n)) ~ O(1)}

    {b Examples:}

{[
let v1 = vec ['a'; 'b'; 'c'; 'd'] in

assert (Vec.unsafe_get v1 1 = 'b');
assert (try
          discard (Vec.unsafe_get v1 4);
          false
        with Invalid_argument _ -> true)
]} *)


val add : 'a t -> 'a -> 'a t
(** [Vec.add a v] adds the element [a] at the end of the vector [v].

    {b Complexity:} {e O(log32(n)) ~ O(1)}

    {b Examples:}

{[
let v1 = vec [] in
let v2 = vec ['a'; 'b'; 'c'; 'd'] in

assert (Vec.add v1 'x' = vec ['x']);
assert (Vec.get (Vec.add v2 'x') 4 = 'x')
]} *)


val of_list : 'a list -> 'a t
(** [Vec.of_list l] constructs a vector of the list [l].

    {b Complexity:} {e O(n * O({!val:add})) ~ O(n)}

    {b Examples:}

{[
let v1 = Vec.of_list ['a'; 'b'; 'c'] in

assert (Vec.len v1 = 3);
assert (Vec.get v1 0 = Some 'a')
]} *)

(* val iter : 'a t -> ('a, 's) iter *)
(** [Vec.iter v] returns an iterator for the vector [v].

    {b Complexity:} {e O(1)}

    {b Examples:}

{[
let colors = vec ["red"; "green"; "blue"] in
let iter   = Vec.iter v1 in

assert (Iter.next iter = Some ("red", 1));
]} *)

(* val fold : ('r -> 'a -> 'r) -> 'r -> 'a t -> 'r *)
(** [Vec.fold step init v] reduces the vector elements with the functin [step]
    starting with initial value [init].

    {b Complexity:} {e O(n)}

    {b Examples:}

{[
let v1 = vec [1; 2; 3; 4] in

assert (Vec.fold (+) 0 v1 = 10);
]} *)


(* val map : ('a -> 'b) -> 'a t -> ('a, int) iter *)
(** [Vec.map f v] maps the function [f] over the elements of the vector [v]
    producing a vector with transformed elements.

    {b Complexity:} {e O(n)}

    {b Examples:}

{[
let v1 = vec [1; 2; 3; 4] in

assert (Vec.map (Int.to_str) v1 = vec ["1"; "2"; "3"; "4"]);
]} *)


(* val filter : ('a -> bool) -> 'a t -> ('a, int) iter *)
(** [Vec.filter pred v] produces a new vector with elements from vector [v] for
    which the predicate [perd] returns [true].

    {b Complexity:} {e O(n)}

    {b Examples:}

{[
let v1 = vec [1; 0; 2; 0; 4; 0; 5; 0; -1; 0; -5] in

assert (Vec.filter (fun a -> a > 0) v1 = vec [1; 2; 4; 5]);
]} *)


(* val each : ('a -> unit) -> 'a t -> unit *)
(** [Vec.each f v] applies the function [f] to the elements of the vector [v]
    discarding the results. Should be used for effectful iterations.

    {b Complexity:} {e O(n)}

    {b Examples:}

{[
let v1 = vec ["a"; "b"; "c"] in

assert (Vec.each print v1 = ());
(* Output:
   a
   b
   c *)
]} *)


(* val join : string -> string t -> string *)
(** [Vec.join sep v] joins vector elements (which should be strings) together
    as a new string, inserting a separator between each.

    {b Complexity:} {e O(n?)}

    {b Examples:}

{[
let v1 = vec ["Lorem"; "ipsum"; "dolor"; "sit"; "amet"] in

assert (Vec.join " " v1 = "Lorem ipsum dolor sit amet")
]} *)


val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** Vector pretty printing formatter. *)

val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
(** [show v] converts the vector [v] to the string representation.

    {b Examples:}

{[
let v1 = [1; 1; 2; 3; 5; 8] in

assert (Vec.show Int.pp v1 = "[1; 1; 2; 3; 5; 8]")
]} *)

