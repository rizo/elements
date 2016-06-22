
open Base

type 'a t = 'a iter

val (<$>) : ('a -> 'b) -> 'a t -> 'b t
val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
val add : 'a t -> 'a -> 'a t
val all : ('a -> bool) -> 'a t -> bool
val any : ('a -> bool) -> 'a t -> bool
val append : 'a t -> 'a t -> 'a t
val chain : 'a iter list -> 'a t
val chunks : 'a t -> int -> 'a t t
val compare : ('a -> 'a -> ordering) -> 'a t -> 'a t -> ordering
val compress : 'a t -> bool t -> 'a t
val contains : 'a t -> 'a -> bool
val count : unit -> int t
val cycle : 'a t -> 'a iter
val dedup : ?by: ('a -> 'a -> bool) -> 'a t -> 'a t
val drop : int -> 'a t -> 'a t
val drop_while : ('a -> bool) -> 'a t -> 'a t
val each : ('a -> unit) -> 'a t -> unit
val empty : 'a t
val ends_with : 'a t -> 'a t -> bool
val enumerate : ?from: int -> 'a t -> (int * 'a) t
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val filter   : ('a -> bool) -> 'a t -> 'a t
val find_all : ('a -> bool) -> 'a t -> 'a t
val select   : ('a -> bool) -> 'a t -> 'a t

val filter_map : ('a -> 'b option) -> 'a t -> 'b t

val find : ('a -> bool) -> 'a t -> 'a option
(** [find p iter] returns the first leftmost element from [iter] matching the
    predicate [p], or [None] if there is no such element. *)


val find_index : ('a -> bool) -> 'a t ->  int option
(** [find p iter] returns the index of the first leftmost element from [itr]
    matching the predicate [p], or [None] if there is no such element. *)

val find_indices : ('a -> bool) -> 'a t -> int t
(** [find_indices p iter] returns indices of all the elements from [iter]
    matching the predicate [p]. *)

val flat_map : ('a -> 'b t) -> 'a t -> 'b t
val flatten : 'a t t -> 'a t
val fold : ('r -> 'a -> 'r) -> 'r -> 'a t -> 'r
val fold_while : ('r -> 'a -> ('r, 'r) either) -> 'r -> 'a t -> 'r
val foldr : ('a -> 'r -> 'r) -> 'a t -> 'r -> 'r

val group : 'a t -> 'a t t
(** [group iter] groups consecutive elements from [iter] that are equal. *)

val group_by : ('a -> 'a -> bool) -> 'a t -> 'a t t
(** [group_by f iter] groups consecutive elements from [iter] that are equal
    according to the [f] discriminator. *)

val group_on : ('a -> 'b) -> 'a t -> 'a t t
(** [group_on f iter] groups consecutive elements from [iter] that are equal
    after applying [f].

    Equivalent to [group_by (fun a b -> f a = f b) iter] *)

val head : 'a t -> 'a option
val index : 'a -> 'a t -> int option
val indices: 'a -> 'a t -> int t
val init : int -> (int -> 'a) -> 'a t
val intersparse : 'a t -> 'a -> 'a t
val is_empty : 'a t -> bool
val iterate : ('a -> 'a) -> 'a -> 'a t
val join : string -> string t -> string
val merge : ('a -> 'b -> 'c option) -> 'a t -> 'b t -> 'c t
val last : 'a t -> 'a option
val lastn : int -> 'a t -> 'a t
val len : 'a t -> int
val map : ('a -> 'b) -> 'a t -> 'b t
val max : ?by:('a -> 'a -> ordering) -> 'a t -> 'a option
val min : ?by:('a -> 'a -> ordering) -> 'a t -> 'a option
val next : 'a t -> ('a * 'a t) option
val nth : 'a t -> int -> 'a option
val pairs : 'a t -> ('a * 'a) t
val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
val powerset : 'a t -> 'a t t
val product : int t -> int
val pure : 'a -> 'a t
val range : int -> int -> int t
val reduce : ('a -> 'a -> 'a) -> 'a t -> 'a option
val reject : ('a -> bool) -> 'a t -> 'a t
val reject_once : ('a -> bool) -> 'a t -> 'a t
val repeat : 'a -> 'a t
val repeatedly : (unit -> 'a) -> 'a t

val remove : 'a -> 'a t
(** [remove x iter] removes the first occurrence of [x] from [iter]. *)

val remove_at : int -> 'a t
(** [remove i iter] removes the element from [iter] at index [i]. *)

val reverse : 'a t -> 'a t
(** [reverse iter] reverses the elements of [iter]. Only works on finite
    iterables.

    {b Complexity:} {e O(n)} *)

val scan : ('r -> 'a -> 'r) -> 'r -> 'a t -> 'r t
val scanr : ('r -> 'a -> 'r) -> 'r -> 'a t -> 'r t
val slice : 'a t -> int -> int -> 'a t
val sort : 'a t -> 'a t
val sort_by : ('a -> 'a -> ordering) -> 'a t -> 'a t
val sort_on : ('a -> 'b) -> 'a t -> 'a t
val starts_with : 'a t -> 'a t -> bool

val combine : 'a t -> 'b t -> ('a * 'b) t
val split : ('a * 'b) t -> 'a t * 'b t

val split_at : int -> 'a t -> 'a t * 'a t
val split_while : ('a -> bool) -> 'a t -> 'a t * 'a t
val sum : int t -> int
val tail : 'a t -> 'a t
val take : int -> 'a t -> 'a t
val take_every : int -> 'a t -> 'a t
val take_while : ('a -> bool) -> 'a t -> 'a t
val to_list : 'a t -> 'a list
val unzip : ('a * 'b) t -> ('a t * 'b t)
val uniq : 'a t -> 'a t
val uniq_by : ('a -> 'a -> bool) -> 'a t -> 'a t
val zip : 'a t -> 'b t -> ('a * 'b) t
val zip_with : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t


module Iterable : sig

  module Finite   : sig end
  module Infinite : sig end

  module Indexed : sig
    type 'a t
    type index

    val range : index * index

    val index : index -> index

    val get : 'a t -> index -> 'a
  end

end





