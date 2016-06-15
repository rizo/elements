
(** Global Data namespace. *)

module Array   = Data_array
module Char    = Data_char
module Counter = Data_counter
module Either  = Data_either
module Float   = Data_float
module Int     = Data_int
module List    = Data_list
module Option  = Data_option
module Map     = Data_map
module Result  = Data_result
module Stream  = Data_stream
module String  = Data_string
module Vec     = Data_persistent_vector
module Iter    = Data_iter
module Void    = Data_void
module Range   = Data_range

module Export : sig
  type void = Void.t
  (** The nonexistent data type. *)

  type 'a vec = 'a Vec.t
  (** A persistent vector type. *)

  val vec : 'a list -> 'a vec
  (** [vec l] constructs a vector of the list [l].

      {b Complexity:} {e O(n * O({!val:PersistentVector.add})) ~ O(n)}

      {b Examples:}

      {[
        let v1 = vec ['a'; 'b'; 'c'] in
        assert (Vec.len v1 = 3);
        assert (Vec.get v1 0 = Some 'a');
      ]} *)
end

