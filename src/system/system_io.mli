
val stdin  : in_channel
val stdout : out_channel
val stderr : out_channel

(* module Channel : sig *)
    (* type t *)
    (* type parser = Parser_combinator_todo *)

    (* val get_line : t -> string *)
    (* val put_line : string -> t *)

    (* val get : parser -> t -> string *)
(* end *)

module In_channel : sig
  type t
  val input_all : t -> string

  val fold_lines : ('a -> string -> 'a) -> 'a -> t -> 'a

  val iter_lines : (string -> unit) -> t -> unit
end

module Labels : sig
  module In_channel : sig
    type t
    val stdin  : in_channel
    val stdout : out_channel
    val stderr : out_channel

    val input_all : t -> string
    val fold_lines : f:('a -> string -> 'a) -> init:'a -> in_channel -> 'a
    val iter_lines : f:(string -> unit) -> in_channel -> unit
  end
end

