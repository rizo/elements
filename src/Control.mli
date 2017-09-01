
(** {1:exn Functional abstractions} *)

(** A module with an abstract monomorphic type. *)
module type Type = sig
  type t
end


(** A module with an abstract polymorphic unary type. *)
module type Type1 = sig
  type 'a t
end


(** A module with an abstract polymorphic binary type. *)
module type Type2 = sig
  type ('a, 'b) t
end


(** A module with an abstract polymorphic ternary type. *)
module type Type3 = sig
  type ('a, 'b, 'c) t
end


module type Semigroup = sig
  type t
  val append : t -> t -> t
end

module type Monoid = sig
  type t
  val empty : t
  include Semigroup with type t := t
end


(** Signature for the polymorphic unary monad types.
    Monad instances represent types that support sequential composition. *)
module type Monad = sig
  type 'a t
  (** The type of the monadic values. *)

  val return : 'a -> 'a t
  (** [return x] injects the value [x] into the monadic type. *)

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( =<< ) : ('a -> 'b t) -> 'a t -> 'b t
  val bind : ('a -> 'b t) -> 'a t -> 'b t
  (** Sequentially compose two actions, passing any value produced by the first
      as an argument to the second.

      [>>=] and [=<<] are the infix versions of bind. *)

  val ( >> ) : 'a t -> (unit -> 'b t) -> 'b t
  (** [m1 >> fun () -> m2] sequentially composes two actions, discarding any
      value produced by [m1], like sequencing operators (such as the semicolon)
      in imperative languages. *)

  val sequence : 'a t list -> 'a list t
  (** [sequence l] evaluates each monadic action in [l] from left to right, and
      collects the results. *)

  val join : 'a t t -> 'a t
  (** [join m] removes one level of monadic structure, projecting its bound
      argument into the outer level. *)
end


(** Structure defining the [Base] implementation for [Monad] signature and a
    functor to build the extended signature. *)
module Monad : sig

  (** The base implementation for the [Monad] instance used to build the
      extended version with [Monad.Make]. *)
  module type Base = sig
    type 'a t
    (** The type of the monadic values. *)

    val return : 'a -> 'a t
    (** [return x] injects the value [x] into the monadic type. *)

    val bind : ('a -> 'b t) -> 'a t -> 'b t
    (** [bind f m] sequentially composes two actions, passing any value
        produced by [m] as an argument to [f]. *)
  end

  module Make(B : Base) : (Monad with type 'a t := 'a B.t)
  (** Functor building an instance of {!Monad} given a {!Monad.Base}
      implementation. *)
end


(** Signature for the polymorphic binary monad types.
    Monad instances represent types that support sequential composition. *)
module type Monad2 = sig
  type ('a, 'x) t
  (** The type of the monadic values. *)

  val return : 'a -> ('a, 'x) t
  (** [return x] injects the value [x] into the monadic type. *)

  val ( >>= ) : ('a, 'x) t -> ('a -> ('b, 'x) t) -> ('b, 'x) t
  val ( =<< ) : ('a -> ('b, 'x) t) -> ('a, 'x) t -> ('b, 'x) t
  val bind : ('a -> ('b, 'x) t) -> ('a, 'x) t -> ('b, 'x) t
  (** Sequentially compose two actions, passing any value produced by the first
      as an argument to the second.

      [>>=] and [=<<] are the infix versions of bind. *)

  val ( >> ) : ('a, 'x) t -> ('b, 'x) t Lazy.t -> ('b, 'x) t
  (** [m1 >> fun () -> m2] sequentially composes two actions, discarding any
      value produced by [m1], like sequencing operators (such as the semicolon)
      in imperative languages. *)

  val sequence : ('a, 'x) t list -> ('a list, 'x) t
  (** [sequence l] evaluates each monadic action in [l] from left to right, and
      collects the results. *)

  val join : (('a, 'x) t, 'x) t -> ('a, 'x) t
  (** [join m] removes one level of monadic structure, projecting its bound
      argument into the outer level. *)
end


module Monad2 : sig
  (** Structure defining the [Base] implementation for [Monad2] signature and a
      functor to build the extended signature. *)

  (** The base implementation for the [Monad2] instance used to build the
      extended version with [Monad2.Make]. *)
  module type Base = sig
    type ('a, 'x) t
    (** The type of the monadic values. *)

    val return : 'a -> ('a, 'x) t
    (** [pure x] injects the value [x] into the monadic type. *)

    val bind  : ('a -> ('b, 'x) t) -> ('a, 'x) t -> ('b, 'x) t
    (** [bind f m] sequentially composes two actions, passing any value
        produced by [m] as an argument to [f]. *)
  end

  module Make(B : Base) : (Monad2 with type ('a, 'x) t := ('a, 'x) B.t)
  (** Functor building an instance of {!Monad} given a {!Monad.Base}
      implementation. *)
end


(** Signature for the polymorphic unary functor types.
    A [Functor] is a type that supports a mapping operation [map] that can
    transform values contained in the type. *)
module type Functor = sig
  type 'a t
  (** The type that can be mapped over. *)

  val map : ('a -> 'b ) -> 'a t -> 'b t
  val (<$>) : ('a -> 'b ) -> 'a t -> 'b t
  val ( |>) : 'a t -> ('a -> 'b ) -> 'b t
end


module Functor : sig
  module type Base = sig
    type 'a t

    val map : ('a -> 'b ) -> 'a t -> 'b t
  end

  module Make(B : Base) : Functor with type 'a t := 'a B.t

  module With_monad(M : Monad.Base) : Functor with type 'a t := 'a M.t
  (** Functor building an instance of {!Functor} signature given a
      {!Monad.Base} implementation. *)
end


module type Functor2 = sig
  type ('a, 'x) t

  val map : ('a -> 'b) -> ('a, 'x) t -> ('b, 'x) t
  val (<$>) : ('a -> 'b ) -> ('a, 'x) t -> ('b, 'x) t
  val ( |>) : ('a, 'x) t -> ('a -> 'b ) -> ('b, 'x) t
end


module Functor2 : sig
  module type Base = sig
    type ('a, 'x) t

    val map : ('a -> 'b) -> ('a, 'x) t -> ('b, 'x) t
  end

  module Make(B : Base) : Functor2 with type ('a, 'x) t := ('a, 'x) B.t

  module With_monad(M : Monad2.Base) : (Functor2 with type ('a, 'x) t := ('a, 'x) M.t)
  (** Functor building an instance of {!Functor2} signature given a
      {!Monad2.Base} implementation. *)
end


(** A functor with application, providing operations to embed pure expressions,
    sequence computations and combine their results. *)
module type Applicative = sig
  type 'a t

  val pure : 'a -> 'a t

  val ap : ('a -> 'b) t -> 'a t -> 'b t

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  val ( <* ) : 'a t -> 'b t -> 'a t

  val ( *> ) : 'a t -> 'b t -> 'b t

  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
end


module Applicative : sig
  module type Base = sig
    type 'a t

    val pure : 'a -> 'a t

    val ap : ('a -> 'b) t -> 'a t -> 'b t
  end

  module With_monad (M : Monad.Base) : (Applicative with type 'a t := 'a M.t)

  module Into_functor (A : Base) : (Functor.Base with type 'a t := 'a A.t)

  module Make (B : Base) : (Applicative with type 'a t := 'a B.t)
end


module type Applicative2 = sig
  type ('a, 'x) t

  val pure : 'a -> ('a, 'x) t

  val ap : ('a -> 'b, 'x) t -> ('a, 'x) t -> ('b, 'x) t

  val ( <*> ) : ('a -> 'b, 'x) t -> ('a, 'x) t -> ('b, 'x) t

  val ( <* ) : ('a, 'x) t -> ('b, 'x) t -> ('a, 'x) t

  val ( *> ) : ('a, 'x) t -> ('b, 'x) t -> ('b, 'x) t

  val lift2 : ('a -> 'b -> 'c) -> ('a, 'x) t -> ('b, 'x) t -> ('c, 'x) t
end


module Applicative2 : sig
  module type Base = sig
    type ('a, 'x) t

    val pure : 'a -> ('a, 'x) t

    val ap : ('a -> 'b, 'x) t -> ('a, 'x) t -> ('b, 'x) t
  end

  module With_monad (M : Monad2.Base) : (Applicative2 with type ('a, 'x) t := ('a, 'x) M.t)

  module Into_functor (A : Base) : (Functor2.Base with type ('a, 'x) t := ('a, 'x) A.t)

  module Make (B : Base) : (Applicative2 with type ('a, 'x) t := ('a, 'x) B.t)
end


module type Alternative = sig
  type 'a t

  val empty : 'a t
  (** [empty] is the identity of [<|>] *)

  val (<|>) : 'a t -> 'a t -> 'a t
  (** [a <|> b] in associative binary operation of [a] and [b]. *)

  val some : 'a t -> 'a list t
  (** [some a] is one or more occurrences of [a]. *)

  val many : 'a t -> 'a list t
  (** [many a] is zero or more occurrences of [a]. *)
end


module Alternative : sig
  module type Base = sig
    type 'a t

    val empty : 'a t

    val (<|>) : 'a t -> 'a t -> 'a t
  end

  module Make
      (B : Base)
      (A : Applicative with type 'a t := 'a B.t)
    : Alternative with type 'a t := 'a B.t
  (** Functor building an instance of {!Alternative} given a
      {!Alternative.Base} and {!Applicative} implementation. *)
end


module type Alternative2 = sig
  type ('a, 'x) t

  val empty : ('a, 'x) t
  (** [empty] is the identity of [<|>] *)

  val (<|>) : ('a, 'x) t -> ('a, 'x) t -> ('a, 'x) t
  (** [a <|> b] in associative binary operation of [a] and [b]. *)

  val some : ('a, 'x) t -> ('a list, 'x) t
  (** [some a] is one or more occurrences of [a]. *)

  val many : ('a, 'x) t -> ('a list, 'x) t
  (** [many a] is zero or more occurrences of [a]. *)
end


module Alternative2 : sig
  module type Base = sig
    type ('a, 'x) t

    val empty : ('a, 'x) t

    val (<|>) : ('a, 'x) t -> ('a, 'x) t -> ('a, 'x) t
  end

  module Make
      (B : Base)
      (A : Applicative2 with type ('a, 'x) t := ('a, 'x) B.t)
    : Alternative2 with type ('a, 'x) t := ('a, 'x) B.t
  (** Functor building an instance of {!Alternative2} given a
      {!Alternative2.Base} and {!Applicative2} implementation. *)
end


module type State = sig
  type state

  type 'a t = state -> ('a * state)

  include Monad with type 'a t := 'a t

  val get : state t
  val put : state -> unit t
  val run : 'a t -> state -> 'a * state
end


module State : sig
  module Make(S : Type) : (State with type state = S.t)
end


module type State1 = sig
  type 'x state

  type ('a, 'x) t = 'x state -> ('a * 'x state)

  include Monad2 with type ('a, 'x) t := ('a, 'x) t

  val get : ('x state, 'x) t
  val put : 'x state -> (unit, 'x) t
  val run : ('a, 'x) t -> 'x state -> 'a * 'x state
end


module State1 : sig
  module Make(S : Type1) : (State1 with type 'x state = 'x S.t)
end


module type StateT = sig
  type state
  type 'a monad
  type 'a t = state -> ('a * state) monad

  include Monad with type 'a t := 'a t

  val run : 'a t -> state -> ('a * state) monad
  val get : state t
  val put : state -> unit t

  val state : (state -> ('a * state)) -> 'a t
  (** Embed a simple state action into the monad. *)

  val modify : (state -> state) -> unit t
  (* Maps an old state to a new state inside a state monad. The old state is
     thrown away. *)
end


module StateT (S : Type) (M : Monad.Base) :
  StateT with type state = S.t
          and type 'a monad = 'a M.t


module type State1T = sig
  type 'x state
  type 'a monad
  type ('a, 'x) t = 'x state -> ('a * 'x state) monad

  include Monad2 with type ('a, 'x) t := ('a, 'x) t

  val run : ('a, 'x) t -> 'x state -> ('a * 'x state) monad
  val get : ('x state, 'x) t
  val put : 'x state -> (unit, 'x) t

  val state : ('x state -> ('a * 'x state)) -> ('a, 'x) t
  (** Embed a simple state action into the monad. *)

  val modify : ('x state -> 'x state) -> (unit, 'x) t
  (* Maps an old state to a new state inside a state monad. The old state is
     thrown away. *)
end


module State1T (S : Type1) (M : Monad.Base) :
  State1T with type 'x state = 'x S.t
           and type 'a monad = 'a M.t


module IxMonad : sig
  type ('a, 'i, 'j) t = 'i -> 'a * 'j

  val return : 'a -> ('a, 'i, 'i) t
  val bind : ('a -> ('b, 'j, 'k) t) -> ('a, 'i, 'j) t -> ('b, 'i, 'k) t

  val get : ('i, 'i, 'i) t
  val put : 'j -> (unit, 'i, 'j) t
end


module Exn : sig
  type t = exn

  val raise : ?trace: bool -> exn -> 'a
  (** [raise ?trace exn] raises the exception [exn]. If [trace] is [false] no
      backtrace will be recorded, resulting in faster execution.

      {[
        let check_list l =
          if List.is_empty l then
            raise Not_found ~trace:false
          else
            print "list is not empty" in

        assert (try check_list []; false
                with Not_found -> true)
      ]} *)

  val raises : ?exn: exn -> (unit -> 'a) -> bool
  (** [raises ?exn f] is [true] if the exception [exn] is raised while calling
      [f], [false] otherwise. If no [exn] is given, will return [true] on any
      exception.

      {[
        assert (raises (fun () -> fail "yes"));
        assert (raises (fun () -> Option.force None) ~exn:No_value);
        assert (not (raises (fun () -> "no")))
      ]} *)

  val show : t -> string
  (** [show exn] is a string representation of the exception [exn]. *)
end


module Function : sig
  type ('a, 'b) t = 'a -> 'b

  val identity : 'a -> 'a

  val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

  val always : 'a -> 'b -> 'a

  val curry : (('a * 'b) -> 'c) -> 'a -> 'b -> 'c

  val uncurry : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c

  val compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  val ( << )  : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  val ( >> )  : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
end

(** Trivial identity type module. *)
module Identity : sig
  type 'a t = 'a

  include Monad   with type 'a t := 'a t
  include Functor with type 'a t := 'a t
end


