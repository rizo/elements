(*---------------------------------------------------------------------------
   Copyright (c) 2017 Rizo Isrof. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Small and modular standard library alternative.

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Proto} *)

(** {1:cmp Comparisons and Ordering}

    This section contains the primitives for comparisons and ordering. *)

(** Nullary signature for equality comparisons which are equivalence relations.

    [Equatable.Base] is the minimal implementation of the [Equatable] instance.
    Some examples of nullary equatable types, i.e., types without parameters
    include: [Int.t], [Float.t], [Char.t], etc. *)
module type Equatable = sig
  type t
  (** The type of the equatable monomorphic values, i.e., non-polymorphic
      nullary values, such as [int], [char], etc. *)

  val equal : t -> t -> bool

  val ( == ) : t -> t -> bool

  val ( <> ) : t -> t -> bool
end


(** Structure defining the [Base] implementation for nullary [Equatable]
    signature and a functor to build the extended signature. *)
module Equatable : sig
  module type Base = sig
    type t

    val equal : t -> t -> bool
  end

  module Make(B : Base) : (Equatable with type t := B.t)
  (** Functor building an instance of {!Equatable} given a {!Equatable.Base}
      implementation. *)
end


(** Unary signature for equality comparisons which are equivalence relations.

    The extended version of [Equatable] for polymorphic types is not possible
    since the included infix functions are only useful with two arguments and
    [Equatable1] would require an extra [equal] argument for each type
    parameter.

    If desired, the generic infix functions can be used with the default
    {!equal} implementation. *)
module type Equatable1 = sig
  type 'a t

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end


(** Binary signature for equality comparisons which are equivalence relations. *)
module type Equatable2 = sig
  type ('a, 'b) t

  val equal :
    ('a -> 'a -> bool) ->
    ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
end


type order = int
(** Defines the relative ordering of two values. The relations are less than
    (order < 0), equal to (order = 0), and greater than (order > 0). *)

(** Nullary comparable signature for types that form a total order.

    [Comparable.Base] is the minimal implementation of the [Comparable]
    instance. Some examples of nullary comparable types, i.e., types with one
    parameter, include: {!Option.t}, {!List.t}, {!Array.t}, etc. *)
module type Comparable = sig
  type t

  include Equatable with type t := t

  val compare : t -> t -> order
  val ( <  ) : t -> t -> bool
  val ( >  ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
  val min : t -> t -> t
  val max : t -> t -> t
end


(** Structure defining the [Base] implementation for nullary [Comparable]
    signature and a functor to build the extended signature. *)
module Comparable : sig
  val less    : order
  val equal   : order
  val greater : order

  module type Base = sig
    type t

    val compare : t -> t -> order
  end

  module Make(B : Base) : (Comparable with type t := B.t)
  (** Functor building an instance of {!Comparable} given a {!Comparable.Base}
      implementation. *)
end


(** Unary comparable signature for types that form a total order.

    The infix functions for the polymorphic versions of [Comparable] are not
    included since it would requires extra [compare] arguments for each type
    parameter and the infix functions are only useful with two arguments.

    If desired, the generic infix functions can be used with the default
    {!compare} implementation. *)
module type Comparable1 = sig
  type 'a t

  val compare : ('a -> 'a -> order) -> 'a t -> 'a t -> order

  val min : ('a -> 'a -> order) -> 'a t -> 'a t -> 'a t

  val max : ('a -> 'a -> order) -> 'a t -> 'a t -> 'a t
end


(** Structure defining the [Base] implementation for unary [Comparable1]
    signature and a functor to build the extended signature. *)
module Comparable1 : sig
  module type Base = sig
    type 'a t

    val compare : ('a -> 'a -> order) -> 'a t -> 'a t -> order
  end

  module Make(B : Base) : (Comparable1 with type 'a t := 'a B.t)
  (** Functor building an instance of {!Comparable1} given a
      {!Comparable1.Base} implementation. *)
end


(** Binary comparable signature for types that form a total order. *)
module type Comparable2 = sig
  type ('a, 'b) t

  val compare :
    ('a -> 'a -> order) -> ('b -> 'b -> order) ->
    ('a, 'b) t -> ('a, 'b) t -> order

  val min :
    ('a -> 'a -> order) -> ('b -> 'b -> order) ->
    ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

  val max :
    ('a -> 'a -> order) -> ('b -> 'b -> order) ->
    ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
end


(** Structure defining the [Base] implementation for the binary [Comparable2]
    signature and a functor to build the extended signature. *)
module Comparable2 : sig
  module type Base = sig
    type ('a, 'b) t

    val compare :
      ('a -> 'a -> order) -> ('b -> 'b -> order) ->
      ('a, 'b) t -> ('a, 'b) t -> order
  end

  module Make(B : Base) : (Comparable2 with type ('a, 'b) t := ('a, 'b) B.t)
  (** Functor building an instance of {!Comparable2} given a
      {!Comparable2.Base} implementation. *)
end


val compare : 'a -> 'a -> order
(** [compare a b] returns [0] if [a] is equal to [b], [-1] if [a] is less than
    [b], and [1] if [a] is greater than [b].

    The ordering implemented by [compare] is compatible with the comparison
    predicates [=], [<] and [>] defined above, with one difference on the
    treatment of the float value {!nan}. Namely, the comparison predicates
    treat [nan] as different from any other float value, including itself;
    while [compare] treats [nan] as equal to itself and less than any other
    float value. This treatment of [nan] ensures that [compare] defines a
    total ordering relation.

    [compare] applied to functional values may raise [Invalid_argument].
    [compare] applied to cyclic structures may not terminate. *)

val equal : 'a -> 'a -> bool
val ( == ) : 'a -> 'a -> bool
(** [a = b] tests for structural equality of [a] and [b]. Mutable
    structures (e.g. references and arrays) are equal if and only if their
    current contents are structurally equal, even if the two mutable objects
    are not the same physical object.

    Equality between functional values raises [Invalid_argument].
    Equality between cyclic data structures may not terminate. *)

val ( <> ) : 'a -> 'a -> bool
(** [a <> b] is [not (a == b)], {e i.e.}, the negation of {!(==)}. *)

val ( <  ) : 'a -> 'a -> bool
val ( >  ) : 'a -> 'a -> bool
val ( <= ) : 'a -> 'a -> bool
val ( >= ) : 'a -> 'a -> bool
(** Structural ordering functions.

    These functions coincide with the usual orderings over integers,
    characters, strings, byte sequences and floating-point numbers, and extend
    them to a total ordering over all types.  The ordering is compatible with
    {!(==)}. As in the case of {!(==)}, mutable structures are compared by
    contents.

    Comparison between functional values raises [Invalid_argument].  Comparison
    between cyclic structures may not terminate. *)

val min : 'a -> 'a -> 'a
(** [min a b] returns the smaller of the two arguments.

    The result is unspecified if one of the arguments contains
    the float value [nan].

    {[
      assert (min 2 5 = 2);
      assert (List.reduce min [3; 4; 2; 1; 9 ; 3] = Some 1)
    ]} *)

val max : 'a -> 'a -> 'a
(** [max a b] returns the greater of the two arguments.

    The result is unspecified if one of the arguments contains
    the float value [nan].

    {[
      assert (max 2 5 = 5);
      assert (List.reduce min [3; 4; 2; 1; 9 ; 3] = Some 9)
    ]} *)

val is : 'a -> 'a -> bool
(** [is a b] tests for physical equality of [a] and [b].
    On mutable types such as references, arrays, byte sequences, records with
    mutable fields and objects with mutable instance variables,
    [is a b] is true if and only if physical modification of [a] also affects [b].

    On non-mutable types, the behavior of [is] is
    implementation-dependent; however, it is guaranteed that
    [is a b] implies [a = b]. *)


(** {1:pp Formatting and Pretty-printing}

    This section contains the primitives for formatting and pretty-printing. *)

type 'a printer = Format.formatter -> 'a -> unit
(** The type for pretty-printers of values of type ['a]. *)

val format : ('a, Format.formatter, unit, string) format4 -> 'a
(** [format fmt (arg1 ... argN)] formats the arguments [arg1] to [arnN]
    according to the format string [fmt] and returns the result as a string.

    The format string is a character string which contains two types of
    objects: plain characters, which are simply copied to the output channel,
    and conversion specifications, each of which causes conversion and printing
    of arguments. For more details see the {!Format} module.

    It is an alias for [Format.asprintf].

    {[
      assert (format "x = %d" 42 = "x = 42");
      assert (format "Hello, %s!" world = "Hello, world!");
    ]} *)

val print : ?out: out_channel -> ?endline:string -> string -> unit
(** [print ?out ?endline str] outputs the string [str] on the [out] channel
    (defaults to {!stdout}) followed by [endline] (defaults to ["\n"]).

    {[
      print "hello";
      print "hello, world" ~endline:"!!!\n";
      print (format "hello, %s!" "world");
      print ~out:stderr "hello";
    ]} *)


(** Signature for the monomorphic types that can be printed.

    [Printable.Base] is the minimal implementation of the [Printable] instance.
    Some examples of nullary printable types, i.e., types without parameters
    include: [Int.t], [Float.t], [Char.t], [Unit.t], etc. *)
module type Printable = sig
  type t
  (** The type of the printable monomorphic values, i.e., non-polymorphic
      nullary values, such as [int], [string], etc. *)

  val pp : t printer
  (** [pp] is the pretty-printer for the type {!t}. *)

  val show : t -> string
  (** [show self] converts [self] to a string representation using the {!pp}
      pretty-printer. *)

  val print : t -> unit
  (** [print self] writes [self] to the standard output using the {!pp}
      pretty-printer. *)
end


(** Structure defining the [Base] implementation for nullary [Printable]
    signature and a functor to build the extended signature. *)
module Printable : sig
  module type Base = sig
    type t
    (** The type of the printable monomorphic values, i.e., non-polymorphic
        nullary values, such as [int], [string], etc. *)

    val pp : t printer
    (** [pp] is the pretty-printer for the type {!t}. *)
  end

  module Make(B : Base) : (Printable with type t := B.t)
  (** Functor building an instance of {!Printable} given a {!Printable.Base}
      implementation. *)
end


(** Signature for the unary types that can be printed.

    [Printable1.Base] is the minimal implementation of the [Printable]
    instance. Some examples of unary printable types, i.e., types with one
    parameter, include: {!Option.t}, {!List.t}, {!Array.t}, etc. *)
module type Printable1 = sig
  type 'a t
  (** The type of the printable polymorphic values of arity one, i.e., values
      with one type parameter, such as ['a list], ['a option], etc. *)

  val pp : 'a printer -> 'a t printer
  (** [pp pp_a] is the pretty-printer for the unary type ['a t]. [pp_a] is the
      pretty-printer for the wrapped type ['a]. *)

  val show : 'a printer -> 'a t -> string
  (** [show pp_a self] converts unary {!'a t} to a string representation using
      {!pp_a} for the wrapped type. *)

  val print : 'a printer -> 'a t -> unit
  (** [print pp_a self] writes [self] to the standard output using [pp_a]
      pretty-printer for the wrapped type and {!Printable1.pp} for [self]. *)
end


(** Structure defining the [Base] implementation for unary [Printable1]
    signature and a functor to build the extended signature. *)
module Printable1 : sig
  module type Base = sig
    type 'a t
    (** The type of the printable polymorphic values of arity one, i.e., values
        with one type parameter, such as ['a list], ['a option], etc. *)

    val pp : 'a printer -> Format.formatter -> 'a t -> unit
    (** [pp] is the pretty-printer for the type {!t}. *)
  end

  module Make(B : Base) : (Printable1 with type 'a t := 'a B.t)
  (** Functor building an instance of {!Printable1} given a {!Printable1.Base}
      implementation. *)
end


(** Signature for the binary types that can be printed.

    [Printable2.Base] is the minimal implementation of the [Printable2]
    instances. Some examples of binary printable types, i.e., types with two
    parameters, include: {!('a, 'e) Result.t} and {!('a, 'b) Either.t}. *)
module type Printable2 = sig
  type ('a, 'b) t
  (** The type of the printable polymorphic values of arity two, i.e., values
      with two type parameters, such as [('a, 'e) Result.t] and [('a, 'b)
      Either.t]. *)

  val pp : 'a printer -> 'b printer -> ('a, 'b) t printer
  (** [pp pp_a pp_b] is the pretty-printer for the binary type [('a, 'b) t].
      [pp_a] and [pp_b] are the pretty-printers for the wrapped types ['a] and
      ['b]. *)

  val show : 'a printer -> 'b printer -> ('a, 'b) t -> string
  (** [show pp_a pp_b self] converts binary [('a, 'b) t] to a string
      representation using [pp_a] and [pp_b] for the wrapped types ['a] and
      ['b]. *)

  val print : 'a printer -> 'b printer -> ('a, 'b) t -> unit
  (** [print pp_a pp_b self] writes [self] to the standard output using [pp_a]
      and [pp_b] pretty-printers for the wrapped types and {!Printable1.pp} for
      [self]. *)
end


(** Structure defining the [Base] implementation for binary [Printable2]
    signature and a functor to build the extended signature. *)
module Printable2 : sig
  module type Base = sig
    type ('a, 'b) t
    (** The type of the printable polymorphic values of arity two, i.e., values
        with two type parameters, such as [('a, 'e) Result.t] and [('a, 'b)
        Either.t]. *)

    val pp : 'a printer -> 'b printer -> ('a, 'b) t printer
    (** [pp pp_a pp_b] is the pretty-printer for the binary type [('a, 'b) t].
        [pp_a] and [pp_b] are the pretty-printers for the wrapped types ['a] and
        ['b]. *)
  end

  module Make(B : Base) : (Printable2 with type ('a, 'b) t := ('a, 'b) B.t)
  (** Functor building an instance of {!Printable2} given a {!Printable2.Base}
      implementation. *)
end


(** {1:interfaces Common Interfaces} *)

(* Signature for nullary hashable types. *)
module type Hashable = sig
  type t

  val hash : t -> int
end


(* Signature for unary hashable types. *)
module type Hashable1 = sig
  type 'a t

  val hash : ('a -> int) -> 'a t -> int
end


(* Signature for binary hashable types. *)
module type Hashable2 = sig
  type ('a, 'b) t

  val hash : ('a -> int) -> ('b -> int) -> ('a, 'b) t -> int
end


(* Bounded is used to name the upper and lower limits of a type. *)
module type Bounded = sig
  type t

  val min_value : t
  val max_value : t
end


(** Signature for nullary types with a default value. *)
module type Default = sig
  type t

  val default : t
end

(** Signature for unary types with a default value. *)
module type Default1 = sig
  type 'a t

  val default : 'a t
end


(** Enum defines operations on sequentially ordered types. *)
module type Enumerable = sig
  type t

  val predecessor : t -> t
  (** [pred self] is the predecessor of [self].

      @raise No_value if [self] has no predecessor. *)

  val successor : t -> t
  (** [succ self] is the successor of [self].

      @raise No_value if [self] has no successor. *)
end


(** Signature for the nullary types that can be parsed. *)
module type Parsable = sig
  type t

  val parse : string -> t option
end


(** Signature for the unary types that can be parsed. *)
module type Parsable1 = sig
  type 'a t

  val parse : (string -> 'a) -> string -> 'a t option
end


(** Signature for the binary types that can be parsed. *)
module type Parsable2 = sig
  type ('a, 'b) t

  val parse : (string -> 'a) -> (string -> 'b) -> string -> ('a, 'b) t option
end


(** Basic numeric interface. *)
module type Numeric = sig
  type t

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val (~- ) : t -> t
  val (~+ ) : t -> t
  val abs : t -> t
  val signum : t -> t
  val of_int : int -> t
end


(** {1:exn Exceptions} *)

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

val raises : ?only: exn -> (unit -> 'a) -> bool
(** [raises ?only f] is [true] if the exception [exn] is raised while calling
    [f], [false] otherwise. If no [exn] is given, will return [true] on any
    exception.

    {[
      assert (raises (fun () -> fail "yes"));
      assert (raises (fun () -> Option.force None) ~only:No_value);
      assert (not (raises (fun () -> "no")))
    ]} *)

val fail : string -> 'a
(** [fail msg] raises the [Failure] exception with given [msg]. Should be used
    for generic failures with a simple message.

    {[
      if List.length a = 0 then
        fail "empty list"
    ]} *)

exception Undefined
(** An exception for undefined or not implemented code branches. *)

val undefined : unit -> 'a
(** [undefined ()] raises the [Undefined] exception.

    {[
      let x = 42 in
      match x with
      | x when x = 0 -> " "
      | x when x > 0 -> "+"
      | x when x < 0 -> "-"
      | _            -> undefined ()
    ]} *)


(** {1:data Standard Data Types} *)

(** Unit type and operations. *)
module Unit : sig
  type t = unit

  (** {6 Implemented instances} *)
  include Bounded    with type t := t
  include Comparable with type t := t
  include Default    with type t := t
  include Equatable  with type t := t
  include Hashable   with type t := t
  include Parsable   with type t := t
  include Printable  with type t := t
end


(** Boolean type and operations. *)
module Bool : sig
  type t = bool
  (** Standard boolean type. *)

  val not : t -> t
  (** [not a] is the boolean negation of [a]. *)

  val ( && ) : t -> t -> t
  (** [a && b] is the boolean 'and' of [a] and [b]. Evaluation is sequential,
      left-to-right, [a] is evaluated first, and if it returns [false], [b] is
      not evaluated at all. *)

  val ( || ) : t -> t -> t
  (** [a || b] is the boolean 'or' of [a] and [b]. Evaluation is sequential,
      left-to-right, [a] is evaluated first, and if it returns [true], [b] is
      not evaluated at all. *)

  val of_int : int -> bool
  val to_int : bool -> int

  val to_option : 'a -> t -> 'a option
  (** [to_option x self] is an optional value containing [x] if [self] is
      [true] and [None] otherwise.

      {[
        assert (Bool.to_option 42 false == None);
        assert (Bool.to_option 42 true  == Some 42);
      ]} *)

  (** {6 Implemented instances} *)
  include Bounded    with type t := t
  include Comparable with type t := t
  include Default    with type t := t
  include Enumerable with type t := t
  include Equatable  with type t := t
  include Hashable   with type t := t
  include Parsable   with type t := t
  include Printable  with type t := t
end

val not : bool -> bool
(** Global alias for {!Bool.not}. *)

val ( && ) : bool -> bool -> bool
(** Global alias for {!Bool.( && )}. *)

val ( || ) : bool -> bool -> bool
(** Global alias for {!Bool.( || )}. *)


(** Integer type and arithmetic operations. *)
module Int : sig
  type t = int
  (** Standard integer type. *)

  val ( / ) : t -> t -> t
  (* Integer devision. *)

  val ( mod ) : t -> t -> t

  (** {6:conv Conversion } *)

  val to_string : t -> string
  (** [to_string x] is a string representation of the integer [x]. *)

  val of_float : float -> int
  (** [of_float x] is an integer representation of the float value [x]. *)

  val of_char : char -> int
  (** [of_char x] is an integer representation of the char value [x].

      See [Char.try_of_int] *)

  module Unsafe : sig
    val of_string : string -> int
    (** [of_string str] is an integer created by parsing the string [str].

        @raise Failure if the string does not represent a valid integer. *)
  end

  (** {6 Implemented instances} *)
  include Bounded    with type t := t
  include Comparable with type t := t
  include Default    with type t := t
  include Enumerable with type t := t
  include Equatable  with type t := t
  include Hashable   with type t := t
  include Numeric    with type t := t
  include Parsable   with type t := t
  include Printable  with type t := t
end


(** Float type and arithmetic operations. *)
module Float : sig

  type t = float

  val ( / ) : t -> t -> t

  val ( mod ) : t -> t -> t

  val infinity : float
  (** Positive infinity. *)

  val neg_infinity : float
  (** Negative infinity. *)

  val nan : float
  (** A special floating-point value denoting the result of an undefined
      operation such as [0.0 /. 0.0]. Stands for 'not a number'. Any
      floating-point operation with [nan] as argument returns [nan] as result.
      As for floating-point comparisons, [=], [<], [<=], [>] and [>=] return
      [false] and [<>] returns [true] if one or both of their arguments is [nan]. *)

  val epsilon : float
  (** The difference between [1.0] and the smallest exactly representable
      floating-point number greater than [1.0]. *)

  val round : float -> float
  (** [round f] rounds the float value to the nearest integer float. *)

  val exp : float -> float
  (** Exponential. *)

  val frexp : float -> float * int
  (** [frexp f] returns the pair of the significant and the exponent of [f]. When
      [f] is zero, the significant [x] and the exponent [n] of [f] are equal to
      zero.  When [f] is non-zero, they are defined by [f = x *. 2 ** n] and [0.5
      <= x < 1.0]. *)

  val ldexp : float -> int -> float
  (** [ldexp x n] returns [x *. 2 ** n]. *)

  val modf : float -> float * float
  (** [modf f] returns the pair of the fractional and integral part of [f]. *)

  type fpclass = Pervasives.fpclass
  (** The five classes of floating-point numbers, as determined by
      the {!Pervasives.classify_float} function. *)

  val classify : float -> fpclass
  (** Return the class of the given floating-point number:
      normal, subnormal, zero, infinite, or not a number. *)

  (** {6:conv Conversion } *)

  val of_int : int -> float
  (** Convert an integer to floating-point. *)

  val to_int : float -> int
  (** Truncate the given floating-point number to an integer. The result is
      unspecified if the argument is [nan] or falls outside the range of
      representable integers. *)

  val to_string : float -> string

  module Unsafe : sig
    val of_string : string -> float
    (** [of_string str] is a float created by parsing the string [str].

        @raise Failure if the string does not represent a valid integer. *)
  end

  (** {6 Implemented instances} *)
  include Bounded    with type t := t
  include Comparable with type t := t
  include Default    with type t := t
  include Equatable  with type t := t
  include Hashable   with type t := t
  include Numeric    with type t := t
  include Parsable   with type t := t
  include Printable  with type t := t
end


(** Char type and operations. *)
module Char : sig

  type t = char
  (** The type for bytes. *)

  val try_of_int : int -> char option
  (** [try_of_int b] is a byte from [b]. [None] is returned if [b] is not in
      the range \[[0x00];[0xFF]\]. *)

  val to_int : char -> int
  (** [to_int b] is the byte [b] as an integer. *)

  module Unsafe : sig
    val of_int : int -> char
    (** [of_int b] is a byte from [b].

        See [Char.try_of_int]
        @raise Failure if the byte [b] does not represent a valid char. *)
  end

  (** {6 Implemented instances} *)
  include Bounded    with type t := t
  include Comparable with type t := t
  include Default    with type t := t
  include Enumerable with type t := t
  include Equatable  with type t := t
  include Hashable   with type t := t
  include Parsable   with type t := t
  include Printable  with type t := t
end


val char : int -> char option
(** Global alias for {!Char.option_of_int}. *)

val code : char -> int
(** Global alias for {!Char.to_int}. *)


(** Bitwise operations *)
module Bitwise : sig
  val (~~~) : int -> int
  (** Bitwise logical negation. *)

  val (|||)  : int -> int -> int
  (** Bitwise logical or. *)

  val (&&&)  : int -> int -> int
  (** Bitwise logical and. *)

  val (^^^)  : int -> int -> int
  (** Bitwise logical exclusive or. *)

  val (<<<)  : int -> int -> int
  (** [n <<< m] shifts n to the left by m bits. The result is unspecified if m
      < 0 or m >= bitsize, where bitsize is 32 on a 32-bit platform and 64 on a
      64-bit platform. *)

  val (>>>) : int -> int -> int
  (** [n >>> m] shifts n to the right by m bits. This is an arithmetic shift:
      the sign bit of n is replicated. The result is unspecified if m < 0 or m
      >= bitsize. *)

  val (>>>!) : int -> int -> int
  (** [n >>>! m] shifts n to the right by m bits. This is a logical shift:
      zeroes are inserted regardless of the sign of n. The result is
      unspecified if m < 0 or m >= bitsize. *)
end


(** {1:fun Function operations}

    Although almost all the functions are public, the module is defined to be
    used if name conflicts exist. *)

val ( |> ) : 'a -> ('a -> 'b) -> 'b
(** [x |> f] reads as "apply forward" or "pipe" and similarly to [<|] is
    equivalent to [f x]. Can be used to create long chains of forward
    transformation in the style of Unix pipes:

    {[
      x |> f |> g |> h = h (g (f x))
    ]} *)

val ( @ ) : ('a -> 'b) -> 'a -> 'b
(** [f @ x] reads as "apply" and is equivalent to [f x]. Can be used to
    create long chains of transformation and since [@] has low,
    right-associative precedence, it sometimes allows parentheses to be
    omitted.

    {[
      f @ g @ h @ x = f (g (h x))
    ]} *)

val identity : 'a -> 'a
(** [identity a] always returns [a]. This is called the identity function. *)

val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
(** [flip f x y] is [f y x], i.e., the application of [f] with its first two
    arguments reversed.*)

val always : 'a -> 'b -> 'a
(** [always x y] is [x] for any input value [y]. *)

val curry : (('a * 'b) -> 'c) -> 'a -> 'b -> 'c
(* [curry f] converts an uncurried function [f] to a curried function. *)

val uncurry : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c
(* [uncurry f] converts a curried function [f] to a function on pairs. *)

val compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
val ( << )  : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
val ( >> )  : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** [compose f g] is the mathematical composition of functions [f] and [g].
    [( << )] and [( >> )] are the infix versions of [compose].

    {[
      compose f g = fun x -> f (g x)
    ]} *)


module Control : module type of Control

(** Export Control modules and definitions. *)
module Exception = Control.Exception
module Function = Control.Function


(** {1:data Datatypes} *)
module Data : sig
  module Array  : module type of Array
  module Either : module type of Either
  module List   : module type of List
  module Option : module type of Option
  module Ref    : module type of Ref
  module Result : module type of Result
  module Set    : module type of Set
  module Stream : module type of Stream
  module String : module type of String
  module Tuple  : module type of Tuple
  module Void   : module type of Void
end

(** Export Data modules and definitions. *)
module Option = Data.Option
module Result = Data.Result

type ('a, 'b) pair = ('a, 'b) Data.Tuple.pair
(** Global alias for {!Data.Tuple.pair}. *)

val first : ('a, 'b) pair -> 'a
(** Global alias for {!Data.Tuple.first}. *)

val second : ('a, 'b) pair -> 'b
(** Global alias for {!Data.Tuple.second}. *)

type void = Data.Void.t
(** Global alias for {!Void.t}. *)


(** {6 Global character operations} *)

val char : int -> char option
(** Global alias for {!Char.option_of_int}. *)

val code : char -> int
(** Global alias for {!Char.to_int}. *)


(** {6 Global option operations} *)

val some : 'a -> 'a option
(** Global alias for {!Option.some}. *)

val none : 'a option
(** Global alias for {!Option.none}. *)

val is_some : 'a option -> bool
(** Global alias for {!Option.is_some}. *)

val is_none : 'a option -> bool
(** Global alias for {!Option.is_none}. *)

val option : (unit -> 'b) -> ('a -> 'b) -> 'a option -> 'b
(** Global alias for {!Option.case}. *)

val ( or ) : 'a option -> 'a -> 'a
(** Global alias for {!Option.( or )}. *)

val or_else : (unit -> 'a) -> 'a option -> 'a
(** Global alias for {!Option.or_else}. *)

val or_fail : string -> 'a option -> 'a
(** Global alias for {!Option.or_fail}. *)


(* (** {6 Global result operations} *) *)

val ok : 'a -> ('a, 'e) result
(** Global alias for {!Result.ok}. *)

val error : 'e -> ('a, 'e) result
(** Global alias for {!Result.error}. *)

val is_ok : ('a, 'e) result -> bool
(** Global alias for {!Result.is_ok}. *)

val is_error : ('a, 'e) result -> bool
(** Global alias for {!Result.is_error}. *)

val result : ('a -> 'b) -> ('e -> 'b) -> ('a, 'e) result -> 'b
(** Global alias for {!Result.case}. *)


(* (** {6 Global either definitions} *) *)

type ('a, 'b) either = Left of 'a | Right of 'b


(*---------------------------------------------------------------------------
   Copyright (c) 2017 Rizo Isrof

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
