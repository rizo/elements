
module Stdlib = Proto_shadow_stdlib
module P = Proto_shadow_stdlib.Pervasives


(* Exceptions *)

let raise ?(trace = true) self =
  if trace then
    P.raise self
  else
    P.raise_notrace self


let raises ?only:exn f =
  try
    P.ignore (f ());
    false
  with e -> begin
      match exn with
      | Some x -> e = x
      | None -> true
    end

let show self =
  Stdlib.Printexc.to_string self

let fail = P.failwith

exception Undefined

let undefined () =
  raise Undefined



(* Comparisons and Ordering *)

module type Equatable = sig
  type t

  val equal : t -> t -> bool
  val (=)   : t -> t -> bool
  val (<>)  : t -> t -> bool
end


module Equatable = struct
  module type Base = sig
    type t

    val equal : t -> t -> bool
  end

  module Make(B : Base) : (Equatable with type t := B.t) = struct
    include B

    let (=)  a b = equal a b
    let (<>) a b = not (equal a b)
  end
end


module type Equatable1 = sig
  type 'a t

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end


module type Equatable2 = sig
  type ('a, 'b) t

  val equal :
    ('a -> 'a -> bool) ->
    ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
end


type order = int

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


module Comparable = struct
  let less    = -1
  let equal   =  0
  let greater = +1

  module type Base = sig
    type t

    val compare : t -> t -> order
  end

  module Make(B : Base) : (Comparable with type t := B.t) = struct
    include B

    include Equatable.Make(struct
        type nonrec t = t
        let equal a b = P.(=) (B.compare a b) equal
      end)

    let ( <  ) a b = P.(= ) (B.compare a b) less
    let ( >  ) a b = P.(= ) (B.compare a b) greater
    let ( <= ) a b = P.(<>) (B.compare a b) greater
    let ( >= ) a b = P.(= ) (B.compare a b) less

    let min a b = if P.(=) (compare a b) less    then a else b
    let max a b = if P.(=) (compare a b) greater then a else b
  end
end


let less    = Comparable.less
let equal   = Comparable.equal
let greater = Comparable.greater


module type Comparable1 = sig
  type 'a t

  val compare :
    ('a -> 'a -> order) -> 'a t -> 'a t -> order

  val min : ('a -> 'a -> order) -> 'a t -> 'a t -> 'a t

  val max : ('a -> 'a -> order) -> 'a t -> 'a t -> 'a t
end


module Comparable1 = struct
  module type Base = sig
    type 'a t

    val compare :
      ('a -> 'a -> order) -> 'a t -> 'a t -> order
  end

  module Make(B : Base) : (Comparable1 with type 'a t := 'a B.t) = struct
    include B

    let min cmp a b = if compare cmp a b = less    then a else b
    let max cmp a b = if compare cmp a b = greater then a else b
  end
end


module type Comparable2 = sig
  type ('a, 'b) t

  val compare :
    ('a -> 'a -> order) ->
    ('b -> 'b -> order) -> ('a, 'b) t -> ('a, 'b) t ->
    order

  val min :
    ('a -> 'a -> order) ->
    ('b -> 'b -> order) -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

  val max :
    ('a -> 'a -> order) ->
    ('b -> 'b -> order) -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
end


module Comparable2 = struct
  module type Base = sig
    type ('a, 'b) t

    val compare :
      ('a -> 'a -> order) ->
      ('b -> 'b -> order) -> ('a, 'b) t -> ('a, 'b) t ->
      order
  end

  module Make(B : Base) :
    Comparable2 with type ('a, 'b) t := ('a, 'b) B.t = struct

    include B

    let min cmp1 cmp2 a b = if compare cmp1 cmp2 a b = less    then a else b
    let max cmp1 cmp2 a b = if compare cmp1 cmp2 a b = greater then a else b
  end
end


let compare = P.compare

let ( =  ) = P.( =  )
let ( <> ) = P.( <> )
let ( <  ) = P.( <  )
let ( >  ) = P.( >  )
let ( <= ) = P.( <= )
let ( >= ) = P.( >= )

let min = P.min
let max = P.max

let is = P.( == )


(* Formatting and Pretty-printing *)

type 'a printer = Format.formatter -> 'a -> unit

let format = Format.asprintf

let print ?(out = P.stdout) ?(endline = "\n") str =
  P.output_string out (str ^ endline)


module type Printable = sig
  type t

  val pp : t printer

  val show : t -> string

  val print : t -> unit
end


module Printable = struct
  module type Base = sig
    type t

    val pp : t printer
  end

  module Make(B : Base) : (Printable with type t := B.t) = struct
    include B

    let show self =
      Format.asprintf "%a" B.pp self

    let print self =
      Format.fprintf Format.std_formatter "%a" B.pp self
  end
end


module type Printable1 = sig
  type 'a t

  val pp : 'a printer -> 'a t printer

  val show : 'a printer -> 'a t -> string

  val print : 'a printer -> 'a t -> unit
end


module Printable1 = struct
  module type Base = sig
    type 'a t

    val pp : 'a printer -> Format.formatter -> 'a t -> unit
  end

  module Make(B : Base) : (Printable1 with type 'a t := 'a B.t) = struct
    include B

    let show : 'a . 'a printer -> 'a B.t -> string =
      fun pp1 self -> Format.asprintf "%a" (B.pp pp1) self

    let print : 'a . 'a printer -> 'a B.t -> unit =
      fun pp1 self -> Format.fprintf Format.std_formatter "%a" (B.pp pp1) self
  end
end


module type Printable2 = sig
  type ('a, 'b) t

  val pp : 'a printer -> 'b printer -> ('a, 'b) t printer

  val show : 'a printer -> 'b printer -> ('a, 'b) t -> string

  val print : 'a printer -> 'b printer -> ('a, 'b) t -> unit
end


module Printable2 = struct
  module type Base = sig
    type ('a, 'b) t

    val pp : 'a printer -> 'b printer -> ('a, 'b) t printer
  end

  module Make(B : Base) :
    (Printable2 with type ('a, 'b) t := ('a, 'b) B.t) = struct

    include B

    let show : 'a 'b . 'a printer -> 'b printer -> ('a, 'b) B.t -> string =
      fun pp1 pp2 self ->
        Format.asprintf "%a" (B.pp pp1 pp2) self

    let print : 'a 'b. 'a printer -> 'b printer -> ('a, 'b) B.t -> unit =
      fun pp1 pp2 self ->
        Format.fprintf Format.std_formatter "%a" (B.pp pp1 pp2) self
  end
end


module type Hashable = sig
  type t

  val hash : t -> int
end

module type Hashable1 = sig
  type 'a t

  val hash : ('a -> int) -> 'a t -> int
end

module type Hashable2 = sig
  type ('a, 'b) t

  val hash : ('a -> int) -> ('b -> int) -> ('a, 'b) t -> int
end

module type Bounded = sig
  type t

  val min_value : t
  val max_value : t
end

module type Default = sig
  type t

  val default : t
end

module type Default1 = sig
  type 'a t

  val default : 'a t
end

module type Enumerable = sig
  type t

  val pred : t -> t option
  val succ : t -> t option
end

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

module type Parsable = sig
  type t

  val parse : string -> t option
end

module type Parsable1 = sig
  type 'a t

  val parse : (string -> 'a) -> string -> 'a t option
end

module type Parsable2 = sig
  type ('a, 'b) t

  val parse : (string -> 'a) -> (string -> 'b) -> string -> ('a, 'b) t option
end


(* Unit type and operations *)

module Unit = struct
  type t = unit

  let try_of_string = function "()" -> Some () | _ -> None

  (* Bounded *)
  let min_value = ()
  let max_value = ()

  (* Comparable *)
  include Comparable.Make(struct
      type nonrec t = t
      let compare (a : t) (b : t) = P.compare a b
    end)

  (* Default *)
  let default = ()

  (* Equatable *)
  include Equatable.Make(struct
      type nonrec t = t
      let equal (a : t) (b : t) = P.( = ) a b
    end)

  (* Hashable *)
  let hash = Hashtbl.hash

  (* Parsable *)
  let parse = function
    | "()" -> Some ()
    | _ -> None

  (* Printable *)
  include Printable.Make(struct
      type nonrec t = t
      let pp fmt () = Format.fprintf fmt "()"
    end)
end


(* Boolean type and operations *)

module Bool = struct
  type t = bool

  let not = P.not

  let ( && ) = P.( && )
  let ( || ) = P.( || )

  let to_int = function false -> 0 | true -> 1
  let of_int = function 0 -> false | _ -> true

  (* Bounded *)
  let min_value = false
  let max_value = true

  (* Comparable *)
  include Comparable.Make(struct
      type nonrec t = t
      let compare (a : t) (b : t) = P.compare a b
    end)

  (* Default *)
  let default = false

  (* Enumerable *)
  let pred = function
    | true  -> Some false
    | false -> None

  let succ = function
    | true  -> None
    | false -> Some true

  (* Equatable *)
  include Equatable.Make(struct
      type nonrec t = t
      let equal (a : t) (b : t) = P.( = ) a b
    end)

  (* Hashable *)
  let hash = Hashtbl.hash

  (* Parsable *)
  let parse str =
    try
      Some (P.bool_of_string str)
    with exn ->
      None

  (* Printable *)
  include Printable.Make(struct
      type nonrec t = t
      let pp = Format.pp_print_bool
    end)
end

let not    = Bool.not
let ( && ) = Bool.( && )
let ( || ) = Bool.( || )


(* Integer type and arithmetic operations *)

module Int = struct
  type t = int

  let ( / ) = P.( / )

  let ( mod ) = P.( mod)

  let to_string self =
    P.string_of_int self

  let of_char = P.int_of_char

  let of_float = P.int_of_float

  module Unsafe = struct
    let of_string = P.int_of_string
  end

  (* Bounded *)
  let min_value = P.min_int
  let max_value = P.max_int

  (* Comparable *)
  include Comparable.Make(struct
      type nonrec t = t
      let compare (a : t) (b : t) = P.compare a b
    end)

  (* Default *)
  let default = 0

  (* Enumerable *)
  let pred x =
    if x > min_value then
      Some (x - 1)
    else
      None

  let succ x =
    if x < max_value then
      Some (x + 1)
    else
      None

  (* Equatable *)
  include Equatable.Make(struct
      type nonrec t = t
      let equal (a : t) (b : t) = P.(=) a b
    end)

  (* Hashable *)
  let hash x = Hashtbl.hash x

  (* Numeric *)
  let ( + ) = P.( + )
  let ( - ) = P.( - )
  let ( * ) = P.( * )
  let (~- ) = P.(~- )
  let (~+ ) = P.(~+ )
  let abs   = P.abs

  let signum x =
    if x > 0 then +1
    else
    if x < 0 then -1
    else
      0

  let of_int x = x

  (* Parsable *)
  let parse s =
    try
      Some (Unsafe.of_string s)
    with e ->
      None

  (* Printable *)
  include Printable.Make(struct
      type nonrec t = t
      let pp = Format.pp_print_int
    end)
end

let (~- )   = Int.(~- )
let (~+ )   = Int.(~+ )
let ( + )   = Int.( + )
let ( - )   = Int.( - )
let ( * )   = Int.( * )
let ( / )   = Int.( / )
let ( mod ) = Int.( mod )
let abs     = Int.abs


module Float = struct

  type t = float

  let ( / ) = P.( /. )
  let ( mod ) = P.mod_float
  let infinity = P.infinity
  let neg_infinity = P.neg_infinity

  let nan = P.nan

  let epsilon = P.epsilon_float

  let round_nearest_lb = -.(2. ** 52.)
  let round_nearest_ub =    2. ** 52.
  let round t =
    if t >= round_nearest_lb && t <= round_nearest_ub then
      floor (t +. 0.49999999999999994)
    else
      t

  let exp   = P.exp
  let frexp = P.frexp
  let ldexp = P.ldexp
  let modf  = P.modf

  type fpclass = P.fpclass
  let classify = P.classify_float

  let of_int x = float_of_int x

  let to_int self = int_of_float self

  let to_string self =
    string_of_float self

  module Unsafe = struct
    let of_string = P.float_of_string
  end

  (* Bounded *)
  let min_value = P.min_float
  let max_value = P.max_float

  (* Comparable *)
  include Comparable.Make(struct
      type nonrec t = t
      let compare (a : t) (b : t) = P.compare a b
    end)

  (* Default *)
  let default = 0.0

  (* Equatable *)
  include Equatable.Make(struct
      type nonrec t = t
      let equal (a : t) (b : t) = P.(=) a b
    end)

  (* Hashable *)
  let hash self = Hashtbl.hash self

  (* Numeric *)
  let ( + ) = P.( +. )
  let ( - ) = P.( -. )
  let ( * ) = P.( *. )
  let (~- ) = P.(~-. )
  let (~+ ) = P.(~+. )
  let abs   = P.abs_float

  let signum self =
    if self > 0.0 then
      1.0
    else
    if self < 0.0 then
      -1.0
    else
    if self = 0.0 then
      0.0
    else
      nan

  (* Parsable *)
  let parse str =
    try
      Some (float_of_string str)
    with e ->
      None

  (* Printable *)
  include Printable.Make(struct
      type nonrec t = t
      let pp = Format.pp_print_float
    end)

end


(* Char type and operations *)

module Char = struct
  type t = char

  let try_of_int x =
    if x < 0 || x > 255 then
      None
    else
      Some (P.char_of_int x)

  let to_int self = P.int_of_char self

  module Unsafe = struct
    let of_int = P.char_of_int
  end

  (* Bounded *)
  let min_value = Unsafe.of_int 0x00
  let max_value = Unsafe.of_int 0xFF

  (* Parsable *)
  let parse s =
    if Stdlib.String.length s = 1 then
      Some (Stdlib.String.get s 0)
    else
      None

  (* Comparable *)
  include Comparable.Make(struct
      type nonrec t = t
      let compare (a : t) (b : t) = P.compare a b
    end)

  (* Default *)
  let default = P.char_of_int 0xFF

  (* Enumerable *)
  let pred self =
    if self > min_value then
      Some (P.char_of_int (to_int self - 1))
    else
      None

  let succ self =
    if self < max_value then
      Some (P.char_of_int (to_int self + 1))
    else
      None

  (* Hashable *)
  let hash x = Hashtbl.hash x

  (* Printable *)
  include Printable.Make(struct
      type nonrec t = t
      let pp = Format.pp_print_char
    end)
end


let char = Char.try_of_int
let code = Char.to_int

module Bitwise = struct

  let (<<<)  = P.(lsl)
  let (>>>)  = P.(asr)
  let (>>>!) = P.(lsr)
  let (~~~)  = P.(lnot)
  let (|||)  = P.(lor)
  let (&&&)  = P.(land)
  let (^^^)  = P.(lxor)
end


(* Function operations *)

let ( @ ) = P.( @@ )
let ( |> ) = P.( |> )
let identity x = x
let always x _ = x
let flip f x y = f y x
let curry f x y = f (x, y)
let uncurry f (x, y) = f x y
let compose f g x = f (g x)
let ( << ) f g = compose f g
let ( >> ) g f = compose f g
