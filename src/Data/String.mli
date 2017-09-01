
type t = string

val split : ?on: char -> t -> t list

include Comparable with type t := t
include Default    with type t := t
include Equatable  with type t := t
include Hashable   with type t := t
include Parsable   with type t := t
include Printable  with type t := t

