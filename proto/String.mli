
open Kernel
open Collection

type t = string
type item = char

val split : ?on: char -> t -> t list

include Comparable  with type t := t
include Default     with type t := t
include Equatable   with type t := t
include Hashable    with type t := t
include Parsable    with type t := t
include Printable   with type t := t
include Iterable0   with type t := t and type item := item
include Collection0 with type t := t and type item := item
