open Kernel
open Collection

type t = string

type item = char

val split : ?on: char -> t -> t iter
(** [split ~on:sep self] is an iterator with substrings of [self] split on
    every occurrence of the [sep]arator character (space by default). Empty
    strings will be produced when multiple consecutive separators are found.

    {[
      assert (slipt "hello world" == iter ["hello"; "world"]);
      assert (split ~on:',' "1997,Ford,E350" == iter ["1997"; "Ford"; "E350"]);
      assert (split ~on:'|' "|x||" == iter [""; "x"; ""; ""]);
    ]} *)


(** {6 Implemented instances} *)
include Comparable   with type t := t
include Default      with type t := t
include Equatable    with type t := t
include Hashable     with type t := t
include Parsable     with type t := t
include Printable    with type t := t
include Iterable0    with type t := t and type item := item
include Collectable0 with type t := t and type item := item

