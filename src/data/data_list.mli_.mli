
module Iter = Data_iter

open Base

type 'a t = 'a list

val cons : 'a -> 'a t -> 'a t

val compare : ?cmp:('a -> 'a -> ordering) -> 'a list -> 'a list -> ordering

include Iter.Iterable.Ext with type 'a t := 'a t

