
module IO = System_io

val time : ?fmt: ('b -> float -> string) -> ('a -> 'b) -> 'a -> 'b

