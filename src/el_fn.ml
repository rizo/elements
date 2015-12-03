
type ('a, 'b) t = 'a -> 'b
let compose f g = fun x -> f (g x)
let invcompose g f = fun x -> f (g x)
let apply f x = f x
let map f x = compose f x
let id x = x
let flip f x y = f y x

external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply"
external (@@) : ('a -> 'b) -> 'a -> 'b = "%apply"
let (%)  = compose
let (%>) = invcompose


