
type ('a, 'b) t = 'a -> 'b

let id x        = Base.id
let flip f x y  = Base.flip
let curry f x y = Base.curry
let uncurry f p = Base.uncurry
let compose f g = Base.compose
let (<<) f g    = Base.(<<)
let (>>) g f    = Base.(>>)

let invcompose g f = fun x -> f (g x)
let apply f x = f x
let map f x = compose f x


external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply"
external (@@) : ('a -> 'b) -> 'a -> 'b = "%apply"
let (%)  = compose
let (%>) = invcompose


