open Local
open Control

type 'a t = 'a Stdlib.ref

external make : 'a -> 'a t = "%makemutable"

external get : 'a t -> 'a = "%field0"

external set : 'a t -> 'a -> unit = "%setfield0"

let ( !  ) = Stdlib.( !  )
let ( := ) = Stdlib.( := )

let swap a b =
  let x = !a in
  a := !b;
  b := x

let modify f self =
  self := f !self

include Functor.Make(struct
    type nonrec 'a t = 'a t

    let map f self =
      make (f !self)
  end)

