
module type Type = sig
  type t
end


module type Type1 = sig
  type 'a t
end


module type Type2 = sig
  type ('a, 'b) t
end


module type Type3 = sig
  type ('a, 'b, 'c) t
end


module type Semigroup = sig
  type t
  val append : t -> t -> t
end


module type Monoid = sig
  type t
  val empty : t
  include Semigroup with type t := t
end


module type Monad = sig
  type 'a t

  val return : 'a -> 'a t

  val ( >>= )  : 'a t -> ('a -> 'b t) -> 'b t
  val ( =<< )  : ('a -> 'b t) -> 'a t -> 'b t
  val bind     : ('a -> 'b t) -> 'a t -> 'b t

  val ( >> ) : 'a t -> (unit -> 'b t) -> 'b t

  val sequence : 'a t list -> 'a list t

  val join : 'a t t -> 'a t
end


module Monad = struct
  module type Base = sig
    type 'a t

    val return : 'a -> 'a t

    val bind : ('a -> 'b t) -> 'a t -> 'b t
  end

  module Make(B : Base) : (Monad with type 'a t := 'a B.t) = struct
    include B

    let ( >>= ) m f =
      B.bind f m

    let ( =<< ) f m =
      B.bind f m

    let ( >> ) m1 m2' =
      m1 >>= fun _ -> m2' ()

    let join maa =
      maa >>= fun ma -> ma

    let sequence mas =
      Shadow_stdlib.List.fold_right
        (fun ma ms ->
           ma >>= fun a ->
           ms >>= fun s ->
           return (a :: s))
        mas
        (return [])

    let sequence_unit mas =
      sequence mas >>= fun _ -> B.return ()
  end
end


module type Monad2 = sig
  type ('a, 'x) t

  val return : 'a -> ('a, _) t

  val ( >>= )  : ('a, 'x) t -> ('a -> ('b, 'x) t) -> ('b, 'x) t
  val ( =<< )  : ('a -> ('b, 'x) t) -> ('a, 'x) t -> ('b, 'x) t
  val bind  : ('a -> ('b, 'x) t) -> ('a, 'x) t -> ('b, 'x) t

  val ( >> ) : ('a, 'x) t -> ('b, 'x) t Lazy.t -> ('b, 'x) t

  val sequence : ('a, 'x) t list -> ('a list, 'x) t

  val join : (('a, 'x) t, 'x) t -> ('a, 'x) t
end


module Monad2 = struct
  module type Base = sig
    type ('a, 'x) t

    val return : 'a -> ('a, 'x) t

    val bind : ('a -> ('b, 'x) t) -> ('a, 'x) t -> ('b, 'x) t
  end

  module Make(B : Base) : (Monad2 with type ('a, 'x) t := ('a, 'x) B.t) = struct
    (* static include Monad.Make? *)
    include B

    let ( >>= ) m f =
      B.bind f m

    let ( =<< ) f m =
      B.bind f m

    let ( >> ) m1 m2 =
      m1 >>= fun _ -> Lazy.force m2

    let join maa =
      maa >>= fun ma -> ma

    let sequence mas =
      Shadow_stdlib.List.fold_right
        (fun ma ms ->
           ma >>= fun a ->
           ms >>= fun s ->
           return (a :: s))
        mas
        (return [])

    let sequence_unit mas =
      sequence mas >>= fun _ -> B.return ()
  end
end


module type Functor = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val (<$>) : ('a -> 'b) -> 'a t -> 'b t
  val ( |>) : 'a t -> ('a -> 'b ) -> 'b t
end


module Functor = struct
  module type Base = sig
    type 'a t

    val map : ('a -> 'b) -> 'a t -> 'b t
  end

  module Make(B : Base) : (Functor with type 'a t := 'a B.t) = struct
    include B

    let ( <$> ) f m = map f m

    let (|>) m f = map f m
  end

  module With_monad(M : Monad.Base) : (Functor with type 'a t := 'a M.t) = struct
    include Make(struct
        type 'a t = 'a M.t

        let map f ma =
          M.bind (fun a -> M.return (f a)) ma
      end)
  end
end


module type Functor2 = sig
  type ('a, 'x) t

  val map : ('a -> 'b) -> ('a, 'x) t -> ('b, 'x) t
  val (<$>) : ('a -> 'b ) -> ('a, 'x) t -> ('b, 'x) t
  val ( |>) : ('a, 'x) t -> ('a -> 'b ) -> ('b, 'x) t
end


module Functor2 = struct
  module type Base = sig
    type ('a, 'x) t

    val map : ('a -> 'b) -> ('a, 'x) t -> ('b, 'x) t
  end

  module Make(B : Base) = struct
    include B

    let ( <$> ) f m =
      map f m

    let (|>) m f = map f m
  end

  module With_monad(M : Monad2.Base) : (Functor2 with type ('a, 'x) t := ('a, 'x) M.t) = struct
    include Make(struct
        type ('a, 'x) t = ('a, 'x) M.t
        let map f ma =
          M.bind (fun a -> M.return (f a)) ma
      end)
  end
end


module type Applicative = sig
  type 'a t

  val pure : 'a -> 'a t

  val ap : ('a -> 'b) t -> 'a t -> 'b t

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  val ( <* ) : 'a t -> 'b t -> 'a t

  val ( *> ) : 'a t -> 'b t -> 'b t

  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
end


module Applicative = struct
  module type Base = sig
    type 'a t

    val pure : 'a -> 'a t

    val ap : ('a -> 'b) t -> 'a t -> 'b t
  end


  module Into_functor (A : Base) : (Functor.Base with type 'a t := 'a A.t) = struct
    type 'a t = 'a A.t

    let map f ma =
      A.ap (A.pure f) ma
  end


  module Make (B : Base) : (Applicative with type 'a t := 'a B.t) = struct
    include B

    module Functor = Into_functor (B)

    let ( <*> ) fab fa =
      B.ap fab fa

    let lift2 f fa fb =
      Functor.map f fa <*> fb

    let ( <* ) fa fb =
      lift2 (fun x _ -> x) fa fb

    let ( *> ) fa fb =
      failwith "TODO"
  end


  module With_monad (M : Monad.Base) : (Applicative with type 'a t := 'a M.t) = struct
    type 'a t = 'a M.t

    module Base = struct
      type nonrec 'a t = 'a t

      let pure =
        M.return

      module Monad = Monad.Make(M)

      let ap fab fa =
        let open Monad in
        fab >>= fun f ->
        fa >>= fun a ->
        M.return (f a)
    end

    include Make(Base)
  end
end


module type Applicative2 = sig
  type ('a, 'x) t

  val pure : 'a -> ('a, 'x) t

  val ap : ('a -> 'b, 'x) t -> ('a, 'x) t -> ('b, 'x) t

  val ( <*> ) : ('a -> 'b, 'x) t -> ('a, 'x) t -> ('b, 'x) t

  val ( <* ) : ('a, 'x) t -> ('b, 'x) t -> ('a, 'x) t

  val ( *> ) : ('a, 'x) t -> ('b, 'x) t -> ('b, 'x) t

  val lift2 : ('a -> 'b -> 'c) -> ('a, 'x) t -> ('b, 'x) t -> ('c, 'x) t
end


module Applicative2 = struct
  module type Base = sig
    type ('a, 'x) t

    val pure : 'a -> ('a, 'x) t

    val ap : (('a -> 'b), 'x) t -> ('a, 'x) t -> ('b, 'x) t
  end


  module Into_functor(A : Base) : (Functor2.Base with type ('a, 'x) t := ('a, 'x) A.t) = struct
    type ('a, 'x) t = ('a, 'x) A.t

    let map f ma =
      A.ap (A.pure f) ma
  end


  module Make(B : Base) : (Applicative2 with type ('a, 'x) t := ('a, 'x) B.t) = struct
    include B

    module Functor = Into_functor(B)

    let ( <*> ) fab fa =
      B.ap fab fa

    let lift2 f fa fb =
      Functor.map f fa <*> fb

    let ( <* ) fa fb =
      lift2 (fun x _ -> x) fa fb

    let ( *> ) fa fb =
      failwith "TODO"
  end


  module With_monad(M : Monad2.Base) : (Applicative2 with type ('a, 'x) t := ('a, 'x) M.t) = struct
    type ('a, 'x) t = ('a, 'x) M.t

    module Base = struct
      type nonrec ('a, 'x) t = ('a, 'x) t

      let pure =
        M.return

      module Monad = Monad2.Make(M)

      let ap fab fa =
        let open Monad in
        fab >>= fun f ->
        fa >>= fun a ->
        M.return (f a)
    end

    include Make(Base)
  end
end


module type Alternative = sig
  type 'a t

  val empty : 'a t

  val (<|>) : 'a t -> 'a t -> 'a t

  val some : 'a t -> 'a list t

  val many : 'a t -> 'a list t
end


module Alternative = struct
  module type Base = sig
    type 'a t

    val empty : 'a t

    val (<|>) : 'a t -> 'a t -> 'a t
  end

  module Make (B : Base) (A : Applicative with type 'a t := 'a B.t) :
    (Alternative with type 'a t := 'a B.t) = struct

    module F = Applicative.Into_functor(struct
        type 'a t = 'a B.t
        include A
      end)

    include B
    open A

    let some v =
      let rec many_v () = some_v () <|> pure []
      and some_v () = F.map Shadow_stdlib.List.cons v <*> many_v () in
      some_v ()

    let many v =
      let rec many_v () = some_v () <|> pure []
      and some_v () = F.map Shadow_stdlib.List.cons v <*> many_v () in
      many_v ()
  end
end


module type Alternative2 = sig
  type ('a, 'x) t

  val empty : ('a, 'x) t

  val (<|>) : ('a, 'x) t -> ('a, 'x) t -> ('a, 'x) t

  val some : ('a, 'x) t -> ('a list, 'x) t

  val many : ('a, 'x) t -> ('a list, 'x) t
end


module Alternative2 = struct
  module type Base = sig
    type ('a, 'x) t

    val empty : ('a, 'x) t

    val (<|>) : ('a, 'x) t -> ('a, 'x) t -> ('a, 'x) t
  end

  module Make (B : Base) (A : Applicative2 with type ('a, 'x) t := ('a, 'x) B.t) :
    (Alternative2 with type ('a, 'x) t := ('a, 'x) B.t) = struct

    module F = Applicative2.Into_functor(struct
        type ('a, 'x) t = ('a, 'x) B.t
        include A
      end)

    include B
    open A

    let some v =
      let rec many_v () = some_v () <|> pure []
      and some_v () = F.map Shadow_stdlib.List.cons v <*> many_v () in
      some_v ()

    let many v =
      let rec many_v () = some_v () <|> pure []
      and some_v () = F.map Shadow_stdlib.List.cons v <*> many_v () in
      many_v ()
  end
end


module type State = sig
  type state

  type 'a t = state -> ('a * state)

  include Monad with type 'a t := 'a t

  val get : state t
  val put : state -> unit t
  val run : 'a t -> state -> 'a * state
end


module State = struct
  module Make(S : Type) : (State with type state = S.t) = struct
    type state = S.t

    type 'a t = state -> ('a * state)

    include Monad.Make(struct
        type nonrec 'a t = 'a t

        let return a = fun s -> (a, s)

        let bind f (m : 'a t) = fun s ->
          let a, s' = m s in
          f a s'
      end)

    let run self s = self s

    let put s = fun _ -> ((), s)
    let get   = fun s -> (s, s)
  end
end


module type State1 = sig
  type 'x state

  type ('a, 'x) t = 'x state -> ('a * 'x state)

  include Monad2 with type ('a, 'x) t := ('a, 'x) t

  val get : ('x state, 'x) t
  val put : 'x state -> (unit, 'x) t
  val run : ('a, 'x) t -> 'x state -> 'a * 'x state
end


module State1 = struct
  module Make(S : Type1) : (State1 with type 'x state = 'x S.t) = struct
    type 'x state = 'x S.t

    type ('a, 'x) t = 'x state -> ('a * 'x state)

    include Monad2.Make(struct
        type nonrec ('a, 'x) t = ('a, 'x) t

        let return a = fun s -> (a, s)

        let bind f (m : ('a, 'x) t) = fun s ->
          let a, s' = m s in
          f a s'
      end)

    let run self s = self s

    let put s = fun _ -> ((), s)
    let get   = fun s -> (s, s)
  end
end



module type StateT = sig
  type state
  type 'a monad
  type 'a t = state -> ('a * state) monad

  include Monad with type 'a t := 'a t

  val run : 'a t -> state -> ('a * state) monad
  val get : state t
  val put : state -> unit t

  val state : (state -> ('a * state)) -> 'a t

  val modify : (state -> state) -> unit t
end


module StateT (S : Type) (M : Monad.Base) = struct
  type state = S.t
  type 'a monad = 'a M.t
  type 'a t = state -> ('a * state) monad

  include Monad.Make(struct
      type nonrec 'a t = 'a t

      let return a = fun s ->
        M.return (a, s)

      let bind f xf s =
        M.bind (fun (x, s') -> (f x) s') (xf s)
    end)

  let run self s = self s

  let put s = fun _ -> M.return ((), s)
  let get   = fun s -> M.return (s, s)

  let state f =
    get >>= fun s ->
    let (a, s') = f s in
    put s' >>= fun _ -> (return a)

  let modify f =
    state (fun s -> ((), f s))
end


module type State1T = sig
  type 'x state
  type 'a monad
  type ('a, 'x) t = 'x state -> ('a * 'x state) monad

  include Monad2 with type ('a, 'x) t := ('a, 'x) t

  val run : ('a, 'x) t -> 'x state -> ('a * 'x state) monad
  val get : ('x state, 'x) t
  val put : 'x state -> (unit, 'x) t

  val state : ('x state -> ('a * 'x state)) -> ('a, 'x) t

  val modify : ('x state -> 'x state) -> (unit, 'x) t
end


module State1T (S : Type1) (M : Monad.Base) = struct
  type 'x state = 'x S.t
  type 'a monad = 'a M.t
  type ('a, 'x) t = 'x state -> ('a * 'x state) monad

  include Monad2.Make(struct
      type nonrec ('a, 'x) t = ('a, 'x) t

      let return a = fun s ->
        M.return (a, s)

      let bind f xf s =
        M.bind (fun (x, s') -> (f x) s') (xf s)
    end)

  let run self s = self s

  let put s = fun _ -> M.return ((), s)
  let get   = fun s -> M.return (s, s)

  let state f =
    get >>= fun s ->
    let (a, s') = f s in
    put s' >>= fun _ -> return a

  let modify f =
    state (fun s -> ((), f s))
end


module IxMonad = struct
  type ('a, 'i, 'j) t = 'i -> ('a * 'j)

  let return a = fun i -> (a, i)

  let bind f m =
    fun i ->
      let (a, j) = m i in
      let m' = f a in
      m' j

  let get   = fun s -> (s,  s)
  let put s = fun _ -> ((), s)
end


module Exn = struct
  type t = exn

  let fail  = Kernel.fail

  let show = Printexc.to_string

  let raise = Kernel.raise
  let raises = Kernel.raises
end


module Function = struct
  type ('a, 'b) t = 'a -> 'b

  let identity = Kernel.identity
  let flip     = Kernel.flip
  let curry    = Kernel.curry
  let uncurry  = Kernel.uncurry
  let compose  = Kernel.compose
  let always   = Kernel.always
  let (<<)     = Kernel.(<<)
  let (>>)     = Kernel.(>>)

  let apply f x = f x

  let map f x = compose f x

  external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply"
  external (@) : ('a -> 'b) -> 'a -> 'b = "%apply"
end


module Identity = struct
  type 'a t = 'a

  module Monad_base = struct
    type nonrec 'a t = 'a t
    let return x = x
    let bind f m = f m
  end
  include Monad.Make(Monad_base)

  include Functor.With_monad(Monad_base)
end
