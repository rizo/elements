
type 'a t = 'a option
exception No_value

let some x = Some x
let none = None

let option ~none:if_none ~some:if_some opt =
  match opt with
  | None -> Lazy.force if_none
  | Some a -> if_some a

let value_exn opt =
  match opt with
  | Some x -> x
  | None -> raise No_value

let value ~default opt =
  match opt with
  | Some x -> x
  | None -> default

let return x = Some x

let guard f x =
  try Some (f x)
  with _ -> None

let (>>=) opt f =
  match opt with
  | Some x -> f x
  | None -> None

let (>>|) opt f =
  match opt with
  | Some x -> Some (f x)
  | None -> None

let (>>) opt1 opt2 =
  opt1 >>= fun _ -> opt2

let (||) opt default =
  value ~default opt

let (!) opt = value_exn opt

