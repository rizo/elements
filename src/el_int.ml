
type t = int

let compare (a : t) (b : t) = Pervasives.compare a b

let to_string i = string_of_int i
let of_string s = int_of_string s

let abs = Pervasives.abs

module Set = Set.Make(struct
    type t = int
    let compare = compare
  end)

module Map = Map.Make(struct
    type t = int
    let compare = compare
  end)


