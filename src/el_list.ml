
open El_base

include StdLabels.List

let cons x xs = x::xs

let fold = fold_left

let rec all xs ~f =
  match xs with
  | []               -> true
  | x :: xs when f x -> all xs ~f
  | _                -> false

let rev l =
  let rec loop acc l =
    match l with
    | x::xs -> loop (x::acc) xs
    | [] -> acc in
  loop [] l

let len l =
  let rec loop acc l =
    match l with
    | _::xs -> loop (acc + 1) xs
    | [] -> acc in
  loop 0 l

let rec range s e =
  if s = e then []
  else s::range (s + 1) e

let iota = range 0

let map l ~f =
  rev (fold l ~f:(fun acc e -> f e::acc) ~init:[])

let nth l n =
  if n < 0 then
    Error (Invalid_argument "nth: negative index")
  else
    let rec go l n =
      match l with
      | [] -> Error (Failure "nth: list index out of range")
      | x :: xs -> if n = 0 then Ok x else go xs (n - 1) in
    go l n

let rec iter f l =
  match l with
  | [] -> ()
  | x :: xs -> f x; iter f xs

let iteri l ~f =
  let rec go i l =
    match l with
    | [] -> ()
    | x :: xs -> f i x; go (i + 1) xs in
  go 0 l

let filter_map l ~f =
  let res =
    fold l ~init:[] ~f:(fun acc e -> f e::acc) in
  fold res ~init:[]
    ~f:(fun acc -> function None -> acc | Some x -> x::acc)

let reduce l ~f =
  match l with
  | x::xs -> Ok (fold xs ~f ~init:x)
  | [] -> Error (Failure "reduce: empty list with no initial value")

let reduce_exn l ~f = El_result.(!) (reduce l ~f)

let find l ?key ~f =
  match key with
  | None -> reduce l ~f
  | Some key -> reduce l ~f:(fun a b -> if f (key a) (key b) then a else b)

let min ?key l =
  match key with
  | None -> reduce l ~f:min
  | Some key -> reduce l ~f:(fun a b -> if (key a < key b) then a else b)

let max ?key l =
  match key with
  | None -> reduce l ~f:Pervasives.max
  | Some key -> reduce l ~f:(fun a b -> if (key a > key b) then a else b)

let max_all ?key l =
  match l with
  | [] | [_] -> l
  | h::lista -> begin
      match key with
      | None -> fst begin
          fold ~init:([h], h)
            ~f:(fun (maxlist,maxelem) b ->
                if (maxelem < b) then ([b],b)
                else if (maxelem = b) then (b::maxlist,maxelem)
                else (maxlist,maxelem)
              ) lista
        end
      | Some f -> fst begin
          fold ~init:([h], f h)
            ~f:(fun (maxlist, maxelem) b ->
                let kb = f b in
                if (maxelem < kb) then ([b],kb)
                else if (maxelem = kb) then (b::maxlist,maxelem)
                else (maxlist,maxelem)
              ) lista
        end
    end

let group_with l ~f =
  let rec loop acc l =
    match l with
    | [] -> acc
    | x::_ as l ->
      let ltrue, lfalse = partition l ~f:(f x) in
      if len ltrue = 0 then
        [x] :: acc
      else
        loop (ltrue :: acc) lfalse in
  loop [] l

let take l n =
  let rec loop l n acc =
    if n = 0 then rev acc
    else match l with
      | x::xs -> loop xs (n - 1) (x :: acc)
      | [] -> rev acc in
  loop l n []

