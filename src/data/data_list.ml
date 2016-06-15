
module Result = Data_result
module String = Data_string
module Option = Data_option

open Base

include List

let direct_depth_default_ = 1000

type ('k, 'v) assoc = ('k * 'v) list

let compare ?(cmp = Pervasives.compare) a b =
  let rec loop a b =
    match a, b with
    | [], [] ->  0
    | [], _  -> -1
    | _ , [] ->  1
    | x :: xs, y :: ys ->
      let n = cmp x y in
      if n = 0
      then loop xs ys
      else n
  in
  loop a b

let cons = Base.cons
let snoc = Base.snoc

let foldl = fold_left
let foldr = fold_right

let head self =
  match self with
  | x :: _ -> Some x
  | [] -> None

let reduce f l =
  match l with
  | x :: xs -> Some (foldl f x xs)
  | []      -> None

let rec all xs f =
  match xs with
  | []               -> true
  | x :: xs when f x -> all xs f
  | _                -> false

let rev l =
  let rec loop acc l =
    match l with
    | x::xs -> loop (x::acc) xs
    | [] -> acc in
  loop [] l

let length l =
  let rec loop acc l =
    match l with
    | _ :: xs -> loop (acc + 1) xs
    | [] -> acc in
  loop 0 l

let range i j =
  let rec up i j acc =
    if i = j then i :: acc else up i (j - 1) (j :: acc)
  and down i j acc =
    if i = j then i :: acc else down i (j + 1) (j :: acc)
  in
  if i <= j then up i j [] else down i j []

let iota n = range 0 n

let map f l =
  rev (foldl (fun acc e -> f e::acc) [] l)

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

let iteri l f =
  let rec go i l =
    match l with
    | [] -> ()
    | x :: xs -> f i x; go (i + 1) xs in
  go 0 l

let filter_map l f =
  let res = foldl (fun acc e -> f e::acc) [] l in
  foldl (fun acc -> function None -> acc | Some x -> x::acc) [] res

let rec drop_while p l =
  match l with
  | [] -> []
  | x :: l' -> if p x then drop_while p l' else l

let rec drop n self =
  match self with
  | _ when n = 0 -> self
  | _ :: rest    -> drop (n - 1) rest
  | []           -> []

let last n l =
  let len = List.length l in
  if len < n then l else drop (len-n) l

let rec find_first p self =
  match self with
  | a :: _ when p a -> Some a
  | _ :: rest       -> find_first p rest
  | []              -> None

let min ?key l =
  match key with
  | None -> reduce min l
  | Some key -> reduce (fun a b -> if (key a < key b) then a else b) l

let max ?key l =
  match key with
  | None -> reduce Pervasives.max l
  | Some key -> reduce (fun a b -> if (key a > key b) then a else b) l

let max_all ?key l =
  match l with
  | [] | [_] -> l
  | h::lista -> begin
      match key with
      | None -> fst begin
          foldl (fun (maxlist,maxelem) b ->
                  if (maxelem < b) then ([b],b)
                  else if (maxelem = b) then (b::maxlist,maxelem)
                  else (maxlist,maxelem)) ([h], h) lista
        end
      | Some f -> fst begin
          foldl (fun (maxlist, maxelem) b ->
                  let kb = f b in
                  if (maxelem < kb) then ([b],kb)
                  else if (maxelem = kb) then (b::maxlist,maxelem)
                  else (maxlist,maxelem)) ([h], f h) lista
        end
    end

let group_with f l =
  let rec loop acc l =
    match l with
    | [] -> acc
    | x::_ as l ->
      let ltrue, lfalse = partition (f x) l in
      if length ltrue = 0 then
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

let take_while p l =
  let rec direct i p l = match l with
    | [] -> []
    | _ when i=0 -> safe p [] l
    | x :: l' ->
        if p x then x :: direct (i-1) p l' else []
  and safe p acc l = match l with
    | [] -> List.rev acc
    | x :: l' ->
        if p x then safe p (x::acc) l' else List.rev acc
  in
  direct direct_depth_default_ p l

let rec fold_while f acc self =
  match self with
  | a :: rest ->
    begin match f acc a with
    | `Continue acc -> fold_while f acc rest
    | `Done     acc -> acc
    end
  | [] -> acc

let enum self =
  mapi (fun i a -> (i, a)) self

let string_of_char = String.make 1

let show show_item self =
  "[" ^ String.concat ", " (map show_item self) ^ "]"

let window size step self =
  assert (size >= step);
  let rec loop r (curr_count, curr, next_count, next) input =
    match input with
    | [] -> List.rev (List.rev curr :: r)
    | x :: xs ->
      let (next_count', next') =
        if curr_count >= step
        then (next_count + 1, x :: next)
        else (next_count, next) in
      if curr_count < size
      then loop r               (curr_count + 1, x :: curr, next_count', next') xs
      else loop (rev curr :: r) (next_count',    next',     0,           [])    xs
  in
  loop [] (0, [], 0, []) self


let join sep l =
  match l with
  | [] -> ""
  | hd :: tl ->
    let str_num = ref 0 and res_len = ref 0 in
    iter (fun s -> incr str_num; res_len := !res_len + String.len s) l;
    let r = Bytes.create (!res_len + String.len sep * (!str_num - 1)) in
    String.unsafe_blit hd 0 r 0 (String.len hd);
    let pos = ref (String.len hd) in
    iter
      (fun s ->
         String.unsafe_blit sep 0 r !pos (String.len sep);
         pos := !pos + String.len sep;
         String.unsafe_blit s 0 r !pos (String.len s);
         pos := !pos + String.len s)
      tl;
    Bytes.unsafe_to_string r

let partition2 p input =
  let rec loop input acc tmp a =
    match input with
    | [] -> rev (rev tmp :: acc)
    | b :: rest when p a b ->
      loop rest (rev tmp :: acc) [b] b
    | b :: rest -> loop rest acc (b :: tmp) b in
  match input with
  | a :: rest -> loop rest [] [a] a
  | []        -> []

