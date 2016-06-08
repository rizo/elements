
module Result = Data_result

open Base

include StdLabels.List

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

let reduce_exn l ~f = Result.force (reduce l ~f)

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

let rec fold_until ~init:acc ~f l =
  match l with
  | a::rest ->
    begin match f acc a with
    | `Continue acc -> fold_until ~init:acc ~f rest
    | `Stop acc -> acc
    end
  | [] -> acc

let enum self =
  mapi ~f:(fun i a -> (i, a)) self

let string_of_char = String.make 1

let show show_item self =
  "[" ^ String.concat ", " (map ~f:show_item self) ^ "]"

let window ~size ~step self =
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
    iter (fun s -> incr str_num; res_len := !res_len + Str.len s) l;
    let r = Bytes.create (!res_len + Str.len sep * (!str_num - 1)) in
    Str.unsafe_blit hd 0 r 0 (Str.len hd);
    let pos = ref (Str.len hd) in
    iter
      (fun s ->
         Str.unsafe_blit sep 0 r !pos (Str.len sep);
         pos := !pos + Str.len sep;
         Str.unsafe_blit s 0 r !pos (Str.len s);
         pos := !pos + Str.len s)
      tl;
    Bytes.unsafe_to_string r

let partition2 should_partition input =
  let rec loop input acc acctemp last_item =
    match input with
    | [] -> rev (rev acctemp :: acc) 
    | next :: tail when should_partition last_item next ->
      loop tail (rev acctemp :: acc) [next] next
    | next :: tail -> loop tail acc (next::acctemp) next in
  match input with
  | x :: xs -> loop xs [] [x] x
  | []      -> []
