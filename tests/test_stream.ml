
(* Sets *)
let s0 = Int.Set.empty
let s1 = Int.Set.of_list l1

(* Arrays *)
let a0 = Array.make 0 0
let a1 = [|1; 2; 3; 4; 5; 6; 7|]
let a2 = Array.make 1_000_000 0

(* Channels *)
let c0 = open_in "/dev/null"
let c1 = open_in "/usr/share/dict/words"
let c2 = open_in "/dev/zero"
let c3 = open_in "/dev/random"

(* Lists *)
let l0 = []
let l1 = [1; 2; 3; 4; 5; 6; 7]
let rec l2 = 0::l2

(* Binary Trees *)
type 'a tree =
  | Nil
  | Bin of 'a tree * 'a * 'a tree

let t0 = Nil
let t1 = Bin (Bin (Nil, 2, Nil), 1, Bin (Nil, 3, Nil))
let rec t2 = Bin (t2, 0, t2)

let rec gen_tree_preorder t =
  match t with
  | Nil           -> empty
  | Bin (l, x, r) -> (yield x ++
                      lazy (gen_tree_preorder l) ++
                      lazy (gen_tree_preorder r))

let rec gen_tree_idorder t =
  match t with
  | Nil           -> empty
  | Bin (l, x, r) -> (gen_tree_idorder l ++
                      lazy (yield x) ++
                      lazy (gen_tree_idorder r))

let rec gen_tree_postorder t =
  match t with
  | Nil           -> empty
  | Bin (l, x, r) -> (gen_tree_postorder l ++
                      lazy (gen_tree_postorder r) ++
                      lazy (yield x))

