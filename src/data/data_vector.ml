(* Persistent bit-partitioned Vector Trie *)

module Array = Data_array
module List  = Data_list

open Base

let shift_by = 5
let trie_len = int_of_float (2.0 ** float_of_int shift_by)

type 'a t = {
  root   : 'a node array;   (* Root node links. *)
  tail   : 'a array;        (* A pointer to the tail of the vector. *)
  length : int;             (* The length of the vector. *)
  shift  : int;             (* Current maximum index shift. *)
} [@@deriving show]

and 'a node =
  | Link of 'a node array   (* Branch node with links to other nodes. *)
  | Data of 'a array        (* Leaf node holds the actual data values. *)
  [@@deriving show]

(* O(1) *)
let empty : 'a t = {
  root   = [| |];
  length = 0;
  tail   = [| |];
  shift  = shift_by;
}

(* O(1) *)
let singleton a = {
  root   = [| |];
  length = 1;
  shift  = shift_by;
  tail   = [| a |];
}

(* O(1) *)
let is_empty v = v.length = 0

(* O(1) *)
let length v = v.length

let link = function
  | Link a -> a
  | Data _ -> fail "link request on data node"

let data = function
  | Data a -> a
  | Link a -> fail "data request on link node"

let tail_offset length =
  if length < 32 then 0
  else ((length - 1) lsr shift_by) lsl shift_by

let node_with_index v idx =
  if idx >= tail_offset v.length
  then Data v.tail
  else
    let rec loop level node =
      if level = 0 then node
      else
        let sub_idx = (idx lsr level) land (trie_len - 1) in
        loop (level - shift_by) (Array.get (link node) sub_idx) in
    loop (v.shift - shift_by) (Array.get v.root (idx lsr v.shift))

(* O(log32(n)) ~ O(1) *)
let unsafe_get v idx =
  let node = node_with_index v idx in
  Array.get (data node) (idx land (trie_len - 1))

(* O(log32(n)) ~ O(1) *)
let get v idx =
  if idx < 0 || idx >= length v
  then None
  else Some (unsafe_get v idx) (* It's safe now. *)

let rec new_path level tail =
  if level = 0 then Data tail
  else Link [| new_path (level - shift_by) tail |]

(* Pushes the tail array to its correct location in the tree.
   If the parent is a leaf node, add the tail as a Data node.
   If index maps to the existing child, extend the child with a Link to tail.
   Otherwise add a new child to parent with a tail node. *)
let rec push_tail length level (parent : 'a node array) tail : 'a node array =
  let sub_idx = ((length - 1) lsr level) land (trie_len - 1) in
  (* Parent is a leaf node. *)
  if level = shift_by then
    let target  = Data tail in
    let parent' = Array.copy_and_add parent target in
    parent'
  (* Maps to existing child.
     Replace the child with a link to target. *)
  else if sub_idx < Array.length parent then
     let child   = Array.get parent sub_idx in
     let target  = Link (push_tail length (level - shift_by) (link child) tail) in
     let parent' = Array.copy parent in
     Array.set parent' sub_idx target;
     parent'
  (* Does not map to existing child.
     Create a link and add path. *)
  else
    let target  = new_path (level - shift_by) tail in
    let parent' = Array.copy_and_add parent target in
    parent'

(* O(log32(n)) ~ O(1) *)
let add v x =
  if v.length = 0 then singleton x
  else
    (* Tail update.
       Tail node has room for another element.
       Duplicate the old tail and add a new element.
       Return the updated vector with incremented length and a new tail. *)
  if v.length land (trie_len - 1) <> 0 then
    { v with length = v.length + 1;
                tail   = Array.copy_and_add v.tail x }
  (* Root overflow
     The current length requires another shift.
     Replace the current root with a new one and add the tail to the tree. *)
  else if v.length lsr shift_by > 1 lsl v.shift then
    { length = v.length + 1;
      shift  = v.shift  + shift_by;
      tail   = [| x |];
      root   = [| Link v.root; new_path v.shift v.tail |] }
  (* Update the tree.
     Push the tail to the root. *)
  else
    { length = v.length + 1;
      shift  = v.shift;
      tail   = [| x |];
      root   = push_tail v.length v.shift v.root v.tail }

let of_list l =
  List.fold_left ~f:(fun v x -> add v x) ~init:empty l

let add_from_list init l =
  List.fold_left ~f:(fun v x -> add v x) ~init l

