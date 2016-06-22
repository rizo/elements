(* Persistent bit-partitioned Vector Trie *)

(* Indexed finite iterable. *)

module Array  = Data_array
module List   = Data_list
module String = Data_string
module Iter   = Data_iter

open Base

type 'a t = {
  root   : 'a node array;   (* Root node links. *)
  tail   : 'a array;        (* A pointer to the tail of the vector. *)
  len    : int;             (* The len of the vector. *)
  shift  : int;             (* Current maximum index shift. *)
} [@@deriving show]

and 'a node =
  | Link of 'a node array   (* Branch node with links to other nodes. *)
  | Data of 'a array        (* Leaf node holds the actual data values. *)
  [@@deriving show]

let shift_by = 5
let trie_len = int_of_float (2.0 ** float_of_int shift_by)

(* O(1) *)
let empty : 'a t = {
  root   = [| |];
  len = 0;
  tail   = [| |];
  shift  = shift_by;
}

(* O(1) *)
let singleton a = {
  root   = [| |];
  len = 1;
  shift  = shift_by;
  tail   = [| a |];
}

(* O(1) *)
let len v = v.len

let link = function
  | Link a -> a
  | Data _ -> fail "link request on data node"

let data = function
  | Data a -> a
  | Link a -> fail "data request on link node"

let tail_offset len =
  if len < 32 then 0
  else ((len - 1) lsr shift_by) lsl shift_by

let node_with_index v idx =
  if idx >= tail_offset v.len
  then Data v.tail
  else
    let rec loop level node =
      if level = 0 then node
      else
        let sub_idx = (idx lsr level) land (trie_len - 1) in
        loop (level - shift_by) (Array.get_exn (link node) sub_idx) in
    loop (v.shift - shift_by) (Array.get_exn v.root (idx lsr v.shift))

(* O(log32(n)) ~ O(1) *)
let idx v idx =
  let node = node_with_index v idx in
  Array.get_exn (data node) (idx land (trie_len - 1))

(* O(log32(n)) ~ O(1) *)
let rec new_path level tail =
  if level = 0 then Data tail
  else Link [| new_path (level - shift_by) tail |]

(* Pushes the tail array to its correct location in the tree.
   If the parent is a leaf node, add the tail as a Data node.
   If index maps to the existing child, extend the child with a Link to tail.
   Otherwise add a new child to parent with a tail node. *)
let rec push_tail len level (parent : 'a node array) tail : 'a node array =
  let sub_idx = ((len - 1) lsr level) land (trie_len - 1) in
  (* Parent is a leaf node. *)
  if level = shift_by then
    let target  = Data tail in
    let parent' = Array.copy_and_add parent target in
    parent'
  (* Maps to existing child.
     Replace the child with a link to target. *)
  else if sub_idx < Array.len parent then
     let child   = Array.get_exn parent sub_idx in
     let target  = Link (push_tail len (level - shift_by) (link child) tail) in
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
  if v.len = 0 then singleton x
  else
    (* Tail update.
       Tail node has room for another element.
       Duplicate the old tail and add a new element.
       Return the updated vector with incremented len and a new tail. *)
  if v.len land (trie_len - 1) <> 0 then
    { v with len = v.len + 1;
                tail   = Array.copy_and_add v.tail x }
  (* Root overflow
     The current len requires another shift.
     Replace the current root with a new one and add the tail to the tree. *)
  else if v.len lsr shift_by > 1 lsl v.shift then
    { len = v.len + 1;
      shift  = v.shift  + shift_by;
      tail   = [| x |];
      root   = [| Link v.root; new_path v.shift v.tail |] }
  (* Update the tree.
     Push the tail to the root. *)
  else
    { len = v.len + 1;
      shift  = v.shift;
      tail   = [| x |];
      root   = push_tail v.len v.shift v.root v.tail }

let of_list l =
  List.fold (fun v x -> add v x) empty l

let add_from_list init l =
  List.fold (fun v x -> add v x) init l

include Iter.Index.Make(struct
    type nonrec 'a t = 'a t
    let len = len
    let idx = idx
  end)


