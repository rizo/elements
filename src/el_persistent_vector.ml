(* Persistent bit-partitioned Vector Trie *)

module Array = El_array
module List  = El_list

let shift_by = 2
let trie_len = int_of_float (2.0 ** float_of_int shift_by)

type 'a t = {
  root   : 'a node array;   (* Root node links. *)
  tail   : 'a array;        (* A pointer to the tail of the vector. *)
  length : int;             (* The length of the vector. *)
  shift  : int;             (* Current maximum index shift. *)
}

and 'a node =
  | Link of 'a node array   (* Branch node with links to other nodes. *)
  | Data of 'a array        (* Leaf node holds the actual data values. *)

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
let is_empty t = t.length = 0

(* O(1) *)
let length t = t.length

let link = function
  | Link a -> a
  | Data _ -> El_exn.fail "link request on data node"

let data = function
  | Data a -> a
  | Link a -> El_exn.fail "data request on link node"

let tail_offset length =
  if length < 32 then 0
  else ((length - 1) lsr shift_by) lsl shift_by

let node_with_index t i =
  if i >= tail_offset t.length then
    Data t.tail
  else
    let rec loop level n =
      if level = 0 then
        Array.get (link n) ((i lsr level) land (trie_len - 1))
      else
        let j = (i lsr level) land (trie_len - 1) in
        loop (level - shift_by) (Array.get (link n) j) in
    loop (t.shift - shift_by) (Array.get t.root (i lsr t.shift))

(* O(log32(n)) ~ O(1) *)
let get t i =
  if i < 0 || i >= length t then None
  else
    let node = node_with_index t i in
    Some (Array.get (data node) (i land (trie_len - 1)))

let rec new_path level tail =
  if level = 0 then Data tail
  else Link [| new_path (level - shift_by) tail |]

let push_tail { length; tail; shift; root } =
  let rec loop level parent =
    let i = ((length - 1) lsr level) land (trie_len - 1) in
    match () with
    | _ when level = shift_by -> Array.add parent (Data tail)
    | _ when i < Array.length root ->
      let next = Array.get parent i in
      let node = loop (level - shift_by) (link next) in
      Array.set parent i (Link node); parent
    | otherwise -> Array.add parent (new_path (level - shift_by) tail)
  in
  loop shift root

(* O(log32(n)) ~ O(1) *)
let add self x =
  if self.length = 0 then singleton x
  else match () with
    (* Tail update *)
    (* Tail node has room for another element. *)
    (* Duplicate the old tail and add a new element. *)
    (* Return the updated vector with incremented length and a new tail. *)
    | _ when self.length land (trie_len - 1) <> 0 ->
      { self with length = self.length + 1;
                  tail   = Array.add self.tail x }

    (* TODO: Test addition with list tail. *)
    (* | _ when self.length land (trie_len - 1) <> 0 -> *)
      (* { self with length = self.length + 1; *)
                  (* tail   = x :: self.tail } *)

    (* Root overflow *)
    (* The current length requires another shift. *)
    (* Replace the current root with a new one and add the tail to the tree. *)
    | _ when self.length lsr shift_by > 1 lsl self.shift ->
      { length = self.length + 1;
        shift  = self.shift  + shift_by;
        tail   = [| x |];
        root   = [| Link self.root; new_path self.shift self.tail |] }

    (* Update the tree. *)
    | otherwise ->
      { length = self.length + 1;
        shift  = self.shift;
        tail   = [| x |];
        root   = push_tail self }

(* TODO: Optimize the intermidiet vecs creation. *)
let of_list l =
  List.fold_left ~f:(fun v x -> add v x) ~init:empty l

