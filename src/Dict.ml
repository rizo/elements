
(* Dictionary is an associative container that maps unique keys to not
   necessarily unique values.

   Different implementations of Dict interface exist:

   {ul
   {- [Hashtable]}
   }*)

(* type 'a printer = Buffer.t -> 'a -> unit *)
(* type 'a formatter = Format.formatter -> 'a -> unit *)

(* module type OrderedType = Shadow_stdlib.Map.OrderedType *)

(* module type S = sig *)
(*   include MoreLabels.Map.S *)

(*   val find_opt : key -> 'a t -> 'a option *)
(*   val find_def : key -> 'a -> 'a t -> 'a *)

(*   val update : key -> f: ('a option -> 'a option) -> 'a t -> 'a t *)

(*   val add_multi : 'a list t -> key:key -> data:'a -> 'a list t *)

(*   val of_assoc  : (key * 'a) list -> 'a t *)
(*   val to_assoc  : 'a t -> (key * 'a) list *)
(*   val add_assoc : 'a t -> (key * 'a) list -> 'a t *)

(*   val pp : ?start:string -> ?stop:string -> ?arrow:string -> ?sep:string -> *)
(*             key printer -> 'a printer -> 'a t printer *)

(*   val print : ?start:string -> ?stop:string -> ?arrow:string -> ?sep:string -> *)
(*             key formatter -> 'a formatter -> 'a t formatter *)
(* end *)

(* module Make(K : Comparable.Base) : sig *)
(*   include S with type 'a t = 'a MoreLabels.Map.Make(K).t *)
(*              and type key = K.t *)
(* end = struct *)
(*   include MoreLabels.Map.Make(K) *)

(*   let find_opt k m = *)
(*     try Some (find k m) *)
(*     with Not_found -> None *)

(*   let find_def k default m = *)
(*     try find k m *)
(*     with Not_found -> default *)

(*   let update k ~f m = *)
(*     let x = *)
(*       try f (Some (find k m)) *)
(*       with Not_found -> f None *)
(*     in *)
(*     match x with *)
(*     | None -> remove k m *)
(*     | Some v' -> add k v' m *)

(*   let values map = *)
(*     Shadow_stdlib.List.rev (fold map ~init:[] ~f:(fun ~key ~data acc -> data::acc)) *)

(*   let to_assoc map = *)
(*     Shadow_stdlib.List.rev (fold map ~init:[] ~f:(fun ~key ~data acc -> (key, data)::acc)) *)

(*   let add_assoc m l = *)
(*     Shadow_stdlib.List.fold_left (fun m (k,v) -> add k v m) m l *)

(*   let of_assoc a = add_assoc empty a *)

(*   let add_multi m ~key ~data:el = *)
(*     let prev_el_list = find_def key [] m in *)
(*     add ~key ~data:(el :: prev_el_list) m *)

(*   let of_gen g = *)
(*     let rec loop acc = *)
(*       match g () with *)
(*       | Some (k, v) -> *)
(*         let acc = *)
(*           begin match find_opt k acc with *)
(*             | Some old_list -> add acc ~key:k ~data:(v::old_list) *)
(*             | None          -> add acc ~key:k ~data:(v::[]) *)
(*           end in *)
(*         loop acc *)
(*       | None -> acc in *)
(*     loop empty *)

(*   let aggregate_list ~by:f ~init ~f:op l = *)
(*     Shadow_stdlib.List.fold_left l ~init:empty ~f:(fun acc elem -> *)
(*         let (key, data) = f elem in *)
(*         match find_opt key acc with *)
(*         | Some old -> add acc ~key ~data:(op data old) *)
(*         | None -> add acc ~key ~data:(op data init)) *)

(*   let pp ?(start="{") ?(stop="}") ?(arrow="->") ?(sep=", ") pp_k pp_v buf m = *)
(*     let first = ref true in *)
(*     Buffer.add_string buf start; *)
(*     iter *)
(*       ~f:(fun ~key:k ~data:v -> *)
(*         if !first then first := false else Buffer.add_string buf sep; *)
(*         pp_k buf k; *)
(*         Buffer.add_string buf arrow; *)
(*         pp_v buf v *)
(*       ) m; *)
(*     Buffer.add_string buf stop *)

(*   let print ?(start="[") ?(stop="]") ?(arrow="->") ?(sep=", ") pp_k pp_v fmt m = *)
(*     Format.pp_print_string fmt start; *)
(*     let first = ref true in *)
(*     iter *)
(*       ~f:(fun ~key:k ~data:v -> *)
(*         if !first then first := false else ( *)
(*           Format.pp_print_string fmt sep; *)
(*           Format.pp_print_cut fmt ()); *)
(*         pp_k fmt k; *)
(*         Format.pp_print_string fmt arrow; *)
(*         pp_v fmt v) m; *)
(*     Format.pp_print_string fmt stop *)
(* end *)

(* module Int   = Make(Int) *)
(* module Float = Make(Float) *)


(* module type Counter = sig *)
(*   include Shadow_stdlib.Map.S *)
(*   val incr : ?by: int -> key: key -> int t -> int t *)
(*   val decr : ?by: int -> key: key -> int t -> int t *)
(*   val reset : key: key -> int t -> int t *)
(* end *)

(* module Make(K : Shadow_stdlib.Map.OrderedType) : S *)
(*   with type 'a t = 'a Shadow_stdlib.Map.Make(K).t *)
(*    and type key = K.t = *)
(* struct *)
(*   include Shadow_stdlib.Map.Make(K) *)

(*   let incr ?(by = 1) ~key m = *)
(*     let n = find_def key 1 m in *)
(*     if n + by <= 0 *)
(*     then remove key m *)
(*     else add m ~key ~data:(n + by) *)

(*   let decr ?(by = 1) ~key m = *)
(*     let n = find_def key (- 1) m in *)
(*     if n - by <= 0 *)
(*     then remove key m *)
(*     else add m ~key ~data:(n - by) *)

(*   let reset ~key m = *)
(*     remove key m *)
(* end *)


