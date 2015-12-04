
type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit

module type S = sig
  include Map.S

  val get : key: key -> 'a t -> 'a option

  val update : key: key -> f: ('a option -> 'a option) -> 'a t -> 'a t

  val of_list  : (key * 'a) list -> 'a t
  val to_list  : 'a t -> (key * 'a) list
  val add_list : 'a t -> (key * 'a) list -> 'a t

  val pp : ?start:string -> ?stop:string -> ?arrow:string -> ?sep:string ->
            key printer -> 'a printer -> 'a t printer

  val p : ?start:string -> ?stop:string -> ?arrow:string -> ?sep:string ->
            key formatter -> 'a formatter -> 'a t formatter
end

module Make(O : Map.OrderedType) = struct
  include MoreLabels.Map.Make(O)

  let get k m =
    try Some (find k m)
    with Not_found -> None

  let update k f m =
    let x =
      try f (Some (find k m))
      with Not_found -> f None
    in
    match x with
    | None -> remove k m
    | Some v' -> add k v' m

  let add_list m l = El_list.fold_left ~f:(fun m (k,v) -> add k v m) ~init:m l

  let of_list l = add_list empty l

  let to_list m =
    fold m ~init:[] ~f:(fun ~key ~data acc -> (key, data)::acc)

  let pp ?(start="{") ?(stop="}") ?(arrow="->") ?(sep=", ") pp_k pp_v buf m =
    let first = ref true in
    Buffer.add_string buf start;
    iter
      ~f:(fun ~key:k ~data:v ->
        if !first then first := false else Buffer.add_string buf sep;
        pp_k buf k;
        Buffer.add_string buf arrow;
        pp_v buf v
      ) m;
    Buffer.add_string buf stop

  let print ?(start="[") ?(stop="]") ?(arrow="->") ?(sep=", ") pp_k pp_v fmt m =
    Format.pp_print_string fmt start;
    let first = ref true in
    iter
      ~f:(fun ~key:k ~data:v ->
        if !first then first := false else (
          Format.pp_print_string fmt sep;
          Format.pp_print_cut fmt ()
        );
        pp_k fmt k;
        Format.pp_print_string fmt arrow;
        pp_v fmt v;
      ) m;
    Format.pp_print_string fmt stop
end

