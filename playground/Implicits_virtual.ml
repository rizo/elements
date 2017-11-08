module Int = struct
  type t = int
end

module type Show = sig
  type t
  val show : t -> string
end

let show {M : Show} x = M.show x


implicit module Show_poly_list {X : sig type t end} = struct
     type t = X.t list
     let show (l : t) =
       "[" ^ string_of_int (List.length l) ^ " elements]"
end

let print {S : Show} (x : S.t) = print_endline (show x)

let () =
  print [1; 2; 3; 4]

