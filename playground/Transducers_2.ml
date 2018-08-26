
module Simple = struct
  type ('a, 'r) reducer =
      Reducer : {
        start  : unit -> 's;
        reduce : 'a -> 's -> 's;
        finish : 's -> 'r;
      } -> ('a, 'r) reducer
end


module Iteratees = struct
  type ('a, 'b) reducer =
    | Done of 'b
    | Await of ('a option -> ('a, 'b) reducer)

  let rec enum_chan iter file_path =
    let rec go iter chan =
      try
        let a = input_line chan in
        match iter with
        | Done _ -> close_in chan; iter
        | Await k -> go (k (Some a)) chan
      with End_of_file ->
        match iter with
        | Done _ -> iter
        | Await k -> k None
    in
    go iter (open_in file_path)

  let head =
    let step input =
      match input with
      | Some a -> Done (Some a)
      | None -> Done None
    in
      Await step

  let lenght =
    let rec step n input =
      match input with
      | Some a -> Await (step (n + 1))
      | None -> Done n
    in
      Await (step 0)

  let to_file path =
    let rec step chan input =
      match input with
      | Some a ->
        output_string chan (a ^ "\n");
        Await (step chan)
      | None ->
        close_out chan;
        Done ()
    in
      Await (step (open_out path))
end

