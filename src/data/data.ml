module Array   = Data_array
module Char    = Data_char
module Counter = Data_counter
module Either  = Data_either
module Float   = Data_float
module Int     = Data_int
module List    = Data_list
module Opt     = Opt
module Map     = Data_map
module Result  = Data_result
module Stream  = Data_stream
module Str     = Str
module Vec     = PersistentVector
module Iter    = Iter
module Void    = Data_void
module Range    = Range

module Export = struct
  type void = Void.t
  type 'a vec = 'a Vec.t
  let vec = Vec.of_list
end

