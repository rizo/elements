module Array   = Data_array
module Char    = Data_char
module Counter = Data_counter
module Either  = Data_either
module Float   = Data_float
module Int     = Data_int
module List    = Data_list
module Option  = Data_option
module Map     = Data_map
module Result  = Data_result
module Stream  = Data_stream
module String  = Data_string
module Vec     = Data_persistent_vector
module Iter    = Data_iter
module Void    = Data_void
module Range   = Data_range

type void = Void.t

type 'a vec = 'a Vec.t

let vec = Vec.of_list


