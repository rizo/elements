#trace open_in;;
#trace open_out;;

#trace close_in;;
#trace close_out;;

open Proto
open Proto.Data
open Proto.IO

let x =
  File.iter "/var/log/system.log"
  |> Iter.map String.length
  |> Iter.map (fun n -> Int.to_string (n / 1) ^ "\n")
  |> Iter.take 100000000
  |> File.write_lines "/tmp/syslines.txt"

;;
#untrace_all;;

