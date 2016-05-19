
include Printexc

let fail  = Base.fail
let guard = Base.guard

let show = Printexc.to_string


(** Examples:

    assert (safe "help" (fun () -> input_line stdin) = "help")
 *)
let safe default f =
  try f ()
  with _ -> default


(** Examples:

    assert (catch (fun () -> input_line stdin))
                  (fun e  -> Debug.log (Exn.show e); "")
 *)
let catch f on_exn =
  try f ()
  with e -> on_exn e




