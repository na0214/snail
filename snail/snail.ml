open Snailml

let _ =
  let in_chan =
    if Array.length Sys.argv = 2 then open_in Sys.argv.(1) else stdin
  in
  Processing.processing in_chan
