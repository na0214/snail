(* snailからCへのトランスレータ *)

open Syntax

let rec translate_to_c toplevel context = context toplevel
