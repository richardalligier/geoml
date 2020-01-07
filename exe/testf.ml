open Geoml
open[@parse.float] Arith

let () =
  Format.printf "%a\n" Point.print (Point.make (1. /. 3.) 0.)
