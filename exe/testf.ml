open Geoml

let () =
  Format.printf "%a\n" Point.print (Point.make (1./.3.) (0.))
