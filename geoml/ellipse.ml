open Math
open Arith


type t = {f1 : Point.t; f2 : Point.t; radius : num;
	  big_axis : num; small_axis : num}

let make_bifocal p1 p2 radius =
  let p1p2 = Point.distance p1 p2 in
  let big_axis = p1p2 +. (of_string "2.") *. radius in
  let a = p1p2 /. (of_string "2.") in
  let c = ((p1p2 +. radius) /. (of_string "2.")) in
  let b = sqrt (c *. c -. a *. a) in
  { f1=p1; f2=p2; radius
  ; big_axis ; small_axis = b *. (of_string "2.") }

let focal1 {f1;_} = f1
let focal2 {f2;_} = f2
let radius {radius;_} = radius
let center {f1;f2;_} = Point.center f1 f2
let big_axis {big_axis;_} = big_axis
let small_axis {small_axis;_} = small_axis

let translate dx dy {f1; f2; radius;_} =
  make_bifocal (Point.translate dx dy f1) (Point.translate dx dy f2) radius

let rotate e c f =
  {e with
    f1=Point.rotate e.f1 c f;
    f2=Point.rotate e.f2 c f}

let rotate_angle e c f =
  {e with
    f1=Point.rotate_angle e.f1 c f;
    f2=Point.rotate_angle e.f2 c f}

let scale_x {f1;f2;radius;_} f =
  let open Point in
  let new_radius =
    if f1.y = f2.y then radius *. f
    else if f1.x = f2.x then radius
    else
      let ratio = (abs (f1.x -. f2.x)) /. (abs (f1.y -. f2.y)) in
      radius *. ratio
  in
  let f1 = Point.scale_x f1 f and f2 =Point.scale_x f2 f in
  make_bifocal f1 f2 new_radius

let scale_y {f1;f2;radius;_} f =
  let open Point in
  let new_radius =
    if f1.x = f2.x then radius *. f
    else if f1.y = f2.y then radius
    else
      let ratio = (abs (f1.y -. f2.y)) /. (abs (f1.x -. f2.x)) in
      radius *. ratio
  in
  let f1 = Point.scale_y f1 f and f2 =Point.scale_y f2 f in
  make_bifocal f1 f2 new_radius

let contains {f1; f2; radius; _} p =
  let a = Point.distance f1 f2 +. radius in
  a >= Point.distance f1 p +. Point.distance f2 p

let area {big_axis; small_axis; _} =
  pi *. (big_axis /. (of_string "2.")) *. (small_axis /. (of_string "2."))