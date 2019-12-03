open Arith

(** Ellipses manipulation *)

type t = private {f1 : Point.t; f2 : Point.t; radius : num
		 ; big_axis : num; small_axis : num}

val make_bifocal : Point.t -> Point.t -> num -> t

val focal1 : t -> Point.t
val focal2 : t -> Point.t
val center : t -> Point.t
val radius : t -> num
val big_axis : t -> num
val small_axis : t -> num

(** radian rotation. rotate e p f returns the rotated ellipse with p
    as the rotation center and f a angle in radian *)
val rotate : t -> Point.t -> num -> t

(** degree rotation. rotate e p f returns the rotated ellipse with p
    as the rotation center and f a angle in degree *)
val rotate_angle : t -> Point.t -> num -> t

val translate : num -> num -> t -> t
val scale_x : t -> num -> t
val scale_y : t -> num -> t
val contains : t -> Point.t -> bool
val area : t -> num
