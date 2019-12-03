open Arith

(** Module for 2d points manipulation *)

type t = private {
  	x : num;
  	y : num;
}

val make : num -> num -> t

val ( % ) : num -> num -> t

type point = t

module Tbl: Hashtbl.S with type key = t

val orig : t

val center : t -> t -> t

val determinant : t -> t -> t -> num

val iso_barycenter : t list -> t

val barycenter : (t * num) list -> t

val sq_distance : t -> t -> num

val distance : t -> t -> num

val x_coord : t -> num

val y_coord : t -> num

val scale_x : t -> num -> t

val scale_y : t -> num -> t

val translate : num -> num -> t -> t

val transform : Affine.t -> t -> t

(** point reflection. reflection p1 p2 returns the symerical point of p2 with respect to p1 *)
val point_reflection : t -> t -> t

(** point rotation. rotate p1 p2 f returns the rotation point of p2 with p1 as center and f a angle in radian *)
val rotate : t -> t -> num -> t

(** point rotation. rotate p1 p2 f returns the rotation point of p2 with p1 as center and f a angle in degree *)
val rotate_angle : t -> t -> num -> t

val print : Format.formatter -> t -> unit
