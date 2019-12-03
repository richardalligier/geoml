open Arith

(** This module provides basic operation over the linear equation type *)

type t = private
	 | X of num
	 | Y of num * num
(** the linear equation type *)

val print : Format.formatter -> t -> unit
(** printer *)

val to_string : t -> string
(** to string *)

type error = | Parallel of t * t
	           | Same_coordinates of Point.t
(** the type of errors *)

exception Error of error
(** the type of exceptions concerning this module *)

val print_error : Format.formatter -> error -> unit
(** error printer*)

val make : num -> num -> num -> t
(** [make a b c] builds a line of equation: ax + by + c = 0*)

val make_x : num -> t
(** [make_x a] builds a line of equation: x = a*)

val make_y : num -> num -> t
(** [make_y a b] builds a line of equation: y = ax + b *)

val x_axis : t
(** the horizontal line of equation: y = 0*)

val y_axis : t
(** the vertical line of equation: x = 0*)

val of_points : Point.t -> Point.t -> t
(** [of_points p1 p2] builds the line that goes through the points p1 and p2.
    It raises [Error(Same_coordinates)] if [p1 = p2] *)

val is_vertical : t -> bool
(** [is_vertical l], is [true] if l has an equation of the form: x=cst,
    where cst is a constant num*)

val is_horizontal: t -> bool
  (** is_horizontal l, returns true if l has an equation of the form: y=cst,
      where cst is a constant num*)

val get_coeff : t -> (num*num*num)
(** returns a tuple (a,b,c) with respect to the equation of line, as: ax + by + c = 0*)

val x_from_y : t -> num -> num
(** 'x_from_y line y', returns 'x', the value on the x-axis corresponding to given 'y' value,
    with f the affine function associated to 'line', as:
    f(x) = y
    raises Parallel if 'line' doesn't intersect the horizontal line going through 'y'
*)

val y_from_x : t -> num -> num
(** 'y_from_x line x', returns 'y', the value on the y-axis corresponding to given 'x' value,
    with f the affine function associated to 'line', as:
    f(x) = y
    raises Parallel if 'line' doesn't intersect the vertical line going through 'x'
*)

val contains : t -> Point.t -> bool
(** contains l1 p returns true if l1goes through p. false otherwise.*)

val scale_x : t -> num -> t

val scale_y : t -> num -> t

val translate : num -> num -> t -> t

val parallel : t -> t -> bool
(** parallel l1 l2 returns true if l1 and l2 are parallel. false otherwise.*)

val intersects : t -> t -> bool
(** intersects l1 l2 returns true if l1 and l2 intersects. false otherwise.*)

val intersection : t -> t -> Point.t
(** intersection l1 l2 returns the point at the intersection of l1 and l2.
    It raises Error(Parralel) if l1 and l2 dont intersect *)

val perpendicular : t -> t -> bool
(** perpendicular l1 l2 returns true if l1 and l2 are perpendicular.
    false otherwise.*)

val perpendicular_of_line : t -> Point.t -> t
(** perpendicular_of_line l p  returns the line perpendicular to l that goes through p.*)

val parallel_of_line : t -> Point.t -> t
(** parallel_of_line l p  returns the line parallel to l that goes through p.*)

val orth_proj : t -> Point.t -> Point.t
(** orth_proj l p, returns the orthogonal projection of p on l*)

val point_bissection : Point.t -> Point.t -> t
(** [point_bissection p1 p2] builds the line [l] that bissects the
   segment p1p2 in its center. It Raises [Error(Same_coordinates)] if [p1 = p2] *)

val arbitrary_point : t -> Point.t
(** [arbitrary_point l] chooses an aribitrary point [p] such that [contains l p] is true *)