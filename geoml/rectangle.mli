open Arith

(** This module provides the basic operations over rectangles *)

type t = private Point.t * num * num
(** rectangle type *)

val make : Point.t -> num -> num -> t
(** make p w h, returns a rectangle where p is the bottom-left
    corner of the rectangle, w its width and h its height *)

 val of_diagonal : Point.t -> Point.t -> t
(**  of_diagonal p1 p2, builds the rectangle where p1p2 is its diagonal
     (the bounding rectangle of the two points)*)

val scale_x : t -> num -> t
val scale_y : t -> num -> t

(** scale on both axis *)
val scale : t -> num -> t

val bottom_left_corner : t -> Point.t
val bottom_right_corner : t -> Point.t
val top_right_corner : t -> Point.t
val top_left_corner : t -> Point.t

val translate : num -> num -> t -> t
val point_reflection : Point.t -> t -> t
val contains : t -> Point.t -> bool
val area : t -> num
val perimeter : t -> num
val proj_x : t -> num * num
val proj_y : t -> num * num

val intersects : t -> t -> bool
(** determines whether or not two rectangles intersect *)

val intersect_line : t -> Line.t -> Point.t list
(** returns the intersection points of a rectangle and a line.
    returns [] if they don't intersect.*)

val segments : t -> Segment.t list
(** returns a list of length 4 containing the segments of the rectangle*)

val is_square : t -> bool
(** tests if the sides of the rectangle have same length *)

val encompass : t -> Point.t -> t
(** given a rectangle and point, returns the smallest rectangle that contains the point and the rectangle given as parameters *)

val bounding : Point.t list -> t
(** given a list of point, returns the smallest rectangle that contains all the points of the list *)

val centroid : t -> Point.t
(** returns the gravity center of a rectangle*)

val random_point : t -> Point.t
(** returns a randomly and uniformly chosen point of the rectangle *)

val print : Format.formatter -> t -> unit
(** printer *)