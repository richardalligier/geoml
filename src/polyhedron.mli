(** 
    The module for the polyhedron manipulation.
    We use the term "polyhedron" to define a set of linear constraint. 
    Each constraint defines a half-plan. A polyhedron is then determined by the intersections of all
    the half-plans formed by its constaint. 
    It differs from polygons which represent finite enveloppes (For example, convex polygons are a 
    particular cases of polyhedra, which can be infinite).
*)

type t = private Constraint.t list

val make : Constraint.t list -> t

val contains : t -> Point.t -> bool

val translate : t -> float -> float -> t

val intersection : t -> t -> t
