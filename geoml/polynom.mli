open Arith

type t

val make : num list -> t

val zero : t

val one : t

val equation : t -> num -> num

val add : t -> t -> t

val derive : t -> t

val print : Format.formatter -> t -> unit
