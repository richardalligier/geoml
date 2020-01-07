type t
type num = t

val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t

val pow : t -> int -> t

val neg : t -> t
val abs : t -> t

val sqrt : t -> t

val cos : t -> t
val sin : t -> t
val atan2 : t -> t -> t

val random : t -> t

val of_string : string -> t
val to_string : t -> string

val of_int : int -> t
val to_int : t -> int

val print : Format.formatter -> t -> unit

(* syntax shortcuts *)
val ( /. ) : t -> t -> t
val ( +. ) : t -> t -> t
val ( -. ) : t -> t -> t
val ( *. ) : t -> t -> t
val ( ~-. ) : t -> t
val ( ** ) : t -> int -> t
