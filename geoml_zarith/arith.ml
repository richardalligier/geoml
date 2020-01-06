type num = Q.t

let ( *. ) = Q.mul
let ( /. ) = Q.div
let ( +. ) = Q.add
let ( -. ) = Q.sub
let ( ~-. ) = Q.neg

let rec pow (a:num) = function
  | 0 -> Q.one
  | 1 -> a
  | n ->
    let b = pow a (n / 2) in
    b *. b *. (if n mod 2 = 0 then Q.one else a)

let ( ** ) = pow

let ( ~-. ) = (~-.)

let random n = Random.float (Q.to_float n) |> Q.of_float


include Q

let sqrt n = sqrt (Q.to_float n) |> Q.of_float
let atan2 n m = atan2 (Q.to_float n) (Q.to_float m) |> Q.of_float
let sin n = sin (Q.to_float n) |> Q.of_float
let cos n = cos (Q.to_float n) |> Q.of_float
let print = Q.pp_print
