open Arith

type t = num list

let make (l:num list) : t = l

let zero = []

let one = [(of_string "1.")]

let equation p f =
  List.fold_left
    (fun (pow,res) coeff -> (pow+1),(res+.coeff*.(f**pow)))
    (0,(of_string "0."))
    (List.rev p)
 |> snd

let add p1 p2 =
  let rec aux res l1 l2 =
  match l1,l2 with
  | [],[] -> res
  | h1::t1,h2::t2 -> aux ((h1+.h2)::res) t1 t2
  | h::t,_ | _,h::t -> aux (h::res) [] t
  in aux zero (List.rev p1) (List.rev p2)

let derive p =
  let rec aux cur res l =
    match l with
    | [] -> res
    | _::tl when cur = 0 -> aux 1 res tl
    | h::tl -> aux (cur+1) (of_int cur *. h::res) tl
  in aux 0 zero (List.rev p)

let print fmt p =
  let degree = ref 0 in
  Format.pp_print_list
    ~pp_sep:(fun f () -> Format.fprintf f " + ")
    (fun fmt e ->
      if !degree = 0 then
	Format.fprintf fmt "%a" print e
      else if !degree = 1 then
	Format.fprintf fmt "%ax" print e
      else
	Format.fprintf fmt "%ax^%i" print e !degree;
      incr degree;
    )
    fmt
    (List.rev p)
