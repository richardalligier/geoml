
type t = {a:float;b:float;c:float}
  
type told = X of float | Y of float * float (** linear equation type *)

let make a b c =
  {a;b;c}

let makeold {a;b;c} =
  if b = 0. then X(-.c/.a)
  else Y((-.a/.b),(-.c/.b))

let print fmt l =
  (function
  | Y(a,b) -> Format.fprintf fmt "y=%fx +. %f" a b
  | X(c) -> Format.fprintf fmt "x=%f" c) (makeold l)

type error = Parallel of t * t | Same_coordinates of Point.t

exception Error of error

let print_error fmt e =
  let open Format in
  match e with
  | Parallel(l1,l2) -> fprintf fmt "Bad arguments : parallel lines %a and %a" print l1 print l2
  | Same_coordinates p -> fprintf fmt "Bad arguments : same coordinates %a" Point.print p


let make_x f = {a=1.;b=0.;c= -.f}

let make_y a b = {a= -.a;b=1.;c= -.b}

let x_axis = {a=0.;b=1.;c=0.}

let y_axis = {a=1.;b=0.;c=0.}

let is_vertical l = l.a = 0.

let is_horizontal l = l.b = 0.

let get_coeff {a;b;c}= a,b,c

let to_string l = (function
  | X(a) -> ("x="^(string_of_float a))
  | Y(a,b) -> ("y="^(string_of_float a)^"x+"^(string_of_float b))) (makeold l)

let of_points (p1:Point.t) (p2:Point.t) =
  let open Point in
  if p1 = p2 then
    raise (Error (Same_coordinates p1))
  else
    let dx = p2.x -. p1.x in
    let dy = p2.y -. p1.y in
    {a = -.dy; b = dx; c = -.p1.y *. dx +. p1.x *. dy}

    (*if p1.x = p2.x then
    X(p1.x)
  else
    let coeff = (p2.y -. p1.y) /. (p2.x -. p1.x) in
    let ord = p1.y -. coeff *. (p1.x)
    in Y(coeff,ord)*)

let x_from_y ({a;b;c} as l) y =
  if a = 0.
  then  raise (Error (Parallel(l,{a=0.;b=1.;c= -.y})))
  else (-.b*.y -. c) /. a

let y_from_x ({a;b;c} as l) x =
  if b = 0.
  then raise (Error (Parallel(l,(make_x x))))
  else (-.a*.x -. c) /. b

let eval {a;b;c} p =
  let open Point in
  a *. p.x +. b *. p.y +. c

let contains (l:t) (p:Point.t) =
  eval l p = 0.

let scale_x l f =
  {l with a = l.a *. f}

let scale_y l f =
  {l with b = l.b *. f}

let translate dx dy ({a;b;c} as l) =
  {l with c = c -. a *. dx -. b *. dy}

let parallel l1 l2 =
  Vector.determinant (Vector.make l1.a l1.b) (Vector.make l2.a l2.b) = 0.

let intersects l1 l2 = parallel l1 l2 |> not

let intersection l1 l2 =
  let open Lacaml.D in
  let a = Mat.of_array
            [| [| l1.a; l1.b|];
               [| l2.a; l2.b|]
            |]
  in
  let b = Vec.of_array [| -.l1.c; -.l2.c |] in
  let x = copy b in
  gesv a (Mat.from_col_vec x);
  let x = Vec.to_array x in
  Point.make x.(0) x.(1)
(*  if parallel l1 l2
  then raise (Error (Parallel(l1,l2)))
  else*)
(*
  match (makeold l1),(makeold l2) with
  | Y(a1,b1),Y(a2,b2) when a1<>a2->
     let x = (b2 -. b1) /. (a1 -. a2) in
     let y = a1 *. x +. b1 in
     Point.make x y
  | Y(a,b), X(x) | X(x), Y(a,b) ->
     let y = a *. x +. b in Point.make x y
  | _  -> raise (Error (Parallel(l1,l2)))
          *)

let perpendicular l1 l2 =
  Vector.dot_product (Vector.make l1.a l1.b) (Vector.make l2.a l2.b) = 0.

let perpendicular_of_line l p =
  let open Point in
  {a= -.l.b;b = l.a; c = -.(-.l.b *. p.x +. l.a *. p.y)}

let parallel_of_line l p =
  let open Point in
  {l with c = -.(l.a *. p.x +. l.b *. p.y)}

let orth_proj l p =
  perpendicular_of_line l p |> intersection l

let point_bissection p1 p2 =
  Point.center p1 p2 |> perpendicular_of_line (of_points p1 p2)

let arbitrary_point l =
  match (makeold l) with
  | X(c) -> Point.make c 0.
  | Y(_,b) -> Point.make 0. b
