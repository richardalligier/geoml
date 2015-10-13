type t =
  {
    dx : float ;
    dy : float ;
    
  }

let make dx dy = {dx;dy}

let x_coord (v:t) = v.dx

let y_coord (v:t) = v.dy

let of_points (a: Point.t) (b: Point.t) : t =  make (b.x-.a.x) (b.y-.a.y)

let magnitude ({dx;dy}:t) = sqrt (dx*.dx +. dy*.dy)

let dot_product ({dx=a;dy=b}:t) ({dx=c;dy=d}:t) = a*.c +. b*.d

let scal_mult ({dx;dy}:t) f : t = make (f*.dx) (f*.dy)

let opposite v = scal_mult v (-1.)

let add (v1:t) (v2:t) : t = make (v1.dx+.v2.dx) (v1.dy+.v2.dy) 

let substract v1 v2 = opposite v2 |> add v1

let move_to ({dx;dy}:t) p = Point.translate p dx dy

let angle v1 v2 = 
  (dot_product v1 v2 |> acos) /. ((magnitude v1) *. (magnitude v2))

let angle_deg v1 v2 = 57.2958 *. (angle v1 v2)
  
