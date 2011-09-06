open Printf

type t = { mutable x:float; mutable y:float }

let make x y =
	{ x = x;
	  y = y; }

let null () =
	{ x = 0.;
	  y = 0.; }

let norm {x=x; y=y} =
	sqrt (x*.x +. y*.y)

let add {x=x1; y=y1} {x=x2; y=y2} =
	{ x = x1 +. x2;
	  y = y1 +. y2 }

let sub {x=x1; y=y1} {x=x2; y=y2} =
	{ x = x1 -. x2;
	  y = y1 -. y2 }

let scal_mul s {x=x; y=y} =
	{ x = s *. x;
	  y = s *. y; }

let scal_div s {x=x; y=y} =
	{ x = x /. s;
	  y = y /. s; }

let doAdd ~on { x=dx; y=dy } =
	on.x <- on.x +. dx;
	on.y <- on.y +. dy

let doSub ~on { x=dx; y=dy } =
	on.x <- on.x -. dx;
	on.y <- on.y -. dy
	
let addX dx { x=x; y=y } =
	{ x = x +. dx;
	  y = y }

let addY dy {x=x; y=y} =
	{ x = x;
	  y = y +. dy }

let subX dx {x=x; y=y} =
	{ x = x -. dx;
	  y = y }

let subY dy {x=x; y=y} =
	{ x = x;
	  y = y -. dy }

let copy {x=x; y=y} =
	{x=x; y=y}

let unit_vec angle =
	make (cos angle) (sin angle)
