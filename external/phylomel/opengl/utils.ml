open Sdl
open Video
open Event
open Glcaml
open Printf

open Phylomel
open Vec2

let pi = 4.0 *. (atan 1.0)

let (|>) x f = f x
let (%) f g x = f (g x)

let array_iter2 f a1 a2 =
     if Array.length a1 <> Array.length a2
     then raise (Invalid_argument "Array.iter2");
     for i = 0 to (Array.length a1) - 1 do
       f (Array.unsafe_get a1 i) (Array.unsafe_get a2 i);
     done

let array_exists p xs =
  let n = Array.length xs in
  let rec loop i =
    if i = n then false
    else if p xs.(i) then true
    else loop (succ i)
  in
  loop 0 

let array_map2 f a1 a2 =
	let len = Array.length a1 in
	if len <> Array.length a2 then
		raise (Invalid_argument "Array.map2");
	if len = 0 then [||]
	else (
		let r = Array.create len (f (Array.unsafe_get a1 0) (Array.unsafe_get a2 0)) in
		for i = 1 to len - 1 do
			Array.unsafe_set r i (f (Array.unsafe_get a1 i) (Array.unsafe_get a2 i))
		done;
		r
	)

let perspective fov aspect zNear zFar =
	let fH = (tan (fov /. 360.0 *. pi )) *. zNear in
	let fW = fH *. aspect in 
	glFrustum (-.fW) fW (-.fH) fH zNear zFar

(* A general OpenGL initialization function.  Sets all of the initial parameters. *)
let init_gl width height =
	glViewport 0 0 width height;
	glClearColor 1. 1. 1. 0.;
	glClearDepth 1.;
	glDepthFunc gl_less;
	glEnable gl_depth_test;
	glShadeModel gl_smooth;
	glMatrixMode gl_projection;
	glLoadIdentity ();
	let aspect = (float_of_int width) /. (float_of_int height) in
	perspective 45.0 aspect 1.0 200.0;
	glMatrixMode gl_modelview

let draw_line p1 p2 =
	glBegin(gl_lines);
	glVertex2d p1.x p1.y;
	glVertex2d p2.x p2.y;
	glEnd()

let draw_circle orig_x orig_y radius =
	glColor3f 0. 0. 0.;
	glBegin(gl_polygon);
	let rec draw angle =
		if angle > 2. *. pi then ()
		else (
			let x = orig_x +. radius *. cos(angle) in
			let y = orig_y +. radius *. sin(angle) in
			glVertex2d x y;
			draw (angle +. 0.5)
		) in
	draw 0.;
	glEnd()

let rand_float bound =
	Random.self_init();
	let abs = Random.float bound in
	if Random.bool () then abs
	else -. abs

let rand_vec x y =
	Random.self_init();
	Vec2.make (rand_float x) (rand_float y)
