(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: morph3d.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open StdLabels

(*-
 * morph3d.c - Shows 3D morphing objects (TK Version)
 *
 * This program was inspired on a WindowsNT(R)'s screen saver. It was written 
 * from scratch and it was not based on any other source code. 
 * 
 * Porting it to xlock (the final objective of this code since the moment I
 * decided to create it) was possible by comparing the original Mesa's gear
 * demo with it's ported version, so thanks for Danny Sung for his indirect
 * help (look at gear.c in xlock source tree). NOTE: At the moment this code
 * was sent to Brian Paul for package inclusion, the XLock Version was not
 * available. In fact, I'll wait it to appear on the next Mesa release (If you
 * are reading this, it means THIS release) to send it for xlock package 
 * inclusion). It will probably there be a GLUT version too.
 *
 * Thanks goes also to Brian Paul for making it possible and inexpensive
 * to use OpenGL at home.
 *
 * Since I'm not a native english speaker, my apologies for any gramatical
 * mistake.
 *
 * My e-mail addresses are
 *
 * vianna@cat.cbpf.br 
 *         and
 * marcelo@venus.rdc.puc-rio.br
 *
 * Marcelo F. Vianna (Feb-13-1997)
 *)

(*
This document is VERY incomplete, but tries to describe the mathematics used
in the program. At this moment it just describes how the polyhedra are 
generated. On futhurer versions, this document will be probabbly improved.

Since I'm not a native english speaker, my apologies for any gramatical
mistake.

Marcelo Fernandes Vianna 
- Undergraduate in Computer Engeneering at Catholic Pontifical University
- of Rio de Janeiro (PUC-Rio) Brasil.
- e-mail: vianna@cat.cbpf.br or marcelo@venus.rdc.puc-rio.br
- Feb-13-1997

POLYHEDRA GENERATION

For the purpose of this program it's not sufficient to know the polyhedra
vertexes coordinates. Since the morphing algorithm applies a nonlinear 
transformation over the surfaces (faces) of the polyhedron, each face has
to be divided into smaller ones. The morphing algorithm needs to transform 
each vertex of these smaller faces individually. It's a very time consoming
task.

In order to reduce calculation overload, and since all the macro faces of
the polyhedron are transformed by the same way, the generation is made by 
creating only one face of the polyhedron, morphing it and then rotating it
around the polyhedron center. 

What we need to know is the face radius of the polyhedron (the radius of 
the inscribed sphere) and the angle between the center of two adjacent 
faces using the center of the sphere as the angle's vertex.

The face radius of the regular polyhedra are known values which I decided
to not waste my time calculating. Following is a table of face radius for
the regular polyhedra with edge length = 1:

    TETRAHEDRON  : 1/(2*sqrt(2))/sqrt(3)
    CUBE	 : 1/2
    OCTAHEDRON   : 1/sqrt(6)
    DODECAHEDRON : T^2 * sqrt((T+2)/5) / 2     -> where T=(sqrt(5)+1)/2
    ICOSAHEDRON  : (3*sqrt(3)+sqrt(15))/12

I've not found any reference about the mentioned angles, so I needed to
calculate them, not a trivial task until I figured out how :)
Curiously these angles are the same for the tetrahedron and octahedron.
A way to obtain this value is inscribing the tetrahedron inside the cube
by matching their vertexes. So you'll notice that the remaining unmatched
vertexes are in the same straight line starting in the cube/tetrahedron
center and crossing the center of each tetrahedron's face. At this point
it's easy to obtain the bigger angle of the isosceles triangle formed by
the center of the cube and two opposite vertexes on the same cube face.
The edges of this triangle have the following lenghts: sqrt(2) for the base
and sqrt(3)/2 for the other two other edges. So the angle we want is:
     +-----------------------------------------------------------+
     | 2*ARCSIN(sqrt(2)/sqrt(3)) = 109.47122063449069174 degrees |
     +-----------------------------------------------------------+
For the cube this angle is obvious, but just for formality it can be
easily obtained because we also know it's isosceles edge lenghts:
sqrt(2)/2 for the base and 1/2 for the other two edges. So the angle we 
want is:
     +-----------------------------------------------------------+
     | 2*ARCSIN((sqrt(2)/2)/1)   = 90.000000000000000000 degrees |
     +-----------------------------------------------------------+
For the octahedron we use the same idea used for the tetrahedron, but now
we inscribe the cube inside the octahedron so that all cubes's vertexes
matches excatly the center of each octahedron's face. It's now clear that
this angle is the same of the thetrahedron one:
     +-----------------------------------------------------------+
     | 2*ARCSIN(sqrt(2)/sqrt(3)) = 109.47122063449069174 degrees |
     +-----------------------------------------------------------+
For the dodecahedron it's a little bit harder because it's only relationship
with the cube is useless to us. So we need to solve the problem by another
way. The concept of Face radius also exists on 2D polygons with the name
Edge radius:
  Edge Radius For Pentagon (ERp)
  ERp = (1/2)/TAN(36 degrees) * VRp = 0.6881909602355867905
  (VRp is the pentagon's vertex radio).
  Face Radius For Dodecahedron
  FRd = T^2 * sqrt((T+2)/5) / 2 = 1.1135163644116068404
Why we need ERp? Well, ERp and FRd segments forms a 90 degrees angle, 
completing this triangle, the lesser angle is a half of the angle we are 
looking for, so this angle is:
     +-----------------------------------------------------------+
     | 2*ARCTAN(ERp/FRd)	 = 63.434948822922009981 degrees |
     +-----------------------------------------------------------+
For the icosahedron we can use the same method used for dodecahedron (well
the method used for dodecahedron may be used for all regular polyhedra)
  Edge Radius For Triangle (this one is well known: 1/3 of the triangle height)
  ERt = sin(60)/3 = sqrt(3)/6 = 0.2886751345948128655
  Face Radius For Icosahedron
  FRi= (3*sqrt(3)+sqrt(15))/12 = 0.7557613140761707538
So the angle is:
     +-----------------------------------------------------------+
     | 2*ARCTAN(ERt/FRi)	 = 41.810314895778596167 degrees |
     +-----------------------------------------------------------+

*)


let scale = 0.3

let vect_mul (x1,y1,z1) (x2,y2,z2) =
  (y1 *. z2 -. z1 *. y2, z1 *. x2 -. x1 *. z2, x1 *. y2 -. y1 *. x2)

let sqr a = a *. a

(* Increasing this values produces better image quality, the price is speed. *)
(* Very low values produces erroneous/incorrect plotting *)
let tetradivisions =            23
let cubedivisions =             20
let octadivisions =             21
let dodecadivisions =           10
let icodivisions =              15

let tetraangle =                109.47122063449069174
let cubeangle =                 90.000000000000000000
let octaangle =                 109.47122063449069174
let dodecaangle =               63.434948822922009981
let icoangle =                  41.810314895778596167

let pi = acos (-1.)
let sqrt2 = sqrt 2.
let sqrt3 = sqrt 3.
let sqrt5 = sqrt 5.
let sqrt6 = sqrt 6.
let sqrt15 = sqrt 15.
let cossec36_2 = 0.8506508083520399322
let cosd x =  cos (float x /. 180. *. pi)
let sind x =  sin (float x /. 180. *. pi)
let cos72 = cosd 72
let sin72 = sind 72
let cos36 = cosd 36
let sin36 = sind 36

(*************************************************************************)

let front_shininess =   60.0
let front_specular  =   0.7, 0.7, 0.7, 1.0
let ambient         =   0.0, 0.0, 0.0, 1.0
let diffuse         =   1.0, 1.0, 1.0, 1.0
let position0       =   1.0, 1.0, 1.0, 0.0
let position1       =   -1.0,-1.0, 1.0, 0.0
let lmodel_ambient  =   0.5, 0.5, 0.5, 1.0
let lmodel_twoside  =   true

let materialRed     =   0.7, 0.0, 0.0, 1.0
let materialGreen   =   0.1, 0.5, 0.2, 1.0
let materialBlue    =   0.0, 0.0, 0.7, 1.0
let materialCyan    =   0.2, 0.5, 0.7, 1.0
let materialYellow  =   0.7, 0.7, 0.0, 1.0
let materialMagenta =   0.6, 0.2, 0.5, 1.0
let materialWhite   =   0.7, 0.7, 0.7, 1.0
let materialGray    =   0.2, 0.2, 0.2, 1.0
let all_gray = Array.create 20 materialGray

let vertex ~xf ~yf ~zf ~ampvr2 =
  let xa = xf +. 0.01 and yb = yf +. 0.01 in
  let xf2 = sqr xf and yf2 = sqr yf in
  let factor = 1. -. (xf2 +. yf2) *. ampvr2
  and factor1 = 1. -. (sqr xa +. yf2) *. ampvr2
  and factor2 = 1. -. (xf2 +. sqr yb) *. ampvr2 in
  let vertx = factor *. xf and verty = factor *. yf
  and vertz = factor *. zf in
  let neiax = factor1 *. xa -. vertx and neiay = factor1 *. yf -. verty
  and neiaz = factor1 *. zf -. vertz and neibx = factor2 *. xf -. vertx
  and neiby = factor2 *. yb -. verty and neibz = factor2 *. zf -. vertz in
  GlDraw.normal3 (vect_mul (neiax, neiay, neiaz) (neibx, neiby, neibz));
  GlDraw.vertex3 (vertx, verty, vertz)

let triangle ~edge ~amp ~divisions ~z =
  let divi = float divisions in
  let vr = edge *. sqrt3 /. 3. in
  let ampvr2 = amp /. sqr vr
  and zf = edge *. z in
  let ax = edge *. (0.5 /. divi)
  and ay = edge *. (-0.5 *. sqrt3 /. divi)
  and bx = edge *. (-0.5 /. divi) in
  for ri = 1 to divisions do
    GlDraw.begins `triangle_strip;
    for ti = 0 to ri - 1 do
      vertex ~zf ~ampvr2
	~xf:(float (ri-ti) *. ax +. float ti *. bx)
	~yf:(vr +. float (ri-ti) *. ay +. float ti *. ay);
      vertex ~zf ~ampvr2
	~xf:(float (ri-ti-1) *. ax +. float ti *. bx)
	~yf:(vr +. float (ri-ti-1) *. ay +. float ti *. ay)
    done;
    vertex ~xf:(float ri *. bx) ~yf:(vr +. float ri *. ay) ~zf ~ampvr2;
    GlDraw.ends ()
  done

let square ~edge ~amp ~divisions ~z =
  let divi = float divisions in
  let zf = edge *. z
  and ampvr2 = amp /. sqr (edge *. sqrt2 /. 2.) in
  for yi = 0 to divisions - 1 do
    let yf = edge *. (-0.5 +. float yi /. divi) in
    let yf2 = sqr yf in
    let y = yf +. 1.0 /. divi *. edge in
    let y2 = sqr y in
    GlDraw.begins `quad_strip;
    for xi = 0 to divisions do
      let xf = edge *. (-0.5 +. float xi /. divi) in
      vertex ~xf ~yf:y ~zf ~ampvr2;
      vertex ~xf ~yf ~zf ~ampvr2
    done;
    GlDraw.ends ()
  done

let pentagon ~edge ~amp ~divisions ~z =
  let divi = float divisions in
  let zf = edge *. z
  and ampvr2 = amp /. sqr(edge *. cossec36_2) in
  let x =
    Array.init 6
      ~f:(fun fi -> -. cos (float fi *. 2. *. pi /. 5. +. pi /. 10.)
	             /. divi *. cossec36_2 *. edge)
  and y =
    Array.init 6
      ~f:(fun fi -> sin (float fi *. 2. *. pi /. 5. +. pi /. 10.)
	             /. divi *. cossec36_2 *. edge)
  in
  for ri = 1 to divisions do
    for fi = 0 to 4 do
      GlDraw.begins `triangle_strip;
      for ti = 0 to ri-1 do
	vertex ~zf ~ampvr2
	  ~xf:(float(ri-ti) *. x.(fi) +. float ti *. x.(fi+1))
	  ~yf:(float(ri-ti) *. y.(fi) +. float ti *. y.(fi+1));
	vertex ~zf ~ampvr2
	  ~xf:(float(ri-ti-1) *. x.(fi) +. float ti *. x.(fi+1))
	  ~yf:(float(ri-ti-1) *. y.(fi) +. float ti *. y.(fi+1))
      done;
      vertex ~xf:(float ri *. x.(fi+1)) ~yf:(float ri *. y.(fi+1)) ~zf ~ampvr2;
      GlDraw.ends ()
    done
  done

let call_list list color =
  GlLight.material ~face:`both (`diffuse color);
  GlList.call list

let draw_tetra ~amp ~divisions ~color =
  let list = GlList.create `compile in
  triangle ~edge:2.0 ~amp ~divisions ~z:(0.5 /. sqrt6);
  GlList.ends();

  call_list list color.(0);
  GlMat.push();
  GlMat.rotate ~angle:180.0 ~z:1.0 ();
  GlMat.rotate ~angle:(-.tetraangle) ~x:1.0 ();
  call_list list color.(1);
  GlMat.pop();
  GlMat.push();
  GlMat.rotate ~angle:180.0 ~y:1.0 ();
  GlMat.rotate ~angle:(-180.0 +. tetraangle) ~x:0.5 ~y:(sqrt3 /. 2.) ();
  call_list list color.(2);
  GlMat.pop();
  GlMat.rotate ~angle:180.0 ~y:1.0 ();
  GlMat.rotate ~angle:(-180.0 +. tetraangle) ~x:0.5 ~y:(-.sqrt3 /. 2.) ();
  call_list list color.(3);

  GlList.delete list

let draw_cube ~amp ~divisions ~color =
  let list = GlList.create `compile in
  square ~edge:2.0 ~amp ~divisions ~z:0.5;
  GlList.ends ();

  call_list list color.(0);
  for i = 1 to 3 do
    GlMat.rotate ~angle:cubeangle ~x:1.0 ();
    call_list list color.(i)
  done;
  GlMat.rotate ~angle:cubeangle ~y:1.0 ();
  call_list list color.(4);
  GlMat.rotate ~angle:(2.0 *. cubeangle) ~y:1.0 ();
  call_list list color.(5);

  GlList.delete list

let draw_octa ~amp ~divisions ~color =
  let list = GlList.create `compile in
  triangle ~edge:2.0 ~amp ~divisions ~z:(1.0 /. sqrt6);
  GlList.ends ();

  let do_list (i,y) =
    GlMat.push();
    GlMat.rotate ~angle:180.0 ~y:1.0 ();
    GlMat.rotate ~angle:(-.octaangle) ~x:0.5 ~y ();
    call_list list color.(i);
    GlMat.pop()
  in
  call_list list color.(0);
  GlMat.push();
  GlMat.rotate ~angle:180.0 ~z:1.0 ();
  GlMat.rotate ~angle:(-180.0 +. octaangle) ~x:1.0 ();
  call_list list color.(1);
  GlMat.pop();
  List.iter [2, sqrt3 /. 2.0; 3, -.sqrt3 /. 2.0] ~f:do_list;
  GlMat.rotate ~angle:180.0 ~x:1.0 ();
  GlLight.material ~face:`both (`diffuse color.(4));
  GlList.call list;
  GlMat.push();
  GlMat.rotate ~angle:180.0 ~z:1.0 ();
  GlMat.rotate ~angle:(-180.0 +. octaangle) ~x:1.0 ();
  GlLight.material ~face:`both (`diffuse color.(5));
  GlList.call list;
  GlMat.pop();
  List.iter [6, sqrt3 /. 2.0; 7, -.sqrt3 /. 2.0] ~f:do_list;

  GlList.delete list

let draw_dodeca ~amp ~divisions ~color =
  let tau = (sqrt5 +. 1.0) /. 2.0 in
  let list = GlList.create `compile in
  pentagon ~edge:2.0 ~amp ~divisions
    ~z:(sqr(tau) *. sqrt ((tau+.2.0)/.5.0) /. 2.0);
  GlList.ends ();

  let do_list (i,angle,x,y) =
    GlMat.push();
    GlMat.rotate ~angle:angle ~x ~y ();
    call_list list color.(i);
    GlMat.pop();
  in
  GlMat.push ();
  call_list list color.(0);
  GlMat.rotate ~angle:180.0 ~z:1.0 ();
  List.iter ~f:do_list
    [ 1, -.dodecaangle, 1.0, 0.0;
      2, -.dodecaangle, cos72, sin72;
      3, -.dodecaangle, cos72, -.sin72;
      4, dodecaangle, cos36, -.sin36;
      5, dodecaangle, cos36, sin36 ];
  GlMat.pop ();
  GlMat.rotate ~angle:180.0 ~x:1.0 ();
  call_list list color.(6);
  GlMat.rotate ~angle:180.0 ~z:1.0 ();
  List.iter ~f:do_list
    [ 7, -.dodecaangle, 1.0, 0.0;
      8, -.dodecaangle, cos72, sin72;
      9, -.dodecaangle, cos72, -.sin72;
      10, dodecaangle, cos36, -.sin36 ];
  GlMat.rotate ~angle:dodecaangle ~x:cos36 ~y:sin36 ();
  call_list list color.(11);

  GlList.delete list

let draw_ico ~amp ~divisions ~color =
  let list = GlList.create `compile in
  triangle ~edge:1.5 ~amp ~divisions
    ~z:((3.0 *. sqrt3 +. sqrt15) /. 12.0);
  GlList.ends ();

  let do_list1 i =
    GlMat.rotate ~angle:180.0 ~y:1.0 ();
    GlMat.rotate ~angle:(-180.0 +. icoangle) ~x:0.5 ~y:(sqrt3/.2.0) ();
    call_list list color.(i)
  and do_list2 i =
    GlMat.rotate ~angle:180.0 ~y:1.0 ();
    GlMat.rotate ~angle:(-180.0 +. icoangle) ~x:0.5 ~y:(-.sqrt3/.2.0) ();
    call_list list color.(i)
  and do_list3 i =
    GlMat.rotate ~angle:180.0 ~z:1.0 ();
    GlMat.rotate ~angle:(-.icoangle) ~x:1.0 ();
    call_list list color.(i)
  in
  GlMat.push ();
  call_list list color.(0);
  GlMat.push ();
  do_list3 1;
  GlMat.push ();
  do_list1 2;
  GlMat.pop ();
  do_list2 3;
  GlMat.pop ();
  GlMat.push ();
  do_list1 4;
  GlMat.push ();
  do_list1 5;
  GlMat.pop();
  do_list3 6;
  GlMat.pop ();
  do_list2 7;
  GlMat.push ();
  do_list2 8;
  GlMat.pop ();
  do_list3 9;
  GlMat.pop ();
  GlMat.rotate ~angle:180.0 ~x:1.0 ();
  call_list list color.(10);
  GlMat.push ();
  do_list3 11;
  GlMat.push ();
  do_list1 12;
  GlMat.pop ();
  do_list2 13;
  GlMat.pop ();
  GlMat.push ();
  do_list1 14;
  GlMat.push ();
  do_list1 15;
  GlMat.pop ();
  do_list3 16;
  GlMat.pop ();
  do_list2 17;
  GlMat.push ();
  do_list2 18;
  GlMat.pop ();
  do_list3 19;

  GlList.delete list

class view area = object (self)
  val area : GlGtk.area = area
  val mutable smooth = true
  val mutable step = 0.
  val mutable obj = 1
  val mutable draw_object = fun ~amp -> ()
  val mutable magnitude = 0.

  method width =  area#misc#allocation.Gtk.width
  method height = area#misc#allocation.Gtk.height

  method draw () =
    let ratio = float self#height /. float self#width in
    GlClear.clear [`color;`depth];
    GlMat.push();
    GlMat.translate ~z:(-10.0) ();
    GlMat.scale ~x:(scale *. ratio) ~y:scale ~z:scale ();
    GlMat.translate ()
      ~x:(2.5 *. ratio *. sin (step *. 1.11))
      ~y:(2.5 *. cos (step *. 1.25 *. 1.11));
    GlMat.rotate ~angle:(step *. 100.) ~x:1.0 ();
    GlMat.rotate ~angle:(step *. 95.) ~y:1.0 ();
    GlMat.rotate ~angle:(step *. 90.) ~z:1.0 ();
    draw_object ~amp:((sin step +. 1.0/.3.0) *. (4.0/.5.0) *. magnitude);
    GlMat.pop();
    Gl.flush();
    area#swap_buffers ();
    step <- step +. 0.05

  method reshape ~width ~height =
    GlDraw.viewport ~x:0 ~y:0 ~w:width ~h:height;
    GlMat.mode `projection;
    GlMat.load_identity();
    GlMat.frustum ~x:(-1.0, 1.0) ~y:(-1.0, 1.0) ~z:(5.0, 15.0);
    GlMat.mode `modelview

  method key sym =
    begin match sym with
      "1" -> obj <- 1
    | "2" -> obj <- 2
    | "3" -> obj <- 3
    | "4" -> obj <- 4
    | "5" -> obj <- 5
    | "\r" -> smooth <- not smooth
    | "\027" -> area#misc#toplevel#destroy (); exit 0
    | _ -> ()
    end;
    self#pinit

  method pinit =
    begin match obj with
      1 ->
	draw_object <- draw_tetra
	     ~divisions:tetradivisions
	     ~color:[|materialRed;  materialGreen;
		     materialBlue; materialWhite|];
	magnitude <- 2.5
    | 2 ->
	draw_object <- draw_cube
	     ~divisions:cubedivisions
	     ~color:[|materialRed; materialGreen; materialCyan;
		     materialMagenta; materialYellow; materialBlue|];
	magnitude <- 2.0
    | 3 ->
	draw_object <- draw_octa
	     ~divisions:octadivisions
	     ~color:[|materialRed; materialGreen; materialBlue;
		     materialWhite; materialCyan; materialMagenta;
		     materialGray; materialYellow|];
	magnitude <- 2.5
    | 4 ->
      draw_object <- draw_dodeca
	   ~divisions:dodecadivisions
	   ~color:[|materialRed; materialGreen; materialCyan;
		   materialBlue; materialMagenta; materialYellow;
		   materialGreen; materialCyan; materialRed;
		   materialMagenta; materialBlue; materialYellow|];
      magnitude <- 2.0
    | 5 ->
	draw_object <- draw_ico
	     ~divisions:icodivisions
	     ~color:[|materialRed; materialGreen; materialBlue;
		     materialCyan; materialYellow; materialMagenta;
		     materialRed; materialGreen; materialBlue;
		     materialWhite; materialCyan; materialYellow;
		     materialMagenta; materialRed; materialGreen;
		     materialBlue; materialCyan; materialYellow;
		     materialMagenta; materialGray|];
	magnitude <- 3.5
    | _ -> ()
    end;
    GlDraw.shade_model (if smooth then `smooth else `flat)
  initializer
    area#connect#display ~callback:self#draw;
    area#connect#reshape ~callback:self#reshape;
    ()
end

open GMain

let main () =
  List.iter ~f:print_string
    [ "Morph 3D - Shows morphing platonic polyhedra\n";
      "Author: Marcelo Fernandes Vianna (vianna@cat.cbpf.br)\n";
      "Ported to LablGL by Jacques Garrigue\n\n";
      "  [1]    - Tetrahedron\n";
      "  [2]    - Hexahedron (Cube)\n";
      "  [3]    - Octahedron\n";
      "  [4]    - Dodecahedron\n";
      "  [5]    - Icosahedron\n";
      "[RETURN] - Toggle smooth/flat shading\n";
      " [ESC]   - Quit\n" ];
  flush stdout;

  let window =
    GWindow.window ~title:"Morph 3D - Shows morphing platonic polyhedra" ()
  in
  window#connect#destroy ~callback:Main.quit;
  window#set_resize_mode `IMMEDIATE;

  let area = GlGtk.area [`DEPTH_SIZE 1;`RGBA;`DOUBLEBUFFER]
      ~width:640 ~height:480 ~packing:window#add () in

  let view = new view area in

  area#connect#realize ~callback:
    begin fun () ->
      view#pinit;
      GlClear.depth 1.0;
      GlClear.color (0.0, 0.0, 0.0);
      GlDraw.color (1.0, 1.0, 1.0);

      GlClear.clear [`color;`depth];
      Gl.flush();

      List.iter ~f:(GlLight.light ~num:0)
	[`ambient ambient; `diffuse diffuse; `position position0];
      List.iter ~f:(GlLight.light ~num:1)
	[`ambient ambient; `diffuse diffuse; `position position1];
      GlLight.light_model (`ambient lmodel_ambient);
      GlLight.light_model (`two_side lmodel_twoside);
      List.iter ~f:Gl.enable
	[`lighting;`light0;`light1;`depth_test;`normalize];

      GlLight.material ~face:`both (`shininess front_shininess);
      GlLight.material ~face:`both (`specular front_specular);

      GlMisc.hint `fog `fastest;
      GlMisc.hint `perspective_correction `fastest;
      GlMisc.hint `polygon_smooth `fastest
    end;

  window#event#connect#key_press
    ~callback:(fun ev -> view#key (GdkEvent.Key.string ev); true);

  Timeout.add ~ms:20
    ~callback:(fun _ -> if area#misc#visible then view#draw (); true);
  window#show ();
  Main.main ()

let _ = main ()
