(**
   A dendogram is an unrooted tree for visual classification of similarity.
   Here we define its type and common basic functions.
*)

open Genotypes
open ExtLib
open Printf

type leaf = {
	geno : int;          (** Each leaf maps the genotype of index geno *)
	mutable index : int; (** Position of the genotype in the dendogram  *)
}

type node = {
	t1 : tree;
	t2 : tree;
	height : int;
	leaves_nb : int;     (** Number of leaves in the subtree *)
	homology : float;    (** Homology between the t1 and t2  *)
	mutable pos : float; 
	  (** Position of the node along the y-axis in the dendogram *)
}

and tree =
	| Leaf of leaf
	| Node of node

(** A dendogram encapsulates a tree with values needed to print it *)

type t = {
	coll : Genotypes.t;
	tree : tree;
	leaves : leaf list;
}

let create coll tree leaves =
	{ coll = coll;
	  tree = tree;
	  leaves = leaves; }

(*TREE, NODES AND LEAVES FUNCTIONS*)

let height = function
	| Leaf(l) -> 0
	| Node(n) -> n.height

let leaves_nb = function
	| Leaf(l) -> 1
	| Node(n) -> n.leaves_nb

let homology diff geno_size =
	let size = float_of_int geno_size in
	(size -. diff) /. size *. 100.0

let mk_leaf geno =
	let l = { geno = geno; index = 0; } in
	Leaf(l)
	
let mk_node t1 t2 homology =
	{ t1 = t1;
	  t2 = t2;
	  height = (max (height t1) (height t2) ) + 1;
	  leaves_nb = (leaves_nb t1) + (leaves_nb t2);
	  homology = homology;
	  pos = 0.0 }

let get_pos = function
	| Leaf(l) -> float_of_int l.index
	| Node(n) -> n.pos

let get_homology = function
	| Leaf(l) -> 100.0
	| Node(n) -> n.homology

let rec min_homology = function
	| Leaf(l) -> 100.
	| Node(n) ->
		min (min_homology n.t1) (min n.homology (min_homology n.t2))

(* PRINTING *)

(* modifies 2D coordinates *)
type transform = float * float -> float * float

(* string array : hypertext links
   string] : target field in hypertext links *)
type links = string array * string

(* Variables named ddg are dendograms *)

(* Both functions below receive :
    - an abstract IO output
    - a dendogram
    - a transform function applied to coordinates *)

(* Writes the genotypes names and infos on out
   Links are written if provided *)
let write_svg_leaves out ?(links_info = None) ddg trans =
    let write_leaf leaf =
 		let geno = ddg.coll.genos.(leaf.geno) in
		let pos = trans (101., (float_of_int leaf.index)) in
		let infos = Genotype.description geno in
		match links_info with
			| None ->
				  Svg.text out infos pos
			| Some(links,target) ->
				  let link = links.(leaf.geno) in
				  Svg.link out infos ~link:link ~target:target pos
    in List.iter write_leaf ddg.leaves

(* Writes the lines of the dendogram on out *)
let rec write_svg_nodes out ddg trans =
	let rec write_tree = function
		| Leaf(leaf) ->	()
		| Node(node)->
			  let pos1 = get_pos node.t1 in
			  let pos2 = get_pos node.t2 in
			  let x = node.homology in
			  let x1 = get_homology node.t1 in
			  let x2 = get_homology node.t2 in
			  Svg.lines3 out
				  (trans (x1,pos1))
				  (trans (x, pos1))
				  (trans (x, pos2))
				  (trans (x2, pos2));
			  write_tree node.t1;
			  write_tree node.t2
    in write_tree ddg.tree

(*
  SVG Header
  There has to be a javascript function named "Init" further in the svg
*)
let header width height =
    sprintf "<?xml version=\"1.0\" standalone=\"no\"?>
            <svg width=\"%d\" height=\"%d\" version=\"1.1\"
            baseProfile=\"full\"
            xmlns=\"http://www.w3.org/2000/svg\"
            xmlns:xlink=\"http://www.w3.org/1999/xlink\"
            xmlns:ev=\"http://www.w3.org/2001/xml-events\"
            onload=\"Init( evt )\">\n"
	width height

(*
  Writing the svg
  Big ugly function that needs documenting
*)
let write_svg out ?(links_info = None) ddg =
    let height =
		int_of_float (50. +. float_of_int(leaves_nb ddg.tree) *. 25.) in
    let width = 1280 in
	let header = header width height in
    let minH = min_homology ddg.tree in
    
    let trans (x,y) =
		( 9. +. 9. *. (x -. minH) *. 100. /. (100. -. minH),
		  50. +. 25. *. y) in
    let trans_legend (x,y) =  ( 9. +. 9. *. x, 10. +. 13. *. y) in
    let trans_text (x,y) = ( 9. +. 9. *. x, 54.5 +. 25. *. y) in
	
	let javascript =
		"//<![CDATA[\n\nvar svgRoot;\nvar toScale;\nvar toTranslate;\nvar myLine;\nvar myLegend;\n\nvar b_mouse_down = false;\nvar b_drag = false;\n\nvar last_pos = 909;\n\nfunction Init() {\n    svgRoot = document.documentElement;\n    toScale = document.getElementById(\"to_scale\");\n    toTranslate = document.getElementById(\"to_translate\");\n    myLine  = document.getElementById(\"line\");\n    myLegend = document.getElementById(\"legend\");\n}\n\nfunction getMouse(evt) {\n    var pos = svgRoot.createSVGPoint();\n    pos.x = evt.clientX;\n    pos.y = evt.clientY;\n    return pos;\n}\n\t\nfunction onMouseDown(evt) {\n    b_mouse_down = true;\n    if(isPointInVertLine(getMouse(evt), myLine)) {\n\tb_drag = true;\n    }\n}\n\t\nfunction onMouseMove(evt) {\n    if(b_drag) {\n\tdoUpdate(evt);\n    }\n}\n\t\nfunction onMouseUp(evt) {\n    var newP = getMouse(evt);\n    if(b_drag) {\n\t\ttoScale.setAttribute(\"transform\", \"matrix(\" + newP.x / 909 + \" 0 0 1 0 0)\");\n\t\ttoTranslate.setAttribute(\"transform\", \"translate(\" + (-(909 - newP.x)) + \" 0)\");\n\t\t\n\t\t//Update legend (10, 20, .. 100)\n\t\tfor (i=0; i<=10; i++) {\n\t\t\tvar text = document.getElementById(\"Legend\" + i);\n\t\t\tx0 = text.getAttribute(\"x0\");\n\t\t\t//text.setAttribute(\"x\",x0 * newP.x / 909);\n\t\t\torig_pos = 9 + 9 * (i*10);\n\t\t\ttext.setAttribute(\"x\",orig_pos * newP.x / 909);\n\t\t}\n\t\t\n\t\t//Update legend marks\n\t\tfor (i=0; i<=20; i++) {\n\t\t\tvar text = document.getElementById(\"LegendLine\" + i);\n\t\t\torig_pos = 9 + 9 * (i*5);\n\t\t\ttext.setAttribute(\"x1\",orig_pos * newP.x / 909);\n\t\t\ttext.setAttribute(\"x2\",orig_pos * newP.x / 909);\n\t\t}\n\n    }\n    b_mouse_down = false;\n    b_drag = false;\n}\n\t\nfunction doUpdate(evt) {\n    var newP = getMouse(evt);\n    myLine.setAttributeNS(null, \"x1\", newP.x );\n    myLine.setAttributeNS(null, \"x2\", newP.x );\n    \n}\n\t\nfunction isPointInVertLine(p,line) {\n    x = line.getAttribute(\"x1\");\n    return Math.abs(x - p.x) <= 4\n\t}\n\t\n// ]]></script>\n" in
	
	Svg.put out header;
	Svg.put out "<script type=\"text/ecmascript\n\">";
    Svg.put out javascript;
	
    (*SCALE GROUP*)
	
    Svg.put out "<g id=\"to_scale\" transform=\"matrix(1 0 0 1 0 0)\">\n";

    (*Scale line*)
    Svg.line out (trans_legend (-.0.1,1.)) (trans_legend (100.1,1.));
	
    (*Tree*)
	write_svg_nodes out ddg trans;

    Svg.put out "</g>\n";

    (*OTHERS*)
    
	(* Little legend marks (how am I supposed to call them ?)*)
    for i=0 to 20 do
		let x = float_of_int (i*5) in
		let id = sprintf "LegendLine%d" i in
		Svg.line out ~id:id (trans_legend (x,1.)) (trans_legend (x,1.2));
    done;
	
	(*10 20 .. 100*)
    for i=0 to 10 do
		let (x,y) = trans_legend ((float_of_int (i*10)), 2.) in
		Svg.put out
			(sprintf
				 "<text id=\"Legend%d\" x=\"%f\" y=\"%f\" x0=\"%f\" style=\"font-family:Arial;font-size:10px;text-anchor:middle\"> %d </text>\n"
				 i x y x (i*10))
    done;

    (*Title*)
    Svg.text out "Homology percentage" ~size:12 (trans_legend (5.,0.5));

    (*Draggable line*)
    Svg.put out
	(sprintf
	     "<line id=\"line\" x1=\"909\" y1=\"23\" x2=\"909\" y2=\"%d\" style=\"stroke:black;stroke-width:1\" opacity=\"0.5\" />\n"
	     (height-25));
	
    (* Canvas
	   big clickable area for interaction *)
    Svg.put out 
	(sprintf 
	     "<rect id=\"canvas\" x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" opacity=\"0\"
		pointer-events=\"visible\"
		onmousedown=\"onMouseDown(evt)\"
		onmousemove=\"onMouseMove(evt)\"
		onmouseup=\"onMouseUp(evt)\"/>\n"
	     width height);

    (*GROUP TO TRANSLATE*)
    Svg.put out "<g id=\"to_translate\" transform=\"translate(0 0)\">\n";
    
    (*Genotype names*)
    write_svg_leaves out ~links_info:links_info ddg trans_text;
    
    Svg.put out "</g>\n";
    
    Svg.close out, width, height

let write_svg_file file ?(links_info = None) ddg =
	let out = IO.output_channel (open_out file) in
	let _, width, height =
		write_svg out ~links_info:links_info ddg in
	width, height
