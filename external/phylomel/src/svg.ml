open Printf

let header width height =
    sprintf "<?xml version=\"1.0\" standalone=\"no\"?>
            <svg width=\"%d\" height=\"%d\" version=\"1.1\"
            baseProfile=\"full\"
            xmlns=\"http://www.w3.org/2000/svg\"
            xmlns:xlink=\"http://www.w3.org/1999/xlink\"
            xmlns:ev=\"http://www.w3.org/2001/xml-events\">\n"
	width height

let rgb x y z =
    sprintf "rgb(%f,%f,%f)" x y z

let put = IO.nwrite

let close out =
	put out "</svg>";
	IO.close_out out
	    
let line out ?(id="") ?(color="black") ?(width=2.) (x1,y1) (x2,y2) =
	put out
    (sprintf
	"<line id=\"%s\" x1=\"%.2f\" y1=\"%.2f\" x2=\"%.2f\" y2=\"%.2f\" style=\"stroke:%s;stroke-width:%f\"/>\n"
	id x1 y1 x2 y2 color width)

let str_of_list =
	let soi = string_of_int in
	let rec print' acc = function
		| [x] -> acc ^ (soi x)
		| h::t ->
			  let acc = acc ^ (soi h) ^ ", " in
			  print' acc t
		| [] -> "" in
	print' ""

let dotted_line out
	?(id="")
	?(color="black")
	?(width=2.)
	?(dashes=[9;5])
	(x1,y1) (x2,y2) =
		put out
		(sprintf
		 "<line id=\"%s\" x1=\"%.2f\" y1=\"%.2f\" x2=\"%.2f\" y2=\"%.2f\" style=\"stroke:%s;stroke-width:%f;stroke-dasharray:%s\"/>\n"
		 id x1 y1 x2 y2 color width (str_of_list dashes))
			
let lines2 out ?(id="") ?(color="black") ?(width=2.)
		   (x1,y1) (x2,y2) (x3,y3) =
	put out
    (sprintf
	"<polyline id=\"%s\" points=\"%.2f,%.2f %.2f,%.2f %.2f,%.2f\" style=\"fill:none;stroke:%s;stroke-width:%f\"/>\n"
	id x1 y1 x2 y2 x3 y3 color width)
		
let lines3 out ?(id="") ?(color="black") ?(width=2.) (x1,y1) (x2,y2) (x3,y3) (x4,y4) =
	put out
    (sprintf
	"<polyline id=\"%s\" points=\"%.2f,%.2f %.2f,%.2f %.2f,%.2f %.2f,%.2f\" style=\"fill:none;stroke:%s;stroke-width:%f\"/>\n"
	id x1 y1 x2 y2 x3 y3 x4 y4 color width)

let point out (x,y) =
	put out (sprintf "%.2f,%.2f " x y)
		
let lines out ?(id="") ?(color="black") ?(width=2.) points =
    put out (sprintf "<polyline id=\"%s\" points=\"" id);
    Array.iter (point out) points;
    put out "\" style=\"fill:none;stroke:black;stroke-width:2\"/>\n"
	
let polygon out ?(id="") ?(fill="none") ?(stroke="black") ?(width=2.) points =
    put out (sprintf "\t<polygon id=\"%s\" points=\"" id);
    Array.iter (point out) points;
    put out (sprintf "\" style=\"fill:%s;stroke:%s;stroke-width:%f\"/>\n" fill stroke width)

let circle out ?(id="") ?(fill="none") ?(stroke="black") ?(width=1.) r (x,y) =
	put out
	(sprintf
	"<circle id=\"%s\" cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"%s\" stroke=\"%s\" stroke-width=\"%f\" />\n" 
	id x y r fill stroke width)

let text out ?(id="") s ?(font="Arial") ?(size=10) ?(weight="normal") ?(anchor="left") (x,y) =
	put out
    (sprintf
	"<text id=\"%s\" x=\"%.f\" y=\"%.f\" style=\"font-family: %s; font-size:%dpx; font-weight: %s; text-anchor: %s\"> %s </text>\n"
	id x y font size weight anchor s)
				
let link out ?(id="") s ?(font="Arial") ?(size=10) ?(weight="normal") ?(anchor="left") ?(target="") ~link (x,y) =
	put out
    (sprintf
	"<a xlink:href=\"%s\" target=\"%s\"> <text id=\"%s\" x=\"%.2f\" y=\"%.2f\" style=\"font-family:%s;font-size:%dpx; font-weight: %s; text-anchor: %s\"> %s </text> </a>\n"
	link target id x y font size weight anchor s)
