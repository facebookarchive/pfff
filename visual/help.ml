(*s: help.ml *)
(*s: interface_doc *)
let interface_doc = "
This tool displays a \"code map\" of a software project using
Treemaps. \"Treemaps display hierarchical (tree-structured) data as a
set of nested rectangles. Each branch of the tree is given a 
rectangle, which is then tiled with smaller rectangles representing
sub-branches. A leaf node's rectangle has an area proportional 
to a specified dimension on the data.
\" - http://en.wikipedia.org/wiki/Treemapping:

In our case the dimension is the size of the file.
Moreover each file is colored according to its
\"category\": display code, third party code, etc.
See the legend. We use basic heuristcs based on the
name of the files and directory.

Files and directories are also sorted alphabetically
and partially ordered from top to bottom and left to right. 
So a toplevel 'zzz' subdirectory should be located at the bottom 
right of the screen.

As you move the mouse, the blue highlighted areas are the next
level of directories.

Double-clicking zooms in on the blue-highlighted area.
Right-clicking zoom directly to the file under the cursor.
Middle-clicking open the file under the cursor in your
favourite editor (provided you have M-x server-start
and have emacsclient in your path).

"
(*e: interface_doc *)
(*e: help.ml *)
