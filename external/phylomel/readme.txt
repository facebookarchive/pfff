This bioinformatics projet will allow users to create and draw
phylogenetic trees. To this intent the project will consist of an
OCaml library and executables for the end user.

The current implementation only supports clustering methods
(upgma and wpgma) and svg output. Still, it is already used on the
www.mlva.eu, a european genotypes database.

I intend to implement soon neighbour joining and pdf
(and/or postscript, perhaps using mlPost or CamlPDF).

For now, I also have a program drawing minimum spanning trees
using a force-based layout. Unfornately it is very slow,
but I intend to tackle this using the Barnes-Hut treecode.
