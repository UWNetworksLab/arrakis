Service dependency graphs

A list of services is in services.txt

The list with descriptions is in services-desc.txt

The list of dependencies is in dependencies.txt

You can generate the input file for the graph scripts using:
makedeptxt.sh

A pregenerated one is provided in: dep.txt

There are 2 python scripts that generate .dot files from dep.txt.

deptree.py generates a regular dependency graph

deptree2.py generates a bipartite graph which is much easier to read.

use them as follows"

python deptree.py > mydep.dot
dot -Tpdf mydep.dot -O 
xpdf mydep.dot.pdf

python deptree2.py [a|f|b|i] > mydep2.dot
dot -Tpdf mydep2.dot -O -Gratio=0.5
xpdf mydep2.dot.pdf

where a|b|f|i specify the type of dependency to use
a: all
f: fundamental
b: barrelfish
i: implementation-specific


