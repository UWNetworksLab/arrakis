#!/usr/bin/gawk -f

BEGIN {
    pr = 0
}

/Known Issues/ {
    pr = 0
}

/Supported PC hardware/ {
   pr = 1
}

{
    if (pr ==1) {
	print
    }
}


