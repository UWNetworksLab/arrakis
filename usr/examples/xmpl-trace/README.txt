The output of this example is important - it provides a trace file.  The 
output will therefore need to be captured to a file.  If running on qemu 
try adding:
	-serial file:myqemulog
(for example, by adding that to the qemu command in symbolic_targets.mk)

or simply do:
make sim | tee output

After saving the trace to a file, you can view it with Aquarium.

Aquarium must be run on Windows.  It can be found in barrelfish/tools/demo/bin.

** Note that in order to build and run this you must have 
** trace = True in $BUILD/hake/Config.hs 

