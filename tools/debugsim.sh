#!/bin/bash
#
# Example script to always start debugging the first copy of the
# kernel image when using the simulator. Also loads symbols of a
# process.
#
# Copy this to your build directory and execute as:
# make debugsim GDB_ARGS='-x `./debugsim.sh`'

OUTPUT=debugsim.gdb

get_section_start ()
{
	objdump -h $2 | awk "\$2 == \"$1\"{ print \$4 }"
}

cat > $OUTPUT <<EOF
debug_kernel_at x86_64/sbin/cpu 0xffffff8004362000
add-symbol-file x86_64/sbin/arrakismon 0x`get_section_start .text x86_64/sbin/arrakismon`
info threads
EOF

echo $OUTPUT
