#!/bin/bash

##########################################################################
# Copyright (c) 2009, 2011, 2013, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

DFLTARCHS="\"x86_64\""
RUN_HAKE="Yes"

usage() { 
    echo "Usage: $0 <options>"
    echo "   -s|--source-dir: path to source tree (required)"
    echo "   -i|--install-dir: path to install directory (defaults to \`pwd\`)"
    echo "   -a|--architecture: specify archtitecture to build for (can be"
    echo "       given multiple times, default architectures are"
    echo "       $DFLTARCHS"
    echo "   -n|--no-hake: just rebuild hake itself, don't run it (only useful"
    echo "       for debugging hake)"
    echo ""
    echo "  The way you use this script is to create a new directory for your"
    echo "  build tree, cd into it, and run this script with the --source-dir"
    echo "  argument specifying the top of the source tree."
    exit 1;
}

#
# Legacy compatibility to avoid breaking the harness...
#
if [ $# -eq 1 ]; then
    echo "WARNING: old usage of hake.sh (sole argument gives the source directory) is"
    echo "deprecated: please use --source-dir instead."
    SRCDIR="$1"
    shift
fi

#
# Parse args
#
while [ $# -ne 0 ]; do
    case $1 in
	"-a"|"--architecture") 
	    if [ -z "$NEWARCHS" ] ; then
		NEWARCHS="\"$2\""
	    else
		NEWARCHS="$NEWARCHS, \"$2\""
	    fi
	    ;;
	"-i"|"--install-dir")
	    INSTALLDIR="$2"
	    ;;
	"-s"|"--source-dir")
	    SRCDIR="$2"
	    ;;
	"-n"|"--no-hake")
	    RUN_HAKE="No"
	    ;;
	*) 
	    usage
	    ;;
    esac
    shift 
    shift
done

if [ -z "$INSTALLDIR" ] ; then
    echo "Install directory defaulting to '.'"
    INSTALLDIR="."
else
    echo "Install directory is $INSTALLDIR"
fi
cd $INSTALLDIR

if [ -z "$SRCDIR" ] ; then
    usage
fi

if [ ! -f "$SRCDIR"/hake/Main.hs ] ; then
    echo "Can't find Hake in the source directory $SRCDIR."
    echo "Did you specify the source directory correctly?"
    usage
fi
echo "Source directory is $SRCDIR"

if [ ! -z "$NEWARCHS" ]; then
    ARCHS="$NEWARCHS"
else 
    ARCHS="$DFLTARCHS"
fi
echo "Architectures to build: $ARCHS"

if [ ! -d hake ] ; then
    echo "Creating a local hake directory..."
    mkdir -p hake
    touch hake/.marker
fi

echo "Setting up hake build directory..."
if [ ! -f hake/Config.hs ]; then
    cp $SRCDIR/hake/Config.hs.template hake/Config.hs
    cat >> hake/Config.hs <<EOF

-- Automatically added by hake.sh. Do NOT copy these definitions to the defaults
source_dir = "$SRCDIR"
architectures = [ $ARCHS ]
install_dir = "$INSTALLDIR"
EOF
else
    echo "You already have Config.hs, leaving it as-is."
fi

if [ ! -f ./symbolic_targets.mk ]; then
    echo "Creating new symbolic_targets.mk file."
    cp "$SRCDIR/hake/symbolic_targets.mk" . 
else
    echo "You already have symbolic_targets.mk, leaving it as-is."
fi

# FIXME: do we really need this; doesn't ghc get the dependencies right? -AB
#rm -f hake/*.hi hake/*.o 

echo "Building hake..."
ghc -O --make -XDeriveDataTypeable \
    -package ghc \
    -package ghc-paths \
    -o hake/hake \
    -outputdir hake \
    -i$SRCDIR/hake \
    -ihake \
    -rtsopts=all \
    -threaded \
    -with-rtsopts="-K32m" \
    $SRCDIR/hake/Main.hs $LDFLAGS || exit 1

    # -eventlog \

if [ "$RUN_HAKE" == "No" ] ; then
    echo "Not running hake as per your request."
    exit
fi

echo "Running hake..."
#./hake/hake --output-filename Makefile --source-dir "$SRCDIR" +RTS -s -N -K64M -A64M -ls -lf || exit
./hake/hake --output-filename Makefile --source-dir "$SRCDIR" +RTS -N -K64M -A64M || exit
cat <<EOF

OK - Hake has bootstrapped.  You should now have a Makefile in this
directory, and you can type "make" to build a predefined target.

To change configuration options, edit the Config.hs file in the hake
subdirectory of this directory and run "make rehake".

To change the set of symbolic make targets available (for example, to
build a different set of modules or architectures for your boot image),
edit the local copy of the symbolic_targets.mk in this directory.
EOF
