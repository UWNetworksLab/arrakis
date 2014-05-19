#!/bin/sh

ARRAKIS_SRC=$HOME/projects/arrakis/src
ARRAKIS_BUILD=$HOME/projects/arrakis/qemu/x86_64

ln -sf $ARRAKIS_SRC/lib/tenaciousd/Makefile
ln -sf $ARRAKIS_SRC/include/tenaciousd
ln -sf $ARRAKIS_SRC/include/storage
ln -sf $ARRAKIS_BUILD/include/errors

make VPATH=$ARRAKIS_SRC/lib/tenaciousd
