#!/bin/bash

if [ $# != 2 ]; then
    echo Usage: $0 menu.lst output_prefix
    exit 1
fi

MENU_LST=$1
OUTPUT_PREFIX=$2

# Prefix prepended to each output file within the directory
# $OUTPUT_PREFIX (for safety, this means we can clean the directory
# by removing everything with this prefix)
FILE_PREFIX=tmp_molly

# Set up output direcotry
if [ -e $OUTPUT_PREFIX  ] && [ ! -d $OUTPUT_PREFIX ]; then
    echo Error: $OUTPUT_PREFIX exists, but is not a direcotry
    exit 1
fi

if [ -d $OUTPUT_PREFIX ]; then
    echo Cleaning old directory $OUTPUT_PREFIX
    rm -f $OUTPUT_PREFIX/$FILE_PREFIX*
fi

if [ ! -d $OUTPUT_PREFIX/ ]; then
    echo Making output directory $OUTPUT_PREFIX
    mkdir $OUTPUT_PREFIX
fi

# Get list of binaries to translate
BINS=$(awk '/^kernel/ || /^module/ {print $2}' $MENU_LST)
# For each binary generate an object file in the output directory.
# The flags to objcopy cause it to place the binary image of the input
# file into an .rodataIDX section in the generated object file where
# IDX is a counter incremented for each binary.  
IDX=1
for BIN in $BINS; do
  #was SLASH=${BIN////_}, which only replaced slashes, but we need to replace "-" for armv7-m
  UNDERSCORED=${BIN//-/_}
  SLASH=${UNDERSCORED////_}
  BIN_OUT="$OUTPUT_PREFIX/${FILE_PREFIX}_$SLASH"
  OBJCOPY=$(which arm-none-linux-gnueabi-objcopy || which arm-linux-gnueabi-objcopy)
  echo $BIN '->' $BIN_OUT
  $OBJCOPY -I binary -O elf32-littlearm -B arm --rename-section .data=.rodata$IDX,alloc,load,readonly,data,contents .$BIN $BIN_OUT
  IDX=$(($IDX+1))
  if [ $IDX = 20 ]; then
      echo Error: linker script cannot handle $IDX modules
      exit 1
  fi
done


