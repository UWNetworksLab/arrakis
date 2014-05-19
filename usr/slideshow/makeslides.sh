#!/bin/bash

##########################################################################
# Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

if [ $# != 1 ] || [ ! -r $1 ]; then
  echo Usage: $0 talk.pdf
  exit 1
fi

set -e
echo "Deleting and creating ./talk/"
rm -rf talk && mkdir -p talk
echo "Converting PDF talk to BMP files using ImageMagick..."
convert -monitor -density 300 -resize 1024x768 -antialias $1 'talk/talk-%03d.bmp'
echo "Done!"
echo "Compressing bitmap files..."
gzip talk/talk*.bmp
echo "Creating cpio archive..."
find talk | cpio -o > talk_ramfs.cpio
echo "Removing ./talk/"
rm -rf talk
echo Done! Now append the line \"module /talk_ramfs.cpio\" to your menu.lst.

# XXX: Old version that uses lots of menu.lst entries
# npages=`ls -1 talk*.bmp.gz | wc -l`
# for n in `seq 0 $(($npages - 1))`; do
#   echo modulenounzip /talk/talk-$n.bmp.gz nospawn
# done > talk_menu.lst

# echo Done! Now append the contents of talk/talk_menu.lst to your menu.lst.
# echo eg: "grep -v '/talk/' menu.lst > menu.lst.new ; cat menu.lst.new talk/talk_menu.lst > menu.lst"
