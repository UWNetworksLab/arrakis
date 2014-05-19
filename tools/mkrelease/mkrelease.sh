#!/bin/bash

EXEC_FILES_LIST=tools/mkrelease/executable_files
export SPLASH2_PATH=/home/netos/projects/barrelfish/splash2

if [ ! \( -f README -a -d hake -a -d .hg -a -r $EXEC_FILES_LIST \) ]; then
  echo "This script must be run in the root of a Barrelfish tree" > /dev/stderr
  exit 1
fi

HGDATE=$(hg log -l 1 --template "{date|shortdate}\n") || exit
DATE=$(echo $HGDATE | tr -d -) || exit
TARFILE=barrelfish-$DATE.tar.bz2

if [ -e $TARFILE ]; then
  echo "Error: $TARFILE already exists" > /dev/stderr
  exit 1
fi

TMPDIR=$(mktemp -d) || exit
DESTDIR=$TMPDIR/barrelfish-$DATE

function die()
{
  echo "Aborted." > /dev/stderr
  rm -rf $TMPDIR
  exit 3
}

echo "Creating hg archive to $DESTDIR..."
hg archive -t files $DESTDIR || die

echo "Removing HG files..."
rm $DESTDIR/.hgtags $DESTDIR/.hgignore $DESTDIR/.hg_archival.txt || die

echo "Looking for executable files that shouldn't be..."
BADPERMS=$(find $DESTDIR -type f -perm /ugo+x \
           | cut -c $((${#DESTDIR} + 2))- \
           | grep -vFf $EXEC_FILES_LIST)
[ $? -gt 1 ] && die
if [ -n "$BADPERMS" ]; then
  echo "Error: the following files are executable, but not listed in $EXEC_FILES_LIST:" > /dev/stderr
  echo "$BADPERMS" > /dev/stderr
  echo "Please fix this before continuing." > /dev/stderr
  rm -rf $TMPDIR
  exit 2
fi

echo "Generating SPLASH2 patch..."
(
  cd $DESTDIR/usr/splash2
  ./runme.sh makepatch
) || die

echo "Removing ETH-private files..."
rm -r \
    $DESTDIR/doc/005-scc/plots/rcce_bench/rcce_runs-v2-cut.xls \
    $DESTDIR/usr/skb/data \
    || die

echo "Creating tarball..."
tar -cj -C $TMPDIR -f $TARFILE barrelfish-$DATE || die

echo "Cleaning up..."
rm -rf $TMPDIR

echo "All done! Release archive is in $TARFILE"
