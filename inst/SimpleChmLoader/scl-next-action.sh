#!/bin/bash

if [ $# -lt 1 ] ; then
    echo Usage: "$0" scl-directory [ timeout ]
    exit 1
fi
if [ $# -lt 2 ] ; then
    TIMEOUT=60
else
    TIMEOUT="$2"
fi

if ! cd "$1" ; then
    echo ERROR cannot cd to remote directory "$1"
    exit 1
fi

if [ "X$(which inotifywait)" == X ] ; then
    echo ERROR inotifywait is not installed.
    exit 1
fi

function doit() {
    echo "$1" | sed 's:/: :'
    exit 0
}

function check() {
    for CMD in ADD ADD-FILE REMOVE-FILE REMOVE ; do
	R=`echo $CMD/*`
	if [ "X$R" != X$CMD'/*' ] ; then doit "$R" ; fi
    done
}

# SCLDIR is relative to $1
SCLDIR=.
check
EV=`inotifywait -q -t $TIMEOUT -e moved_to,create "$SCLDIR/ADD" "$SCLDIR/REMOVE" "$SCLDIR/ADD-FILE" "$SCLDIR/REMOVE-FILE"`
if [ "X$EV" == X ] ; then
    check
    echo LIST `mktemp -p $SCLDIR/.scl scl-list-XXXXXX`
else
set "$EV"
    W=`echo "$1" | sed -e 's:^\./::' -e 's:/$::'`
    E="$2"
    F="$3"
    echo "$W" "$F"
fi
exit 0
