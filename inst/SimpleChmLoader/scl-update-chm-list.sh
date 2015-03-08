#!/bin/bash

if [ $# -ne 2 ] ; then
    echo Usage: $0 scldir chmlistfile
    exit 1
fi

SCLDIR="$1"
NEWLIST="$2"
OLDLIST=`mktemp -p "$SCLDIR"/.scl scl-llist-XXXXXX`
trap "/bin/rm $OLDLIST $NEWLIST" 0

if [ ! -d "$SCLDIR/LIST" ] ; then
    echo "$SCLDIR/LIST" does not exist
    exit 1
fi

(cd "$SCLDIR/LIST" ; find . -maxdepth 1 -mindepth 1 -type d) | sed 's:^\./::' > "$OLDLIST"

function setdiff () {
    # Execute command $1 on all names in file $3 but not $2
    #echo setdiff "$1" "$2" "$3"
    declare -A what
    declare x=
    #echo 'At init $# ==' ${#x[@]}
    while read -r ; do
        what[$REPLY]=here
    done < "$2"

    while read -r ; do
        if [ "X${what[$REPLY]}" != Xhere ] ; then
            x[${#x[@]}]="$SCLDIR/LIST/$REPLY"
        fi
    done < "$3"

    #echo '$# ==' ${#x[@]}
    if [ ${#x[@]} -gt 1 ] ; then
        #echo $1 ${x[@]}
        $1 ${x[@]}
    fi
}

setdiff mkdir "$OLDLIST" "$NEWLIST"
setdiff rmdir "$NEWLIST" "$OLDLIST"

exit 0
