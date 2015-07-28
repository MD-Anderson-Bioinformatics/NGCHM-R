#!/bin/bash

if [ $# -lt 1 ] ; then
    echo Usage: $0 chmDatadir '[user@]host:path'
    echo
    echo Read the script for detailed information.
    exit 1
fi

# This is the Simple CHM Loader (SCL).  It is a very basic facility for
# automatically updating the contents of a Next-Generation Clustered
# Heat Map (NG-CHM) server.  For more detail on NG-CHMs please see
# http://bioinformatics.mdanderson.org/main/NG-CHM:Overview
#
# The server will copy CHMs on the 'host' server, directory 'path' to
# the server's CHM directory.  
#
# This script should be run as the chmuser (user id 999 in a standard
# system) on a machine with access to the server's CHM directory
# (normally /chmData).
#
# For unattended operation the script requires passwordless login to 
# 'host'.  The id used by the script on the host must be able to read,
# write, and delete all files in the remote directory at 'path'.
#
# The remote directory at 'path' must contain a subdirectory '.scl'
# containing the companion software to this script.  That subdirectory
# also includes Documentation for the user protocol for uploading CHMs.
#
########################################################################

# Parse the supplied parameters and echo to user.
LOCALDIR=$1
echo LOCALDIR is $1

set `echo $2 | sed 's/:/ /'`

echo REMOTE HOST is $1
echo REMOTE DIRECTORY is $2
HOST=$1
REMOTEDIR=$2

# We will establish an SSH master connection so we don't need to
# repeatedly ask the user for credentials.
CM=ControlMaster=Yes
CP=ControlPath=$HOME/.ssh/scl-master-%u@%h:%p

# Define scp command.
# -q disables scp progress meter and ssh messages.
SCP="scp -q"

# Create a local temporary directory
TMP=`mktemp -d -p /tmp/ scl.XXXXXXXX`
if [ "X$HOST" == Xlocalhost ] ; then
    trap "/bin/rm -rf $TMP" 0
else
    trap "ssh -q -o $CP -O stop $HOST 2> /dev/null ; /bin/rm -rf $TMP" 0
fi

# Establish a master connection to host.
if [ "X$HOST" != Xlocalhost ] ; then
    ssh -f -N -o $CP -o $CM $HOST
fi

function rmoldchm() {
    declare LOCALDIR="$1"
    declare OLDDIR="$2"
    if [ "X$OLDDIR" != "X" ] ; then
	/bin/rm -rf "$OLDDIR"
	if [ -d "$OLDDIR" ] ; then
	    echo Unable to fully delete old NGCHM in "$OLDDIR". Moving to attic.
	    if [ ! -d "$LOCALDIR"/.attic ] ; then
		mkdir "$LOCALDIR"/.attic
	    fi
	    mv "$OLDDIR" "$LOCALDIR"/.attic
	fi
    fi
}

if [ "X$HOST" == Xlocalhost ] ; then
    DOREMOTE=""
else
    DOREMOTE="ssh -T -q -o $CP $HOST"
fi

# Repeatedly get and act upon remote user request.
while true ; do
    ACTION=`$DOREMOTE $REMOTEDIR/.scl/scl-next-action.sh $REMOTEDIR`
    echo ACTION is "$ACTION"
    if [ "X$ACTION" == "X" ] ; then
        echo Unable to get next SCL action.
        echo Please check that you have ssh access to "$HOST" and that "$REMOTEDIR" is an SCL directory.
	exit 1
    fi
    set $ACTION
    case "X$1" in
	XADD)
	    CHM="$2"
	    LTMP=`mktemp -d -p "$LOCALDIR" .scl-add-XXXXXX`
	    RMTMP=''
	    echo Copying remote CHM "$CHM" to "$LTMP"
	    if [ "X$HOST" == Xlocalhost ] ; then
		cp "$REMOTEDIR"/ADD/"$CHM" "$LTMP"
	    else
		$SCP -r -o $CP "$HOST":"$REMOTEDIR"/ADD/"$CHM" "$LTMP"
	    fi
	    echo Copied CHM to "$LTMP"
	    if [ -d "$LOCALDIR"/"$CHM" ] ; then
		echo Deleting existing CHM "$CHM"
		RMTMP=`mktemp -d -p "$LOCALDIR" .scl-del-XXXXXX`
		mv "$LOCALDIR"/"$CHM" "$RMTMP"/
	    fi
	    echo Installing new NGCHM "$CHM"
	    mv "$LTMP"/"$CHM" "$LOCALDIR"/"$CHM"
	    # Remove remote CHM to signal the CHM has been installed.
	    $DOREMOTE /bin/rm -rf "$REMOTEDIR"/ADD/"$CHM"
	    # Clean up
	    echo Cleaning up after installing "$CHM"
	    rmdir "$LTMP"
	    rmoldchm "$LOCALDIR" "$RMTMP"
	    ;;
	XADD-FILE)
	    CHM="$2"
	    LTMP=`mktemp -d -p "$LOCALDIR" .scl-add-XXXXXX`
	    echo Copying remote files for CHM "$CHM" to "$LTMP"
	    if [ "X$HOST" == Xlocalhost ] ; then
		cp "$REMOTEDIR"/ADD-FILE/"$CHM"/"*" "$LTMP"/
	    else
		$SCP -r -o $CP "$HOST":"$REMOTEDIR"/ADD-FILE/"$CHM"/"*" "$LTMP"/
	    fi
	    RMTMP=`mktemp -d -p "$LOCALDIR" .scl-del-XXXXXX`
	    for file in "$LTMP"/* ; do
		filename=`basename "$file"`
	        mv "$LOCALDIR"/"$CHM"/"$filename" "$RMTMP"
	        mv "$file" "$LOCALDIR"/"$CHM"/
	    done
	    # Remove remote CHM directory to signal the CHM file(s) have been installed.
            $DOREMOTE /bin/rm -rf "$REMOTEDIR"/ADD-FILE/"$CHM"
	    rmdir "$LTMP"
	    rmoldchm "$LOCALDIR" "$RMTMP"
	    ;;
	XREMOVE)
	    CHM="$2"
	    RMTMP=''
	    if [ -d "$LOCALDIR"/"$CHM" ] ; then
		echo Deleting existing CHM "$CHM"
		RMTMP=`mktemp -d -p "$LOCALDIR" .scl-del-XXXXXX`
		mv "$LOCALDIR"/"$CHM" "$RMTMP"/
	    else
		echo Cannot delete CHM "$CHM" since it does not exist.
	    fi
	    # Remove remote CHM to signal the CHM has been removed (or does not exist).
            $DOREMOTE /bin/rm -rf "$REMOTEDIR"/REMOVE/"$CHM"
	    echo Cleaning up after removing "$CHM"
	    rmoldchm "$LOCALDIR" "$RMTMP"
	    ;;
	XREMOVE-FILE)
	    CHM="$2"
	    LTMP=`mktemp -d -p "$LOCALDIR" .scl-rmf-XXXXXX`
	    echo Copying remote files for CHM "$CHM" to "$LTMP"
	    if [ "X$HOST" == Xlocalhost ] ; then
		cp "$REMOTEDIR"/REMOVE-FILE/"$CHM"/"*" "$LTMP"/
	    else
		$SCP -r -o $CP "$HOST":"$REMOTEDIR"/REMOVE-FILE/"$CHM"/"*" "$LTMP"/
	    fi
	    RMTMP=`mktemp -d -p "$LOCALDIR" .scl-del-XXXXXX`
	    for file in "$LTMP"/* ; do
		filename=`basename "$file"`
	        mv "$LOCALDIR"/"$CHM"/"$filename" "$RMTMP"
	    done
	    # Remove remote CHM directory to signal the CHM file(s) have been removed.
            $DOREMOTE /bin/rm -rf "$REMOTEDIR"/REMOVE-FILE/"$CHM"
	    rmoldchm "$LOCALDIR" "$LTMP"
	    rmoldchm "$LOCALDIR" "$RMTMP"
	    ;;
	XLIST)
	    echo Updating LIST
            TMPFILE="$REMOTEDIR/$2"
	    (cd "$LOCALDIR" ; find . -maxdepth 1 -mindepth 1 -type d) | sed 's:^\./::' > $TMP/dlist
	    #echo $SCP -o $CP "$TMP/dlist" "$HOST":"$TMPFILE"
	    if [ "X$HOST" == Xlocalhost ] ; then
		cp "$TMP/dlist" "$TMPFILE"
	    else
		$SCP -o $CP "$TMP/dlist" "$HOST":"$TMPFILE"
	    fi
	    #echo TMPFILE copied
            $DOREMOTE $REMOTEDIR/.scl/scl-update-chm-list.sh "$REMOTEDIR" "$TMPFILE"
	    ;;
	*) echo Unrecognized SCL response.  Please verify the SCL directory is compatible.
	   exit 1
    esac
done

exit 0
