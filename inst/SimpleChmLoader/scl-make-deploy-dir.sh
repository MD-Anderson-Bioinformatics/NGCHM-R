#!/bin/bash

if [ $# -lt 1 ] ; then
    echo Usage: $0 '[user@]host:path'
    echo
    echo Read the script for detailed information.
    exit 1
fi

# This script initializes a Simple CHM Loader (SCL) control directory.
# SCL is a very basic facility for automatically updating the contents
# of a Next-Generation Clustered Heat Map (NG-CHM) server.  For more
# detail on NG-CHMs please see
# http://bioinformatics.mdanderson.org/main/NG-CHM:Overview
#
# The server will create and initialize a remote directory on the 'host'
# server, directory 'path' to which users may upload NG_CHMs for
# installation on the NG-CHM serveru using the 'SCL' protocol.  Users
# can also use the protocol to remove NG-CHMs from the server, and add
# and remove specific files within NG-CHMs.
#
########################################################################

# Parse the supplied parameters and echo to user.
LOCALDIR=$(dirname $0)
echo LOCALDIR is $LOCALDIR

set `echo $1 | sed 's/:/ /'`

echo REMOTE HOST is $1
echo REMOTE DIRECTORY is $2
HOST=$1
REMOTEDIR=$2

SCRIPTS="scl-next-action.sh  scl-update-chm-list.sh"
OTHERFILES="README.SCL.txt"
for script in $SCRIPTS ; do
    if [ ! -r ${LOCALDIR}/${script} ] ; then
        echo Unable to find required script ${script} in directory ${LOCALDIR}.
        exit 1
    fi
done
for file in ${OTHERFILES} ; do
    if [ ! -r ${LOCALDIR}/${file} ] ; then
        echo Unable to find required file ${file} in directory ${LOCALDIR}.
        exit 1
    fi
done

# We will establish an SSH master connection so we don't need to
# repeatedly ask the user for credentials.
CM=ControlMaster=Yes
CP=ControlPath=$HOME/.ssh/scl-master-%u@%h:%p

# Define standard ssh command.
SSH="ssh -T -q -o $CP"

# Define standard scp command.
# -q disables scp progress meter and ssh messages.
SCP="scp -q -o $CP"

# Create a local temporary directory
trap "ssh -q -o $CP -O stop $HOST 2> /dev/null" 0

# Establish a master connection to host.
ssh -f -N -o $CP -o $CM $HOST

if ! $SSH "$HOST" mkdir "$REMOTEDIR" "$REMOTEDIR"/.scl ; then
    echo Unable to create SCL directory "${REMOTEDIR}"
    exit 1
fi
if ! $SSH "$HOST" chmod 755 "$REMOTEDIR" "$REMOTEDIR"/.scl ; then
    echo Unable to set permissions on SCL directory "${REMOTEDIR}"
    exit 1
fi
for script in $SCRIPTS ; do
    if ! $SCP -o $CP "${LOCALDIR}/${script}" "$HOST":"$REMOTEDIR"/.scl ; then
	echo "Unable to copy script ${script} to SCL directory ${REMOTEDIR}"
	exit 1
    fi
    if ! $SSH "$HOST" chmod 755 "$REMOTEDIR"/.scl/"${script}" ; then
	echo "Unable to set permissions on script ${script} in SCL directory ${REMOTEDIR}"
    fi
done
for file in $OTHERFILES ; do
    if ! $SCP -o $CP "${LOCALDIR}/${file}" "$HOST":"$REMOTEDIR"/.scl ; then
	echo "Unable to copy file ${file} to SCL directory ${REMOTEDIR}"
	exit 1
    fi
    if ! $SSH "$HOST" chmod 644 "$REMOTEDIR"/.scl/"${file}" ; then
	echo "Unable to set permissions on file ${file} in SCL directory ${REMOTEDIR}"
    fi
done
if ! $SSH "$HOST" mkdir "$REMOTEDIR"/LIST "$REMOTEDIR"/STAGE "$REMOTEDIR"/ADD "$REMOTEDIR"/REMOVE "$REMOTEDIR"/ADD-FILE "$REMOTEDIR"/REMOVE-FILE ; then
    echo Unable to create a required subdirectory of SCL directory "${REMOTEDIR}"
    exit 1
fi
if ! $SSH "$HOST" chmod 755 "$REMOTEDIR"/LIST ; then
    echo Unable to set permissions on a required subdirectory of SCL directory "${REMOTEDIR}"
    exit 1
fi
if ! $SSH "$HOST" chmod 3777 "$REMOTEDIR"/STAGE "$REMOTEDIR"/ADD "$REMOTEDIR"/REMOVE "$REMOTEDIR"/ADD-FILE "$REMOTEDIR"/REMOVE-FILE ; then
    echo Unable to set permissions on a required subdirectory of SCL directory "${REMOTEDIR}"
    exit 1
fi

echo Remote SCL directory "${REMOTEDIR}" initialized successfully.
exit 0
