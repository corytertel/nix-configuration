{ pkgs }:

pkgs.writeShellScriptBin "dyalogscript" ''
#set -x
## DO NOT CHANGE THIS FILE

INSTALLDIR="${pkgs.dyalog}"
EXE="dyalog"

ARGS=""
SCRIPT=""

while [ $# -ne 0 ]
do
	case $1 in
	*=*|-*)
		if [ -z "$SCRIPT" ]; then
			OPTS="$OPTS $1"
		else
			ARGS="$ARGS ${"$"}{1@Q}" # maintain the quotes
		fi
		shift
		;;
	*)
		if [ -z "$SCRIPT" ]; then
			SCRIPT=$1
		else
			ARGS="$ARGS ${"$"}{1@Q}"	# maintain the quotes
		fi
		shift
		;;
	esac
done



#RIDE cannot currently debug apl #! scripts
#if [ ! -z $RIDE_INIT ]; then
#	OPTS="$OPTS RIDE_INIT=$RIDE_INIT"
#fi

: ${"$"}{SCRIPTDIR:=$INSTALLDIR}

eval "${"$"}{SCRIPTDIR}/${"$"}{EXE}" APLKEYS=\"$INSTALLDIR/aplkeys\" APLTRANS=\"$INSTALLDIR/apltrans\" $OPTS -script \"$SCRIPT\" $ARGS
''
