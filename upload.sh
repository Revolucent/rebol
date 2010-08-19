#!/bin/bash


if [ \! -z "$1" ]; then
	FILENAME=$1
	scp $FILENAME share.revolucent.net:r3
else
	for FILENAME in *.r; do
		scp $FILENAME share.revolucent.net:r3
	done
fi

