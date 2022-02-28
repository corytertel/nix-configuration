#!/bin/sh

vol=$(($(pamixer --get-volume) / 10))
meter=''

for i in {1..$vol}; do meter=$meter'#'; done

echo $meter
