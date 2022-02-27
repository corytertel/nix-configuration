#!/bin/sh

vol=$(($(pamixer --get-volume) / 10))
meter=''

vol=$((10 - $vol))

for i in {1..$vol}; do meter=$meter':'; done

echo $meter
