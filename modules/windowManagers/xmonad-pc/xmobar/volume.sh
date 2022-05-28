#!/bin/sh
amixer sget Master | awk -F"[][]" '/%/ { print $2 }' | head -n 1
