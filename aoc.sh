#!/bin/sh

if [ -z "$1" ] ;then
	echo "usage: aoc.sh day" >&2
	exit 1
fi

day=$(printf "%d" "$1")

if [ 1 -gt "$day" ] || [ "$day" -gt 25 ] ;then
    echo "must be in 1..25" >&2
    exit 1
fi

if [ -z "$AOC_COOKIE" ] ;then
	echo "no \$AOC_COOKIE" >&2
	exit 1
fi

out=$(printf "bin/d%02d" "$day")

if [ ! -d "$out" ] ;then
	cp -r template "$out"
fi

curl -o "$out/input.txt" -b "$AOC_COOKIE" "https://adventofcode.com/2024/day/$day/input"
