#!/bin/bash

rm -f example1.out
rm -f part1.out

mkfifo example1.out
mkfifo part1.out

rm -f fun3
csc -o fun3 fun3.scm

rm -f viz3
csc -o viz3 viz3.scm

