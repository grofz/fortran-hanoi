#!/bin/sh

set -xe

# Linux Wauland
libs="-lraylib -lGL -lm -lpthread -ldl -lrt -lwayland-client -lwayland-cursor -lwayland-egl -lxkbcommon"

# Linux X11
#libs="-lraylib -lGL -lm -lpthread -ldl -lrt -lX11"

bindsrc=../fortran-raylib

bindobs="${bindsrc}/raylib_camera.o \
         ${bindsrc}/raylib.o \
         ${bindsrc}/raylib_math.o \
         ${bindsrc}/raylib_util.o"

flags="-fno-range-check --max-errors=1 -Wall -Wextra -pedantic -std=f2023"

gcc -c wrapper.c
gfortran $flags -I$bindsrc hanoigui.f90 main.f90 wrapper.o $bindobs $libs -o hanoi
