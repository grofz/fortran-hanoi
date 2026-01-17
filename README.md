# Fortran Hanoi Towers

A classic puzzle of Hanoi Towers coded using Fortran.

![Illustration picture](assets/illustration.png)

## Installation

### 1 Install Raylib library

Download, build and install raylib library using instructions from here:
- https://github.com/raysan5/raylib/wiki/Working-on-GNU-Linux

Installing dependecies:
```sh
$ sudo dnf install alsa-lib-devel mesa-libGL-devel libX11-devel libXrandr-devel libXi-devel libXcursor-devel libXinerama-devel libatomic
```

Buliding raylib:
```sh
$ git clone --depth 1 https://github.com/raysan5/raylib.git raylib
$ cd raylib/src/
$ make PLATFORM=PLATFORM_DESKTOP # To make the static version.
$ sudo make install # Static version
```

Testing the installation
```
$ cd raylib/examples
$ make PLATFORM=PLATFORM_DESKTOP
```

### 2 Download and compile Fortran bindings with raylib

- https://github.com/interkosmos/fortran-raylib
```sh
$ git clone --depth 1 https://github.com/interkosmos/fortran-raylib
$ cd fortran-raylib
$ make
```

### 3 Tune the build script to your needs

In `build.sh` or `build.bat` set the path to the directory containing `fortran-raylib`
code.

### 4 Compile and run the game

```sh
$ ./build.sh
$ ./hanoi
```

or in Windows

```sh
$ ./build.bat
$ ./hanoi.exe
```

## Instructions

- Use left mouse button clicks to move rings between columns.
- Press a number key '1', '2', etc. to restart the puzzle with the particular number of rings.
- Press 'Z' to toggle animation style, press '+' and '-' to increase and decrease animation speed.
- Press 'A' and then type the filename of the script with move instructions

### Script file

Here is an example of a script file

```
3
1 3
1 2
3 2
1 3
2 1
2 3
1 3
```

The number at the first line is the number of rings. The rest of the lines are
instructions to move rings. For example, `1 3` is to move the ring from the
leftmost (1) to the rightmost (3) base.
