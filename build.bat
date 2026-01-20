 gcc -c wrapper.c
 gfortran -Ofast -J../fortran-snake/fortran-raylib wrapper.o hanoigui.f90 main.f90 -lraylib -lwinmm -lgdi32 -fno-range-check --max-errors=3 -o hanoi.exe
