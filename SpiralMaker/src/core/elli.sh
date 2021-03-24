#!/bin/bash

#mv opengl.mod kky
rm -f *.mod
#mv kky opengl.mod
#gcc -c saveit.c
#gcc -o gd.e gd.c -lgd

#gfortran -S -w openGLu.f03
gfortran -S -w OpenGL_gl.f90 OpenGL_glu.f90 OpenGL_glut.f90

#THE LIBRARY PATHS NEED NO LONGER TO BE SPECIFIED
#
gfortran -w -fexceptions -fno-underscoring -fbounds-check aleas.mod.f90 general.mod.f90 neighboring.mod.f90 genetic.mod.f90 energy.mod.f90 io.mod.f90 pola.mod.f90 mitosis.mod.f90 ic.mod.f90 nexus.mod.f03 biomechanic.mod.f90 model.mod.f90 inicial.mod.f90 editor.mod.f90 drawer.mod.f90 automaticon.mod.f90 geompack3.f90 elli.f90 -o SMaker -lGL -lGLU -lglut
#ls -lart *.mod
