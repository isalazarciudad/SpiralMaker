#!/bin/bash


cd src/core


echo 'descending'
pwd

aleas=aleas.mod.f90

#making object files
echo 'making object files'

gfortran -S -w OpenGL_gl.f90 OpenGL_glu.f90 OpenGL_glut.f90
gfortran -w -c $aleas
gfortran -w -c general.mod.f90 
gfortran -w -c geompack3.f90 
gfortran -w -c neighboring.mod.f90
gfortran -w -c genetic.mod.f90
gfortran -w -c energy.mod.f90 io.mod.f90 
gfortran -w -c biomechanic.mod.f90 pola.mod.f90
gfortran -w -c ic.mod.f90 mitosis.mod.f90
gfortran -w -c nexus.mod.f03
gfortran -w -c inicial.mod.f90
gfortran -w -c editor.mod.f90 model.mod.f90
gfortran -w -c drawer.mod.f90 automaticon.mod.f90
gfortran -w -c elli.f90


#linking
echo 'linking'

gfortran -w -fexceptions -fno-underscoring -fbounds-check aleas.mod.f90 general.mod.f90 neighboring.mod.f90 genetic.mod.f90 energy.mod.f90 io.mod.f90 pola.mod.f90 mitosis.mod.f90 ic.mod.f90 nexus.mod.f03 biomechanic.mod.f90 model.mod.f90 inicial.mod.f90 editor.mod.f90 drawer.mod.f90 automaticon.mod.f90 geompack3.f90 elli.f90 -o SMaker -lGL -lGLU -lglut

#cleaning
echo 'cleaning'
rm *.s *.o *.mod

cd .. && cd ..
mv src/core/SMaker bin
cp src/core/4cellstage bin
cp src/core/config_file.txt bin

echo 'executables installed in bin/'


