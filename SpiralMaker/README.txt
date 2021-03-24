***************************************************************************************************
THE SPIRALMAKER SOFTWARE INCLUDES THIRD PARTY SOURCE CODE FROM THE F03GL PROJECT 
(http://www-stone.ch.cam.ac.uk/pub/f03gl/index.xhtml) BY ANTHONY STONE AND ALEKSANDAR DONEV.
THE F03GL SOURCE CODE IS UNDER THE GNU PUBLIC LICENSE VERSION 3 (READ LICENSE.TXT FOR MORE DETAILS)

THE NETWORKMAKER SOFTWARE INCLUDES THIRD PARTY SOURCE CODE FROM THE GTK-FORTRAN PROJECT
(https://github.com/jerryd/gtk-fortran).
THE GTK-FORTRAN SOURCE CODE IS UNDER THE GNU PUBLIC LICENSE VERSION 3 (READ LICENSE.TXT FOR MORE DETAILS)


Linux x86-64 binaries are provided for the SpiralMaker in the bin/ directory.

***************************************************************************************************

HOW TO COMPILE EMBRYOMAKER


Linux:

  Before compilation, make sure your system has the following software packages installed.
  (look at the manual in doc folder for more details).
    freeglut3-dev
    gfortran
    gnuplot (optional, only used by some specific options)

  Run the compile_core.sh script

  ./compile_EmbryoMaker.sh

  It may be that you need to grant execution permissions to that script, in that case type

  chmod +x compile_EmbryoMaker.sh

MAC:

  Before compilation, make sure your system has the following software packages installed.
  (look at the manual in doc folder for more details).
    Xcode
    MacPort
    gcc46


  if you are using OS X 10.6 execute this:

  compile_EmbryoMaker_mac.10.6.sh

  For other versions run this script:

  compile_EmbryoMaker_mac.sh


  The program has only been tested in OS X 10.6 and, might not work in other versions.


 For more details on the installation and usage of the software read the EmbryoMaker User's Manual in the doc/
 directory.



***************************************************************************************************

HOW TO COMPILE NETWORKMAKER GENE NETWORK EDITOR

software requirements

-gtk2.0+ libraries: 
	-http://www.gtk.org
	-In Ubuntu this can be usually installed by running “sudo apt-get install gtk2.0+”

-gtk-2-fortran
	- https://github.com/jerryd/gtk-fortran/wiki 
             or directly: https://aur.archlinux.org/packages/gtk-2-fortran-git/ 
            -We provide the necessary library within the package, which should be sufficient for most
             users' requirements, so manual download may not be needed.

-gfortran compiler:
	-https://gcc.gnu.org/wiki/GFortran
	-In Ubuntu this can be installed by running “apt-get install gfortran”

 Compilation in Linux:

 In the EmbryoMaker_NetworkMaker main directory, execute the following script.
  ./compile_NetworkMaker.sh

 For more details on the installation and usage of the software read the NetworkMaker User's Manual in the doc/
 directory.


*******************************************************************************************************


HOW TO LOAD DIFFERENT PRE-SET INITIAL CONDITIONS WITH EmbryoMaker? USING THE CONFIG_FILE

The file config_file.txt, in the same directory, is read by the EmbryoMaker program and sets some runtime variables
for the program. You can load different pre-set initial conditions by changing the first number that appears in the
file at line 5. Next to it you will find some indications what kind of initial conditions correspond to each value 
you may set.
The config_file also allows you to set the initial values of some variables that can be changed in the display mode,
for example the position of the camera, or the way the cells and nodes are drawn. You may change the values of those
variables, but make sure to backup the file before, because if you accidentally alter the format of the file the 
program won't be able to read it and it will crash.
The file is not strictly necessary in order to run the program, if there is no config_file present, the program sets
default initial conditions and display settings.


******************************************************************************************************
