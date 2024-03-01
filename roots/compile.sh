SRC=./src
OBJ=./obj
BIN=./bin

#Creating objeects
# the.mod objects are going to be stored in the main irectory

#gfortran -c $SRC/subs1.f95 -o $OBJ/subs1.o
# -J $OBJ Saves objects
gfortran -J $OBJ -c $SRC/roots.f95 -o $OBJ/roots.o
gfortran -J $OBJ -c $SRC/ode_solvers.f95 -o $OBJ/ode_solvers.o


#-I $OBJ  look for .mod objects
gfortran -I $OBJ -c $SRC/main.f95 -o $OBJ/main.o


#makes Exe withh all the .o in OBJ directory  *.o all the 
gfortran $OBJ/*.o -o $BIN/main.exe
#Execute program 

$BIN/main.exe #<<< input_param.dat