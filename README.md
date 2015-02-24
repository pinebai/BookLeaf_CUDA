# BookLeaf


## Introduction

Bookleaf is an unstructured Lagrangian Hydro mini-app.

Four input decks are provided: Sod, Sedov, Saltzmann and Noh.

The partitioner branch of Bookleaf allows for in-situ mesh partitioning.


## BookLeaf Build Procedure

Bookleaf can either be built in the src directory or in a user specified directory. 
If a user specified directory is used then a copy of the `src/Makefile` must be placed
in there. Additionally the make command line must include:

`SRCDIR=path/to/src`

Bookleaf has a number of example makefiles for different compilers and architectures
in src/makefiles. By default it will use the makefile.GENERIC and makefile.intel 
files. This behaviour can be changed by setting new values on the command line:

```
MKFILEM=<new makefile> - replaces makefile.GENERIC
MKFILEC=<new makefile> - replaces makefile.intel
```

Four input decks are provided: Sod, Sedov, Saltzmann and Noh. A separate version of
Bookleaf must be built for each deck. Specify which version is being built using 
this argument on the make command line:

`MOD=<sod|sedov|saltzmann>`

The executable will be named: `bookleaf_$MOD`

Note that the noh deck does not require a MOD command and the executable will simply be called `'bookleaf'

## MPI

Bookleaf will automatically partition the mesh according to the number of cores that the problem is run on.

By default Bookleaf builds with MPI, however a truly serial version can be built
by adding:

`NO_MPI=1`



### Examples

1) Building the Sod problem without MPI:

`make NO_MPI=1 MOD=sod bookleaf`

2) Building the Sedov problem using the pgi compiler:

`make MOD=sedov MKFILEC=makefile.pgi bookleaf`

3) Building the Noh problem in a seperate build directory:

`make SRCDIR=../src bookleaf`


## Running the Code

BookLeaf can run with no command line arguments. By default it expects to find a
file called "control" in the directory it is running in. This can be changed 
by running:

`bookleaf_sod file=<newfile>`

This file is a copy of the files found in the inputs directory, depending on 
which problem you wish to run.

