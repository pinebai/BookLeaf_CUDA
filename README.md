# BookLeaf


## Introduction

Bookleaf is an unstructured Lagrangian Hydro mini-app.

Four input decks are provided: Sod, Sedov, Saltzmann and Noh.

Current BookLeaf_ref as V1.1, plus:
* parallel ALE capability
* Modified makefile.intel
* Input endtimes modified to match test runs


Wiki: https://github.com/UK-MAC/BookLeaf/wiki


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
The default paritioner is rcb, however Metis 5.1.0 can be used if bookleaf has it linked in. To buld a 
Metis version set METIS=1.

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

4) Building the Noh problem in a seperate build directory and linking in Metis 5.1.0:

`make METIS=1 SRCDIR=../src bookleaf`


## Running the Code

BookLeaf can run with no command line arguments. By default it expects to find a
file called "control" in the directory it is running in. This can be changed 
by running:

`bookleaf_sod FILE=<[path_to_file/]newfile>`

This file is a copy of the files found in the inputs directory, depending on 
which problem you wish to run.

## Version History

BookLeaf_ref - As V1.3

V1.3   - Adds in Metis 5.1.0 support. Plus:
* new noh_lag_medium input added
* bug fixes for memory leaks during comms
* bug fix in comms that would cause some examples to hang on step 1
* bug fix in geometry that would (incorrectly) declare that the cell volume had collapsed
* final timing print now also shows the time spent outside of initialisation, as the initialisation can be very slow.

V1.2   - Adds in parallel ALE. Plus:
* Modified makefile.intel for Xeon vectorisation at OPT level and PHI=1 option to build for Xeon Phi, -qopt-report=3 no longer default flag
* Makefile help has PHI option added plus version updated to v1.1
* End times for sod and sedov test cases changed to reflect test cases run

V1.1   - Adds in mesh partitioning. Parallel running now available.

V1.0   - Initial version. Contains MPI comms, but only serial meshes can be contructed.


