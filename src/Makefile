#
# !Crown Copyright 2014 AWE.
# !
# ! This file is part of Bookleaf.
# !
# ! Bookleaf is free software: you can redistribute it and/or modify it under
# ! the terms of the GNU General Public License as published by the
# ! Free Software Foundation, either version 3 of the License, or (at your option)
# ! any later version.
# !
# ! Bookleaf is distributed in the hope that it will be useful, but
# ! WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# ! FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# ! details.
# !
# ! You should have received a copy of the GNU General Public License along with
# ! Bookleaf. If not, see http://www.gnu.org/licenses/.
#
#
# machine input
MKFILEM = makefile.GENERIC
MKFILEC = makefile.intel
MKFILEV = $(SRCDIR)/makefiles/makefile.VER
# paths
SRCDIR  = .
MODDIR  = $(SRCDIR)/mods

# build info
NO_MPI  =
SILO    =
DEP     =
# Version
VERS = 0.1
EXE  = bookleaf
ifeq ($(MOD),)
  EXENAME  = $(EXE)
else
  EXENAME  = $(EXE)_$(MOD)
endif

# Determine level
ifeq ($(LEV),)
  LEV=RELEASE
endif
FIND_LEV=$(findstring $(LEV),MEMDEBUG DEBUG TV MEMTV OPTDEBUG RELEASE)
ifeq ($(FIND_LEV),)
  $(error Unknown build level)
endif

# Default
help:
	# Usage
	# Pre_pro:
	#  MKFILEM     - machine specific makefile,  DEFAULT makefile.GENERIC
	#  MKFILEC     - compiler specific makefile, DEFAULT makefile.intel
	#  MKFILEV     - override default list of library versions
	#  SRCDIR      - path to source directory,   DEFAULT .
	#  LEV         - build level, options: MEMDEBUG,DEBUG,TV,MEMTV,OPTDEBUG,RELEASE,
	#                DEFAULT RELEASE
	#  MOD         - include problem specific initialisation modification, DEFAULT none
	#  NO_MPI      - Do not compile MPI routines
	#  SILO        - Compile Silo routines
	#
	# Commands:
	#  help        - print this message
	#  bookleaf    - build executable
	#  clean       - remove objects, modules and executable

# Include machine specific options
ifneq (,$(findstring $(EXE),$(MAKECMDGOALS)))
  include $(SRCDIR)/makefiles/$(MKFILEM)
  include $(SRCDIR)/makefiles/$(MKFILEC)
  include $(MKFILEV)
endif

# Include object list
ifneq (,$(findstring $(MAKECMDGOALS),$(EXE)))
  include $(SRCDIR)/makefile.OBJ
endif

# set pre-processor flags, adjust object list and dependency list
ifneq ($(SILO),)
  ifeq ($(MKFILEC), makefile.xl)
    PP_SILO=-WF,-DSILO
  else
    PP_SILO=-DSILO
  endif
  OBJ+=silo.o
  DEP+=silo
  include $(SRCDIR)/cutils/makefile.cutils
endif
ifneq ($(MOD),)
   ifeq ($(MKFILEC), makefile.xl)
  PP_MOD=-WF,-DMOD_P
  else
  PP_MOD=-DMOD_P
endif
  OBJ+=$(MOD).o
endif
ifeq ($(NO_MPI),)
  include $(SRCDIR)/comms/makefile.T3
else
   ifeq ($(MKFILEC), makefile.xl)
  PP_MPI=-WF,-DNOMPI
else
  PP_MPI=-DNOMPI
endif
  include $(SRCDIR)/comms/makefile.T1
endif


# Include dependencies
ifneq (,$(findstring $(EXE),$(MAKECMDGOALS)))
  sinclude ./makefile.dep
endif

# Source paths
VPATH=$(SRCDIR):$(MODDIR):$(SRCDIR)/comms:$(SRCDIR)/cutils

# Collect all pre-processor options
PREPRO = $(PP_MOD)   \
	 $(PP_IO)    \
	 $(PP_MPI)   \
	 $(PP_SILO)  \

CPREPRO = $(F2C_DEF)

# Includes
INC =	$(SILOINC)   \
	$(HDF5INC) 

# Libraries
LIB = 	$(SILOLIB)   \
	$(HDF5LIB)   \
	$(ZLIBLIB)

# Rules
.SUFFIXES:
.SUFFIXES: .o .f90 .F90 .c
banner.o:
	$(FC) $(FFLAGS) $(INC) banner.f90
.f90.o:
	$(FC) $(FFLAGS) $(INC) $<
.F90.o:
	$(FC) $(FFLAGS) $(PREPRO) $(INC) $<
.c.o:
	$(CC) $(CFLAGS) $(CPREPRO) $<

# Commands
$(EXE): $(DEP) $(OBJ)
	$(FC) $(OBJ) $(LIB) -o $(EXENAME)

clean:
	- rm -f *.o
	- rm -f *.mod
	- rm -f $(EXE)
