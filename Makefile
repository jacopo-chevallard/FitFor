FC         = mpifort -cpp -DMPI
FFLAGS     = -Wall -g  -O3 -Wextra -fbacktrace -fbounds-check -ffree-line-length-none
LIBCFITSIO = cfitsio$(DEBUG)

LIBDIR     = $(HOME)/lib
INCLUDEDIR = $(HOME)/include

INCLUDEDIR_FITFOR = $(HOME)/My_Codes/FitFor
LIBDIR_FITFOR = $(HOME)/My_Codes/FitFor

INCLUDE_LIB			= -L$(LIBDIR) -l$(LIBCFITSIO) -L$(LIBDIR_FITFOR) -lfitfor

F90 	   		= $(FC) $(FFLAGS) -I$(INCLUDEDIR) -I$(INCLUDEDIR_FITFOR)

OBJLIB = fits_parameters.o \
				 fits_image.o \
				 fits_column.o \
				 fits_table.o \
				 manipulate_fits.o

test_fits:test_fits.o
	$(FC) $(FFLAGS)  -o test_fits test_fits.o  $(INCLUDE_LIB) 

test_fits.o:./test_fits.f90 
	 $(F90) -c ./test_fits.f90 $(INCLUDE_LIB) 

fits_parameters.o:./fits_parameters.f90 
	 $(F90) -c ./fits_parameters.f90 

fits_column.o:./fits_column.f90  fits_parameters.o
	 $(F90) -c ./fits_column.f90 

fits_table.o:./fits_table.f90 fits_column.o fits_parameters.o
	 $(F90) -c ./fits_table.f90 

fits_image.o:./fits_image.f90 fits_parameters.o
	 $(F90) -c ./fits_image.f90 

manipulate_fits.o:./manipulate_fits.f90  fits_parameters.o fits_column.o fits_table.o fits_image.o
	 $(F90) -c ./manipulate_fits.f90 

libfitfor: $(OBJLIB)
	ar ruv libfitfor.a $?
	ranlib libfitfor.a

# clean all object and module files 
clean:
	rm -f -r f_{files,modd}* *.o *.a *.mod *.M *.d V*.inc *.vo \
