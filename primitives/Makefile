.SUFFIXES:      .f .F .c .o .a .f90 .F90 .Abs

.F90.o:
	$(FC) -c $(FCFLAGS) $<
.c.o:
	$(CC) -c $(CFLAGS) $<

F_ENDIAN =  -byteswapio
CC = clang
CFLAGS =  -O2 -I. -DNO_RPN_MACROS -Wall -DWITH_TEST
FC = flang
FCFLAGS =  -O2 -I. -I.. -Wall $(F_ENDIAN)

all: librmn_c.a librmn_f.a test_baseio_lean.Abs

librmn_c.a: c_baseio_lean.o
	ar rcv librmn_c.a c_baseio_lean.o

librmn_f.a: f_baseio_lean.o
	ar rcv librmn_f.a f_baseio_lean.o

test_baseio.Abs: librmn_c.a librmn_f.a test_baseio.F90
	$(FC) $(FCFLAGS) test_baseio.F90 -L. -lrmn_f -lrmn_c -o test_baseio.Abs -DNOT_LEAN

test_baseio_lean.Abs: test_baseio.F90 librmn_c.a librmn_f.a
	$(FC) $(FCFLAGS) test_baseio.F90 -L. -lrmn_f -lrmn_c -o test_baseio_lean.Abs

test1_lean: test_baseio_lean.Abs
	rm -f C_file C_file2 /tmp/Scrap*
	./test_baseio_lean.Abs

test1: test_baseio.Abs
	rm -f C_file C_file2 /tmp/Scrap*
	./test_baseio.Abs
	rm -f C_file C_file2 /tmp/Scrap*

testc:
	$(CC) $(CFLAGS) -DSELF_TEST -o testc.Abs c_baseio_lean.c
	./testc.Abs
	ls -l C_file C_file2
	rm -f ./testc.Abs C_file C_file2

clean:
	rm -f librmn_c.a librmn_f.a *.o *.mod a.out *.Abs a.out test_namelist C_file C_file2

