.SUFFIXES:      .f .F .c .o .a .f90 .F90 .Abs

.F90.o:
	$(FC) -c $(FCFLAGS) $<
.c.o:
	$(CC) -c $(CFLAGS) $<

CC = clang
CFLAGS =  -O2 -I. -DNO_RPN_MACROS -Wall -DWITH_TEST
FC = flang
FCFLAGS =  -O2 -byteswapio

all: librmn_c.a librmn_f.a test_baseio.Abs test_baseio_lean.Abs

librmn_c.a: c_baseio.o
	ar rcv librmn_c.a c_baseio.o

librmn_f.a: f_baseio.o
	ar rcv librmn_f.a f_baseio.o

test_baseio.Abs: librmn_c.a librmn_f.a test_baseio.F90
	$(FC) $(FCFLAGS) test_baseio.F90 -L. -lrmn_f -lrmn_c -o test_baseio.Abs -DNOT_LEAN

test_baseio_lean.Abs: test_baseio.F90 c_baseio_lean.o f_baseio_lean.o
	$(FC) $(FCFLAGS) test_baseio.F90 f_baseio_lean.o c_baseio_lean.o -o test_baseio_lean.Abs

test1_lean: test_baseio_lean.Abs
	rm -f C_file C_file2 /tmp/Scrap*
	./test_baseio_lean.Abs
	rm -f C_file C_file2 /tmp/Scrap*

test1: test_baseio.Abs
	rm -f C_file C_file2 /tmp/Scrap*
	./test_baseio.Abs
	rm -f C_file C_file2 /tmp/Scrap*

clean:
	rm -f librmn_c.a librmn_f.a *.o *.mod a.out test_baseio.Abs test_baseio_lean.Abs

