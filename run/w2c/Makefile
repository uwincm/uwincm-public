FC = ifort
FCFLAGS = -convert big_endian -O0

all:
	$(MAKE) FC=$(FC) FCFLAGS="$(FCFLAGS)" all --directory=src
	ln -s src/cg.exe .
	ln -s src/vortex.exe .
	ln -s src/vorrel.exe .

clean:
	$(MAKE) clean --directory=src
	rm *.exe
