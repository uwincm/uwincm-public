# University of Miami Wave Model

# Top-level Makefile

.PHONY: all umwm docs tools clean

all: umwm tools docs

umwm:
	$(MAKE) --directory=src
docs:
	cd docs && pdflatex umwm_manual_v2.tex
	cd docs && pdflatex umwm_manual_v2.tex
	cd docs && pdflatex umwm_manual_v2.tex
	rm -vf docs/*.{aux,log,toc}
tools:
	$(MAKE) --directory=tools/src
clean:
	rm -vf umwm
	rm -vf tools/umwm_gridgen
	rm -vf tools/umwm_topogen
	rm -vf tools/wrf2umwmgrid
	rm -vf tools/wrf2umwmin
	$(MAKE) --directory=src clean
	$(MAKE) --directory=tools/src clean
