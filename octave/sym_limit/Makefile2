INPUT=$(wildcard *_system.m)
PLOT=$(wildcard *.gplot)
TEX=$(PLOT:.gplot=.tex)
DATA=$(INPUT:_system.m=.data)
EPS=$(TEX:.tex=.eps)

all: generated_pictures $(DATA) $(TEX) update copy 

update:
	@for i in generated_pictures/*.tex ; do sed -i -e "s/pdf/pdf.eps/g" $$i ; done 

generated_pictures:
	mkdir generated_pictures

copy:
	cp generated_pictures/* ../../generated_pictures/

%.data: %_system.m
	octave $< 

%.tex: %.gplot
	gnuplot $< 

clean:
	rm *.data *.tex *.eps