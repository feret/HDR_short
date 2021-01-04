INPUT=kappa_AI_main.tex
NONSTOP?= -interaction=nonstopmode
BIN=bin
BINPATH=$(BIN)/
BUILD=_build
BUILDPATH=$(BUILD)/
FIGPATH=figures/
IMAGE=generated_pictures
IMAGEPATH=$(IMAGE)/
GNUPLOTPATH=gnuplot/
OCTAVEPATH=octave/
KAPPAREP=kappa/
DATAREP=data/
GKAPPAREP=$(HOME)/GKappa/_build/sources

shortINPUT=$(basename $(INPUT))
OUTPUT=$(INPUT:%.tex=%.pdf)

OCTAVE=$(wildcard $(OCTAVEPATH)*/sym_system.m)
KAPPA=$(wildcard $(KAPPAREP)*.ka)
DATA1=$(patsubst $(KAPPAREP)%,$(DATAREP)%,$(KAPPA))
DATA=$(OCTAVE:%_system.m=%.data) $(DATA1:%.ka=%.data)
GNUPLOT=$(wildcard $(GNUPLOTPATH)*.gplot)


BBL=$(INPUT:%.tex=%.bbl)
INDEX=$(INPUT:%.tex=%.toc) $(INPUT:%.tex=%.ilg)
DOT=$(wildcard $(FIGPATH)*.dot)
DOT2 = $(wildcard $(BUILD)/*.dot)
DOT3 = $(patsubst $(BUILD)%,$(IMAGEPATH)%,$(DOT2))

BIB = $(wildcard *.bib)
INPUTTEX = $(wildcard *.tex)

LADOT1 = $(wildcard $(FIGPATH)*.dot)
LADOT2 = $(wildcard $(BUILD)/*.ladot)
LADOT3 = $(patsubst $(BUILD)%,$(IMAGEPATH)%,$(LADOT2))

OCAMLFIG=$(wildcard $(FIGPATH)*.figs.ml)
OCAMLFIG2=$(patsubst $(FIGPATH)%,$(BINPATH)%,$(OCAMLFIG))
OCAMLBIN=$(OCAMLFIG2:%.figs.ml=%)
OCAMLSIG=$(wildcard $(FIGPATH)*.sig.ml)
OCAMLSIG2=$(OCAMLSIG:%.sig.ml=%.ml)
SIGML=$(patsubst $(FIGPATH)%,$(BUILDPATH)%,$(OCAMLSIG2))
SIGCMX=$(SIGML:%.ml=%.cmx)

OCAMLWITNESS=$(OCAMLBIN:%=%.witness)

MACRO=$(wildcard *.sty)

HTML=

PDFFIG=$(wildcard $(FIGPATH)*.pdf)

FIG=$(wildcard $(FIGPATH)*.fig)
CHAPTERS=$(wildcard *.tex)
PREFIG=$(patsubst $(FIGPATH)%,$(IMAGEPATH)%,$(FIG))
PREGPLOT=$(patsubst $(GNUPLOTPATH)%,$(IMAGEPATH)%,$(GNUPLOT))
TEX=$(PREFIG:%.fig=%.pdf_t) $(PREGPLOT:%.gplot=%.tex)

PDF=$(PREGPLOT:%.gplot=%.pdf) $(PREFIG:%.fig=%.pdf) $(patsubst $(FIGPATH)%,$(IMAGEPATH)%,$(PDFFIG))  $(patsubst $(FIGPATH)%,$(IMAGEPATH)%,$(DOT:%.dot=%.pdf)) $(patsubst $(FIGPATH)%,$(IMAGEPATH)%,$(PDF3)) $(LADOT3:%.ladot=%.pdf)
JPG=
PNG=

PREIMAGES=$(JPG) $(PNG)
IMAGES= $(IMAGEPATH) $(PDF) $(TEX) $(PREIMAGES:%=$(IMAGEPATH)%)

OCAMLOPT=ocamlopt.opt -I $(BUILD) -I $(GKAPPAREP) data_structures.cmx geometry.cmx gkappa.cmx config.cmx

BENCHMARKREP=benchmarks/
MODELREPS=$(wildcard $(BENCHMARKREP)*/)

WITNESS=$(MODELREPS:%=%.witness) $(GNUPLOT:%.gplot=%.witness) figure.witness

all: $(DATA) $(WITNESS) $(PDF) $(OUTPUT)
	echo $(MODELREPS)

figure.witness: $(OCAMLWITNESS)
	touch $@
	$(MAKE)

witness:
	echo $(WITNESS)

$(BENCHMARKREP)%.witness:
	cd $(basename $@) && ./script
	touch $@

$(GNUPLOTPATH)%.witness: $(GNUPLOTPATH)%.gplot $(DATA)
	mkdir -p $(IMAGE)
	gnuplot $(basename $@).gplot && touch $@

$(IMAGEPATH)%.tex: $(GNUPLOTPATH)%.witness
$(IMAGEPATH)%.pdf: $(GNUPLOTPATH)%.witness

short:
	pdflatex $(NONSTOP) $(shortINPUT)

$(OUTPUT): $(INPUTTEX) $(TEX) $(CHAPTERS) $(BBL) $(INDEX) $(PDF) $(INPUT)
	pdflatex $(NONSTOP) $(shortINPUT)
	pdflatex $(NONSTOP) $(shortINPUT)

%.witness: %.tex
	pdflatex $<
	pdflatex $<
	touch $@

%.bbl: %.tex %.witness $(TEX) $(CHAPTERS) $(BIB)
	bibtex $(basename $<)

%.toc: %.tex %.witness $(CHAPTERS) $(TEX)
	makeindex $(basename $<)

%.ilg: %.tex %.witness $(CHAPTERS) $(TEX)
	makeindex $(basename $<)

$(BIN)/%.witness: $(BUILD)/%.ml  $(COMMONCMX) $(SIGCMX)
	mkdir -p $(BIN)
	mkdir -p $(BUILD)
	$(OCAMLOPT) $(COMMONCMX) $(SIGCMX) $< -o $(basename $@)
	cd $(BUILD) && $(CURDIR)/$(basename $@)
	touch $@

build_tmp: $(TMPTEX) $(TMPPDF)

$(IMAGEPATH)%.pdf: $(BUILDPATH)%.ladot.tex $(IMAGEPATH) $(BUILDPATH)%.ladot.eps
	rm -rf tmp
	mkdir tmp
	cat fm_header $(BUILDPATH)$(notdir $(basename $@)).ladot.tex > tmp/$(notdir $(basename $@))_fm
	cp $(BUILDPATH)$(notdir $(basename $@)).ladot.eps tmp/$(notdir $(basename $@))_fm.eps
	cd tmp  && fragmaster
	cp tmp/$(notdir $(basename $@)).pdf $@
	rm -rf tmp

$(IMAGEPATH)%.pdf: $(BUILDPATH)%.ladotdot.ladot.tex $(BUILDPATH)%.ladotdot.ladot.eps
		rm -rf tmp
		mkdir tmp
		cat fm_header $(BUILDPATH)$(notdir $(basename $@)).ladotdot.ladot.tex > tmp/$(notdir $(basename $@))_fm
		cp $(BUILDPATH)$(notdir $(basename $@)).ladotdot.ladot.eps tmp/$(notdir $(basename $@))_fm.eps
		cd tmp && fragmaster
		cp tmp/$(notdir $(basename $@)).pdf $@
		rm -rf tmp

$(BUILDPATH)%.ladot.tex: $(BUILDPATH)%.ladot
	cd $(BUILD) && ladot $(notdir $<)

$(BUILDPATH)%.ladot.eps: $(BUILDPATH)%.ladot
	cd $(BUILD) && ladot $(notdir $<)

$(BUILDPATH)%.ladotdot.ladot.tex: $(BUILDPATH)%.ladotdot
		cd $(BUILD) && ladotdot $(notdir $<)

$(BUILDPATH)%.ladotdot.ladot.eps: $(BUILDPATH)%.ladotdot
		cd $(BUILD) && ladotdot $(notdir $<)

$(BUILDPATH)%.ladotdot: $(FIGPATH)%.dot
	cp $< $@

%.data: %_system.m %_system_aux.m %_system_init.m
	cd $(dir $<) && octave $(notdir $<)

data/%.data: kappa/%.m
	cd $(dir $<) && octave $(notdir $<)

kappa/%.m: kappa/%.ka
	KaDe -l 6 --count Occurrences $< --output $(basename $<) --output-plot ../data/$(basename $(notdir $<)).data

$(BUILD)/%.cmx: $(BUILD)/%.ml
	cd $(BUILD) && $(OCAMLOPT) -c $(notdir $<)

$(BUILD)/%.ml: $(FIGPATH)%.sig.ml
	mkdir -p $(BUILD)
	cp $< $@

$(BUILD)/%.ml: $(FIGPATH)%.figs.ml
	mkdir -p $(BUILD)
	cp $< $@

$(IMAGEPATH)%.fig: $(FIGPATH)%.fig
	mkdir -p $(IMAGE)
	cp $< $@


$(IMAGEPATH)%.pdf_t: $(IMAGEPATH)%.pdf $(IMAGEPATH)%.fig
	fig2dev -L pdftex_t -p $^ $@

$(IMAGEPATH)%.pdf:  $(IMAGEPATH)%.fig
	fig2dev -L pdftex $< $@

$(IMAGEPATH)%.pdf: $(IMAGEPATH)%.eps
	ps2pdf -dEPSCrop $< $@

$(IMAGEPATH)%.pdf: $(FIGPATH)%.pdf
	cp $< $@


demo: $(HTML)

sessions:
	mkdir sessions


clean:
	rm -f *.blg *.out *.toc *.todo *.idx *.bbl *~ \#* *.dvi *.aux *.log  *.bak *.html $(PREIMAGES) $(BBL) $(TEX) $(DATA) $(WITNESS)
	rm -fr sessions $(BIN) $(BUILD)

clean_figure:
	rm -fr $(IMAGEPATH) $(BUILD) $(BIN)

clean_benchmarks:
	cd benchmarks && $(MAKE) clean

clean_kappa:
	cd kappa && $(MAKE) clean

clean_octave:
	cd octave && $(MAKE) clean_data

clean_all:
	$(MAKE) clean
	$(MAKE) clean_figure
	$(MAKE) clean_benchmarks
	$(MAKE) clean_kappa
	$(MAKE) clean_octave
	rm -fr $(IMAGEPATH) $(BUILD) $(BIN)

force:
	rm -f $(OUTPUT)
	$(MAKE)

nonstop:
	$(MAKE) NONSTOP=-halt-on-error

help:
	@echo make figure -> to generate all the figures (GKappa is required)
	@echo make        -> to generete the pdf (figures shall be compiled before)
	@echo make clean  -> remove latex intermediary files
	@echo make clean_figures -> remove the pdf files for figures
	@echo make clean_all -> remove everything including the final output
