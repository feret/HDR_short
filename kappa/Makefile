KAPPA=$(wildcard *.ka)

OCTAVE=$(patsubst %.ka,%.m,$(KAPPA))
DATA=$(patsubst %.ka,../data/%.data,$(KAPPA))

octave: $(OCTAVE)
data: $(DATA)

clean_data:
	rm -f $(DATA)

clean_octave:
	rm -f $(OCTAVE)

clean: clean_octave clean_data
	rm -rf output 

%.m: %.ka
	KaDe -l 6 --count Occurrences $< --output $(basename $<) --output-plot ../data/$(basename $<).data

../data/%.data: %.m
	octave $<
