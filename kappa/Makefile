KAPPA=$(wildcard *.ka)

OCTAVE=$(patsubst %.ka,%.m,$(KAPPA))
DATA=$(patsubst %.ka,../data/%.data,$(KAPPA))

octave: $(OCTAVE)
data: $(DATA)

clean_data:
	rm $(DATA)

clean_octave:
	rm $(OCTAVE)

%.m: %.ka
	KaDe -l 6 --count Occurrences $< --output $(basename $<) --output-plot ../data/$(basename $<).data

../data/%.data: %.m
	octave $<
