OUTPUT?=benchmarks.tex
MODEL?=
TITLE?=
KASA?=KaSa
OUTPUTDIRECTORY?=output

TIMEOUTBIN=/usr/local/bin/gtimeout $(TIMEOUT)
TIMEOUT?=500s

DEALWITHERRORS=||exit 0
OPTIONS?=
COMMON_OPTIONS = --reset-all --backdoor-file $(OUTPUT) $(MODEL) $(OPTIONS) --output-directory $(OUTPUTDIRECTORY)

all:

clean:
	rm -rf output
	rm -f benchmarks.tex  

hline:
	echo '\\hline' >> $(OUTPUT)

title:
	echo " & nombre & nombre de & nombre de & temps " > $(OUTPUT)
	echo '\\cr' >> $(OUTPUT)
	echo " mod{\\egrave}le & de & contraintes & r{\\egrave}gles mortes & d'analyse " >> $(OUTPUT)
	echo '\\cr' >> $(OUTPUT)
	echo " & r{\\egrave}gles & inf{\\ecute}r{\\ecute}es &  d{\ecute}tect{\ecute}es & (secondes)" >> $(OUTPUT)
	echo '\\cr\\hline \c\\hline \c'>> $(OUTPUT)

line:
	echo '$(TITLE)\c' >> $(OUTPUT)
	echo ' & \c' >> $(OUTPUT)
	$(TIMEOUTBIN) $(KASA) $(COMMON_OPTIONS) --compute-reachability-analysis --backdoor-dump-nbr-rules $(DEALWITHERRORS)
	echo ' & \c' >> $(OUTPUT)
	$(TIMEOUTBIN) $(KASA) $(COMMON_OPTIONS) --compute-reachability-analysis --backdoor-dump-nbr-nr-constraints $(DEALWITHERRORS)
	echo ' & \c' >> $(OUTPUT)
	$(TIMEOUTBIN) $(KASA) $(COMMON_OPTIONS) --compute-reachability-analysis --backdoor-dump-nbr-dead-rules $(DEALWITHERRORS)
	echo ' & \c' >> $(OUTPUT)
	$(TIMEOUTBIN) $(KASA) $(COMMON_OPTIONS) --compute-reachability-analysis --backdoor-dump-timing $(DEALWITHERRORS)
	echo '\\cr \n\c' >> $(OUTPUT)
