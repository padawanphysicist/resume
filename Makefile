RESUME=victor_santos
PACKAGE=http://lmtx.pragma-ade.nl/install-lmtx/context-linux-64.zip
DESTDIR=$$HOME/bin/context

all: pdf txt

$(RESUME).pdf: $(RESUME).ctex
	context $(RESUME).ctex

$(RESUME).txt: $(RESUME).pdf
	pdftotext -layout $(RESUME).pdf
	sed -i.bak -e 's/ \.//g' -e '/Page [1-9].*/d' $(RESUME).txt
	# Remove double empty lines
	sed -i.bak -e '/^$$/N;/^\n$$/D' $(RESUME).txt
	# Remove ASCII linefeed control sequences
	sed -i.bak -e 's/\o14//g' -e 's/\o12//g' $(RESUME).txt

.PHONY: pdf txt clean veryclean install

pdf: $(RESUME).pdf

txt: $(RESUME).txt

clean:
	@rm -f $(RESUME).{tuc,log,txt.bak}

veryclean: clean
	@rm -f $(RESUME).{pdf,txt}

install:
	mkdir -p $(DESTDIR)
	( cd $(DESTDIR) && \
		wget $(PACKAGE) && \
		unzip `basename $(PACKAGE)` && \
		sh install.sh )
	echo "export PATH=$(DESTDIR)/tex/texmf-linux-64/bin:\$$PATH" >> ~/.bashrc
