RESUME=victor_santos
PACKAGE=http://lmtx.pragma-ade.nl/install-lmtx/context-linux-64.zip
DESTDIR=$$HOME/bin/context

all: en pt clean

en: dist/victor-santos-1page-en.pdf dist/victor-santos-2page-en.pdf
pt: dist/victor-santos-1page-pt_br.pdf dist/victor-santos-2page-pt_br.pdf

.PHONY: dist
dist:
	@mkdir --parents dist

dist/victor-santos-2page-en.pdf: dist resume.tex
	@context --purgeall --mode=twopage,english resume.tex --result=$(notdir $@)
	@mv $(notdir $@) dist

dist/victor-santos-2page-pt_br.pdf: dist resume.tex
	@context --purgeall --mode=twopage,portuguese resume.tex --result=$(notdir $@)
	@mv $(notdir $@) dist

dist/victor-santos-1page-en.pdf: dist resume.tex
	@context --purgeall --mode=onepage,english resume.tex --result=$(notdir $@)
	@mv $(notdir $@) dist

dist/victor-santos-1page-pt_br.pdf: dist resume.tex
	@context --purgeall --mode=onepage,portuguese resume.tex --result=$(notdir $@)
	@mv $(notdir $@) dist

.PHONY: clean clean-all
clean:
	rm -rf *.log *.tuc
clean-all:
	rm -rf dist

# all: pdf txt

# $(RESUME).pdf: $(RESUME).ctex
# 	context $(RESUME).ctex

# $(RESUME).txt: $(RESUME).pdf
# 	pdftotext -layout $(RESUME).pdf
# 	sed -i.bak -e 's/ \.//g' -e '/Page [1-9].*/d' $(RESUME).txt
# 	# Remove double empty lines
# 	sed -i.bak -e '/^$$/N;/^\n$$/D' $(RESUME).txt
# 	# Remove ASCII linefeed control sequences
# 	sed -i.bak -e 's/\o14//g' -e 's/\o12//g' $(RESUME).txt

# .PHONY: pdf txt clean veryclean install

# pdf: $(RESUME).pdf

# txt: $(RESUME).txt

# clean:
# 	@rm -f $(RESUME).{tuc,log,txt.bak}

# veryclean: clean
# 	@rm -f $(RESUME).{pdf,txt}

# install:
# 	mkdir -p $(DESTDIR)
# 	( cd $(DESTDIR) && \
# 		wget $(PACKAGE) && \
# 		unzip `basename $(PACKAGE)` && \
# 		sh install.sh )
# 	echo "export PATH=$(DESTDIR)/tex/texmf-linux-64/bin:\$$PATH" >> ~/.bashrc
