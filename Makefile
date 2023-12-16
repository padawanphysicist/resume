# Name of your emacs binary
EMACS=emacs

BATCH=$(EMACS) --debug --batch --no-init-file	\
	--load elisp/ox-altacv.el					\
	--load elisp/publish.el

# Update LaTeX class
update-altacv:
	curl -L	https://raw.githubusercontent.com/liantze/AltaCV/main/altacv.cls --output texmf/tex/latex/altacv.cls

build: victor-santos.tex
	TEXMFHOME=./texmf arara -lv victor-santos.tex

pdf: victor_santos.org
	@$(BATCH) --funcall org-publish-pdf

.PHONY: clean
clean:
	rm -f *.pdf *.out *aux *bbl *blg *log *toc *.ptb *.tod *.fls *.fdb_latexmk *.lof *.bcf *.xml *.xmpi
