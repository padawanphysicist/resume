EMACS?=emacs

publish: pub clean
all: pkg pub clean

pkg:
	@echo "Installing Emacs packages..."
	$(EMACS) $(DEBUG) --batch --script lisp/packages.el
	@echo ""
	@echo "[OK] Done."

pub:
	@echo "Publishing index..."
	sed "s@{{root-dir}}@${PWD}/@g" lisp/publish.mo.el > lisp/publish.el
	$(EMACS) $(DEBUG) -Q --batch -l lisp/publish.el index.org --eval "(export t)"
	@echo "[OK] Done."

pdf:
	@echo "Creating pdf..."
	sed "s@{{root-dir}}@${PWD}/@g" lisp/publish.mo.el > lisp/publish.el
	$(EMACS) $(DEBUG) -Q --batch -l lisp/publish.el index.org --eval "(export-pdf t)"
	@rm -rf index.tex
	@find . -iname '*~' -type f -delete
	@echo "[OK] Done."

clean:
	find . -iname '*~' -type f -delete
	rm -rf lisp/publish.el
