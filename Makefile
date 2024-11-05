clean:
	@rm -f init.elc configuration.el configuration.elc

compile: init.el configuration.org clean
	@emacs -Q --batch -l 'lisp/compile.el'
