version = 0.0
dist = dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/glam-$(version)/x/glam/build/glam/glam.jsexe
ifdef dev
	export glamjs = glam.js
else
	export glamjs = glam.min.js
endif

.PHONY: www
www: www/index.html www/$(glamjs)

.PHONY: FORCE
FORCE:

$(dist)/all.js: FORCE
	cabal build --ghcjs

www/glam.js: $(dist)/all.js
	cp $< $@

www/glam.min.js: $(dist)/all.js $(dist)/all.js.externs
	closure-compiler -O advanced -W quiet --jscomp_off undefinedVars \
		--externs $(dist)/all.js.externs --js $< --js_output_file $@

www/index.html: www/index.template.html FORCE
	naturals=$$(< examples/naturals.glam) fibonacci=$$(< examples/fibonacci.glam) y=$$(< examples/y.glam) \
	    envsubst '$$naturals$$fibonacci$$y$$glamjs' < $< > $@
