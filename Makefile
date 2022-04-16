version = 0.0
dist = dist-newstyle/build/js-ghcjs/ghcjs-8.10.7/glam-$(version)/x/glam/build/glam/glam.jsexe
ifdef dev
	export glamjs = glam.js
else
	export glamjs = glam.min.js
endif

.PHONY: docs
docs: docs/index.html docs/$(glamjs)

.PHONY: FORCE
FORCE:

$(dist)/all.js: FORCE
	cabal build --ghcjs

docs/glam.js: $(dist)/all.js
	cp $< $@

docs/glam.min.js: $(dist)/all.js $(dist)/all.js.externs
	closure-compiler -O advanced -W quiet --jscomp_off undefinedVars \
		--externs $(dist)/all.js.externs --js $< --js_output_file $@

docs/index.html: docs/index.template.html FORCE
	naturals=$$(< examples/naturals.glam) \
	fibonacci=$$(< examples/fibonacci.glam) \
	primes=$$(< examples/primes.glam) \
	y=$$(< examples/y.glam) \
	    envsubst '$$naturals$$fibonacci$$primes$$y$$glamjs' < $< > $@
