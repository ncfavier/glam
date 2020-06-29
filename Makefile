version = 0.0
dist = dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/glam-$(version)/x/glam/build/glam/glam.jsexe
example = examples/fib.glam
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
	cabal build --ghcjs exe:glam

www/glam.js: $(dist)/all.js
	cp $< $@

www/glam.min.js: $(dist)/all.js $(dist)/all.js.externs
	closure-compiler -O advanced -W QUIET --jscomp_off undefinedVars \
		--externs $(dist)/all.js.externs --js $< --js_output_file $@

www/index.html: www/index.template.html FORCE
	nats=$$(< examples/nats.glam) fib=$$(< examples/fib.glam) envsubst '$$nats$$fib$$glamjs' < $< > $@
