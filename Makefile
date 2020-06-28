version = 0.0
dist = dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/glam-$(version)/x/glam/build/glam/glam.jsexe
example = examples/nats.glam

.PHONY: www
www: www/index.html www/glam.min.js

.PHONY: FORCE
FORCE:

$(dist)/all.js: FORCE
	cabal build --ghcjs exe:glam

www/glam.min.js: $(dist)/all.js $(dist)/all.js.externs
	closure-compiler -O advanced -W QUIET --jscomp_off undefinedVars --externs $(dist)/all.js.externs --js $(dist)/all.js --js_output_file $@

www/index.html: www/index.template.html $(example)
	example=$$(< $(example)) envsubst '$$example' < $< > $@
