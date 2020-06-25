DIST = dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/glam-0.0/x/glam/build/glam/glam.jsexe

.PHONY: www
www:
	cabal --ghcjs build exe:glam
	closure-compiler -O advanced -W QUIET --jscomp_off undefinedVars --externs $(DIST)/all.js.externs --js $(DIST)/all.js --js_output_file www/glam.min.js
