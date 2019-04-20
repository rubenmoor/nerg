public/app.js: output/.tree_built
	purs bundle './output/**/*.js' -m Main --main Main -o $@

output/.tree_built: src/Main.purs
	psc-package build
	touch $@
