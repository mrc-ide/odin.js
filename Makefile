PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')
RSCRIPT = Rscript --no-init-file

all: install

test:
	${RSCRIPT} -e 'library(methods); devtools::test()'

roxygen:
	@mkdir -p man
	${RSCRIPT} -e "library(methods); devtools::document()"

install:
	R CMD INSTALL .

build:
	R CMD build .

check:
	_R_CHECK_CRAN_INCOMING_=FALSE make check_all

check_all:
	${RSCRIPT} -e "rcmdcheck::rcmdcheck(args = c('--as-cran', '--no-manual'))"

autodoc:
	${RSCRIPT} autodoc.R process

pkgdown:
	rm -fr docs/simple
	${RSCRIPT} -e "library(methods); pkgdown::build_site()"

example:
	${RSCRIPT} -e 'devtools::load_all(); odin.js::odin_js_example("inst/models/sir.R", "simple", "docs/simple")'

website: pkgdown example
	./scripts/update_web.sh

README.md: README.Rmd
	Rscript -e 'library(methods); devtools::load_all(); knitr::knit("README.Rmd")'
	sed -i.bak 's/[[:space:]]*$$//' $@
	rm -f $@.bak

clean:
	rm -f src/*.o src/*.so src/*.dll

coverage:
	Rscript -e 'covr::shine(covr::package_coverage(quiet=FALSE))'

js: inst/dopri.js

js/dopri.js: js/package.json js/in.js
	./js/build

inst/support.min.js: inst/support.js
	uglifyjs $< > $@

inst/dopri.js: js/dopri.js
	mkdir -p inst
	cp $< $@
	cp js/node_modules/dopri/LICENCE inst/LICENSE.dopri
	cp js/dopri.min.js inst

.PHONY: all test document install vignettes build js example
