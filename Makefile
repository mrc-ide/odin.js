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
	${RSCRIPT} -e "library(methods); pkgdown::build_site()"

website: pkgdown
	./update_web.sh

README.md: README.Rmd
	Rscript -e 'library(methods); devtools::load_all(); knitr::knit("README.Rmd")'
	sed -i.bak 's/[[:space:]]*$$//' $@
	rm -f $@.bak

clean:
	rm -f src/*.o src/*.so src/*.dll

coverage:
	Rscript -e 'covr::shine(covr::package_coverage(quiet=FALSE))'

js: inst/bundle.js

js/bundle.js: js/package.json js/in.js
	./js/build

inst/bundle.js: js/bundle.js
	mkdir -p inst
	cp $< $@
	cp js/node_modules/dopri/LICENCE inst/LICENSE.dopri
	cp js/bundle.min.js inst

.PHONY: all test document install vignettes build js
