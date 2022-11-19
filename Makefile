PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: build install clean

deps:
	tlmgr install pgf preview xcolor;\
	Rscript -e 'if (!require("Rd2roxygen")) install.packages("Rd2roxygen", repos="http://cran.rstudio.com")'

docs:
	R -q -e 'library(Rd2roxygen); rab(".", build = FALSE)'

build:
	cd ..;\
	R CMD build --no-manual $(PKGSRC);\
	R -e 'reticulate::install_miniconda()'
	R -e 'reticulate::py_install("dominodatalab-data", pip = TRUE)'

build-cran:
	cd ..

install: build
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: build-cran
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran

clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/
