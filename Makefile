PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: rd check clean

alldocs: rd readme

figure:
	Rscript -e 'source("example.R")'

	
rd:
	Rscript -e 'library(methods); devtools::document()'
# Rscript -e 'roxygen2::roxygenise(".")'

readme:
	Rscript -e 'rmarkdown::render("README.Rmd", encoding="UTF-8")'

build:
	cd ..;\
	R CMD build $(PKGSRC)

build2:
	cd ..;\
	R CMD build --no-build-vignettes $(PKGSRC)

install:
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: rd build
	cd ..;\
	Rscript -e "rcmdcheck::rcmdcheck('$(PKGNAME)_$(PKGVERS).tar.gz')"

check2: rd build
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz

clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/

giteeinit:
	git remote add gitee git@gitee.com:GuangchuangYu/$(PKGNAME).git;\
	git fetch --all

update:
	git fetch --all;\
	git checkout master;\
	git merge gitee/master;\
	git merge origin/master

push:
	git push gitee master;\
	git push origin master

deploy: gh
	git checkout gh-pages;\
	git add .;\
	git commit -m 'update vignette';\
	git push -u origin gh-pages;\
	git checkout master

gh:
	cd vignettes;\
	Rscript -e "rmarkdown::render('nCov2019.Rmd')";\
	mv nCov2019.html ../.. ;\
	cd ..;\
	git checkout gh-pages;\
	mv ../nCov2019.html index.html
