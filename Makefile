pkgname := te.test
srcname := te.test.r
sourcefile_in_pkg := $(pkgname)/R/$(srcname)
mans := $(pkgname)/man/$(srcname)d
version := $(shell cat DESCRIPTION | awk '/^Version: .*/ {print($$2)}')
tarball := $(pkgname)_$(version).tar.gz

main: $(tarball)

$(tarball): $(sourcefile_in_pkg) $(pkgname)/DESCRIPTION $(pkgname)/NAMESPACE $(mans)
	R CMD build $(pkgname)
	mv $(pkgname) $(pkgname)_tmp
	tar -xf $(tarball)
	sed 's/UTC;.*/UTC;REDACTED/' $(pkgname)/DESCRIPTION > $(pkgname)/DESCRIPTION2
	rm -R $(pkgname)/DESCRIPTION
	mv $(pkgname)/DESCRIPTION2 $(pkgname)/DESCRIPTION
	rm -R $(tarball)
	tar -czvf $(tarball) $(pkgname)
	rm -R $(pkgname)
	mv $(pkgname)_tmp $(pkgname)


$(mans) $(pkgname)/NAMESPACE: $(sourcefile_in_pkg) $(pkgname)/DESCRIPTION
	Rscript -e '\
	library("roxygen2"); \
	roxygen2::roxygenize(package.dir = "$(pkgname)"); \
	'
	touch $@

$(sourcefile_in_pkg): $(srcname) | $(pkgname) $(pkgname)/R
	cp $(srcname) $(sourcefile_in_pkg)

$(pkgname)/DESCRIPTION: DESCRIPTION | $(pkgname) $(pkgname)/R
	cp DESCRIPTION $(pkgname)/DESCRIPTION

$(pkgname)/R $(pkgname):
	Rscript -e '\
	source("$(srcname)", encoding = "UTF-8"); \
	package.skeleton(name="$(pkgname)",code_files="$(srcname)",encoding="UTF-8"); \
	'
	rm -R $(pkgname)/man
	rm -R $(pkgname)/NAMESPACE
	rm -R $(pkgname)/DESCRIPTION
	rm -R $(pkgname)/Read-and-delete-me

clean:
	rm $(tarball)
	rm -R $(pkgname)