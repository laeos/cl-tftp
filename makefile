#
# $Id$
#

SHTOOL=shtool
RM=rm -f
DISTFILE=cl-tftp-$(VERS).tar.gz
PROJECT=cl-tftp
VERS:=$(shell sed -ne '/:version/ { s/^.* "\(.*\)"$$/\1/p }' cl-tftp.asd)

clean:
	$(RM) *.fasl *.~

fixperm:
	$(SHTOOL) fixperm -v * 

project:
	@echo $(PROJECT)

version: 
	@echo $(VERS)

tag:
	svn cp $(cltftp)/trunk $(cltftp)/tags/cl-tftp-$(VERS)

dist: clean fixperm 
	shtool tarball -v -o $(DISTFILE) -c 'gzip -9' \
	    -e 'CVS,\.svn,\.cvsignore,makefile,version.txt,*.tar.gz,*.fasl,*~,.*.swp' .

sign: dist
	gpg -b -a $(DISTFILE)
	md5sum $(DISTFILE) > $(DISTFILE).md5
