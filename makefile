#
# $Id$
#

SHTOOL=shtool
RM=rm -f

DISTFILE=cl-tftp-$(VERS).tar.gz
VERSION_BIT=-n CL-TFTP -p CL-TFTP -l txt version.txt

VERS:=$(shell shtool version -d short $(VERSION_BIT))

clean:
	$(RM) *.fasl *.~

fixperm:
	$(SHTOOL) fixperm -v * 

setv:
	$(SHTOOL) subst -e "s;:version.*;:version \"$(VERS)\";" cl-tftp.asd

version:
	@echo "This CL-TFTP is version $(VERS)".

tag:
	tag=`echo CL_TFTP_$(VERS) | sed -e 's/\./_/g' -e 's/-/_/g'`; \
	cvs tag $$tag

dist: clean fixperm setv
	shtool tarball -v -o $(DISTFILE) -c 'gzip -9' \
	    -e 'CVS,\.cvsignore,makefile,version.txt,*.tar.gz,*.fasl,*~' .

sign: dist
	gpg -b -a $(DISTFILE)
	md5sum $(DISTFILE) > $(DISTFILE).md5

new-version:
	$(SHTOOL) version -iv  $(VERSION_BIT)

new-release:
	$(SHTOOL) version -ir  $(VERSION_BIT)

new-patch:
	$(SHTOOL) version -il  $(VERSION_BIT)
