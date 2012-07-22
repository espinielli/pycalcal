# AUTOMATICALLY GENERATED FROM pycalcal.nw: ANY CHANGES WILL BE OVERWRITTEN.

################################################################################
# Author: Enrico Spinielli
#
# Makefile for my noweb project on calendrical calculations in Python.
# Got hints from Makefile in Noweb distibution
#
################################################################################

VERSION = 1.0.0
NW_MAIN=pycalcal.nw
NW_SRC=
TOOLS=prefix excerpt premarkup blankexcerpt blankpremarkup sconstruct
PYTHON_SITE_PACKAGES=/Library/Python/2.6/site-packages
UNIT_TEST_FILES=basicCodeUnitTest.py \
		egyptianAndArmenianCalendarsUnitTest.py \
		gregorianCalendarUnitTest.py \
		isoCalendarUnitTest.py \
		julianCalendarUnitTest.py \
		copticAndEthiopicCalendarsUnitTest.py \
		ecclesiasticalCalendarsUnitTest.py \
		islamicCalendarUnitTest.py \
		hebrewCalendarUnitTest.py \
		mayanCalendarsUnitTest.py \
		oldHinduCalendarsUnitTest.py \
		balineseCalendarUnitTest.py \
		timeAndAstronomyUnitTest.py \
		persianCalendarUnitTest.py \
		bahaiCalendarUnitTest.py \
		frenchRevolutionaryCalendarUnitTest.py \
		chineseCalendarUnitTest.py \
		modernHinduCalendarsUnitTest.py \
		tibetanCalendarUnitTest.py \
		astronomicalLunarCalendarsUnitTest.py

# We can use:
# PREMARKUP=./premarkup $(NW_MAIN)
# PREMARKUP=./blankpremarkup $(NW_MAIN)
PREMARKUP=cat $(NW_MAIN)

NOTANGLE_PURE=cat $(NW_MAIN) | notangle
NOTANGLE=$(PREMARKUP) | notangle
NOWEAVE=$(PREMARKUP)  | noweave -n -delay
NODEFS=$(PREMARKUP)   | nodefs
#LATEX=pdflatex --include-directory=$$(cygpath -w /usr/local/noweb/texmf)
LATEX=latex
PDFLATEX=pdflatex
BIBTEX=bibtex

# to be used only when there are multiple .nw files
NOINDEX=noindex
# change to ">" to ensure all sources are always made
CPIF=| cpif

NW_ALL=$(NW_MAIN) $(NW_SRC)

.SUFFIXES:
.SUFFIXES: .nw .tex .py .dvi .defs .html .pdf .mp .asy .mps .gv .png .cl

.nw.py:
	$(NOTANGLE) -filter btdefn -R$*.py - $(CPIF) $*.py

.nw.cl:
	$(NOTANGLE) -filter btdefn -R$*.cl - $(CPIF) $*.cl

.nw.html:
	$(NOWEAVE) -filter l2h -filter btdefn -index -html $*.nw $(CPIF) $*.html

.nw.defs:
	$(NODEFS) - $(CPIF) $*.defs

.nw.tex:
	$(NOWEAVE) -filter btdefn -index - $(CPIF) $*.tex

.tex.dvi:
	$(LATEX) $*; \
	$(NOINDEX) $*;\
	if grep -s 'There were undefined references' $*.log;\
	then $(BIBTEX) $*; fi;\
	while grep -s 'Rerun to get cross-references right' $*.log;\
	do $(LATEX) $*;\
	done; \

.tex.pdf:
	$(PDFLATEX) $*; \
	$(NOINDEX) $*; \
	if grep -s 'There were undefined references' $*.log;\
	then $(BIBTEX) $*; fi;\
	while grep -s 'Rerun to get cross-references right' $*.log;\
	do $(PDFLATEX) $*;\
	done; \

.mp.mps:
	mpost -tex=pdflatex $*.mp

.mp.pdf:
	mptopdf --latex $*.mp

.asy.pdf:
	asy -tex pdflatex $*.asy

.nw.gv:
	$(NOTANGLE) -filter btdefn -R$*.gv - $(CPIF) $*.gv

.gv.png:
	dot -Tpng $*.gv > $*.png

# %.chk (this is a check for latex)
# Do not delete the following targets:
.PRECIOUS: %.aux %.bbl

.PHONY : all
all: tools code tests doc 

.PHONY : tools
tools:
	if [[ ! -x "prefix" ]]; \
	then \
		$(MAKE) $(TOOLS); \
	fi;


.PHONY : code
code: pycalcal.py pycalcaltests.py

.PHONY : tests
tests: testSunset.cl
	$(MAKE) testdata
	$(MAKE) $(UNIT_TEST_FILES)


.PHONY : doc
doc:
	-$(MAKE) figures
	-$(MAKE) index
	$(MAKE) pycalcal.pdf


###############################
.PHONY : index
index: pycalcal.defs

pycalcal.defs: $(NW_MAIN) premarkup
	$(NODEFS) - $(CPIF) $*.defs

#all.defs: pycalcal.defs
#	cat pycalcal.defs ${CPIF} all.defs
#	sort -u $(NW_ALL:.nw=.defs) $(CPIF) all.defs
###############################


###############################
.PHONY : figures
figures: fig_ra-dec.pdf fig_ecliptic.pdf fig_alt-az.pdf
###############################


###############################
DISTRO_FILES=$(NW_MAIN) $(NW_MAIN:.nw=.py) $(NW_MAIN:.nw=.pdf) \
		README INSTALL STATUS COPYRIGHT_DERSHOWITZ_REINGOLD \
		makemake.sh \
		Makefile \
		calendrica-3.0.cl \
		calendrica-3.0.errata.cl \
		$(NW_MAIN:.nw=.tex) \
		$(NW_MAIN:.nw=.bib) \
		figure.mp \
		astro.mp \
		alt-az.asy \
                testSunset.cl

.PHONY : distro
distro: all
	tar -czf pycalcal_$$(date +"%Y%m%d%H%M").tar.gz $(DISTRO_FILES)
###############################


###############################
pycalcaltests.py:
	$(NOTANGLE) -filter btdefn -R$*.py - $(CPIF) $*.py

testSunset.cl:
	$(NOTANGLE) -filter btdefn -R$*.cl - $(CPIF) $*.cl


.PHONY : check
check: code tests
	python pycalcaltests.py 2>&1 | tee testResult.txt

.PHONY : check1by1
check1by1: code tests
	for t in $$(cat Makefile | grep -e '^[^ 	]*UnitTest.py:' | \
		cut -f1 -d':' | grep -v appendix); \
	do \
		$(MAKE) $$t; \
	done

# create all unit test files
.PHONY : ut
ut: 
	$(MAKE)  $$(cat pycalcal.nw | grep -e '^<<.*UnitTest.py>>=' | \
		grep -v appendix | sed -e 's/<<//g' -e 's/>>=//g')
#	$(MAKE) $(UNIT_TEST_FILES)


basicCodeUnitTest.py: appendixCUnitTest.py
	$(NOTANGLE) -filter btdefn -R$*.py - > $*.py

.PHONY : basicCodeUnitTest
basicCodeUnitTest: basicCodeUnitTest.py
	python $@.py 2>&1 | tee $@_result.txt


egyptianAndArmenianCalendarsUnitTest.py: appendixCUnitTest.py
	$(NOTANGLE) -filter btdefn -R$*.py - > $*.py

.PHONY : egyptianAndArmenianCalendarsUnitTest
egyptianAndArmenianCalendarsUnitTest: egyptianAndArmenianCalendarsUnitTest.py
	python $@.py 2>&1 | tee $@_result.txt

gregorianCalendarUnitTest.py: appendixCUnitTest.py
	$(NOTANGLE) -filter btdefn -R$*.py - > $*.py

.PHONY : gregorianCalendarUnitTest
gregorianCalendarUnitTest: gregorianCalendarUnitTest.py
	python $@.py 2>&1 | tee $@_result.txt


isoCalendarUnitTest.py: appendixCUnitTest.py
	$(NOTANGLE) -filter btdefn -R$*.py - > $*.py

.PHONY : isoCalendarUnitTest
isoCalendarUnitTest: isoCalendarUnitTest.py
	python $@.py 2>&1 | tee $@_result.txt

julianCalendarUnitTest.py: appendixCUnitTest.py
	$(NOTANGLE) -filter btdefn -R$*.py - > $*.py

.PHONY : julianCalendarUnitTest
julianCalendarUnitTest: julianCalendarUnitTest.py
	python $@.py 2>&1 | tee $@_result.txt

copticAndEthiopicCalendarsUnitTest.py: appendixCUnitTest.py
	$(NOTANGLE) -filter btdefn -R$*.py - > $*.py

.PHONY : copticAndEthiopicCalendarsUnitTest
copticAndEthiopicCalendarsUnitTest: copticAndEthiopicCalendarsUnitTest.py
	python $@.py 2>&1 | tee $@_result.txt

ecclesiasticalCalendarsUnitTest.py: appendixCUnitTest.py
	$(NOTANGLE) -filter btdefn -R$*.py - > $*.py

.PHONY : ecclesiasticalCalendarsUnitTest
ecclesiasticalCalendarsUnitTest: ecclesiasticalCalendarsUnitTest.py
	python $@.py 2>&1 | tee $@_result.txt

islamicCalendarUnitTest.py: appendixCUnitTest.py
	$(NOTANGLE) -filter btdefn -R$*.py - > $*.py

.PHONY : islamicCalendarUnitTest
islamicCalendarUnitTest: islamicCalendarUnitTest.py
	python $@.py 2>&1 | tee $@_result.txt

hebrewCalendarUnitTest.py: appendixCUnitTest.py
	$(NOTANGLE) -filter btdefn -R$*.py - > $*.py

.PHONY : hebrewCalendarUnitTest
hebrewCalendarUnitTest: hebrewCalendarUnitTest.py
	python $@.py 2>&1 | tee $@_result.txt

mayanCalendarsUnitTest.py: appendixCUnitTest.py
	$(NOTANGLE) -filter btdefn -R$*.py - > $*.py

.PHONY : mayanCalendarsUnitTest
mayanCalendarsUnitTest: mayanCalendarsUnitTest.py
	python $@.py 2>&1 | tee $@_result.txt

oldHinduCalendarsUnitTest.py: appendixCUnitTest.py
	$(NOTANGLE) -filter btdefn -R$*.py - > $*.py

.PHONY : oldHinduCalendarsUnitTest
oldHinduCalendarsUnitTest: oldHinduCalendarsUnitTest.py
	python $@.py 2>&1 | tee $@_result.txt

balineseCalendarUnitTest.py: appendixCUnitTest.py
	$(NOTANGLE) -filter btdefn -R$*.py - > $*.py

.PHONY : balineseCalendarUnitTest
balineseCalendarUnitTest: balineseCalendarUnitTest.py
	python $@.py 2>&1 | tee $@_result.txt

timeAndAstronomyUnitTest.py: appendixCUnitTest.py
	$(NOTANGLE) -filter btdefn -R$*.py - > $*.py

.PHONY : timeAndAstronomyUnitTest
timeAndAstronomyUnitTest: timeAndAstronomyUnitTest.py
	python $@.py 2>&1 | tee $@_result.txt

persianCalendarUnitTest.py: appendixCUnitTest.py
	$(NOTANGLE) -filter btdefn -R$*.py - > $*.py

.PHONY : persianCalendarUnitTest
persianCalendarUnitTest: persianCalendarUnitTest.py
	python $@.py 2>&1 | tee $@_result.txt

bahaiCalendarUnitTest.py: appendixCUnitTest.py
	$(NOTANGLE) -filter btdefn -R$*.py - > $*.py

.PHONY : bahaiCalendarUnitTest
bahaiCalendarUnitTest: bahaiCalendarUnitTest.py
	python $@.py 2>&1 | tee $@_result.txt

frenchRevolutionaryCalendarUnitTest.py: appendixCUnitTest.py
	$(NOTANGLE) -filter btdefn -R$*.py - > $*.py

.PHONY : frenchRevolutionaryCalendarUnitTest
frenchRevolutionaryCalendarUnitTest: frenchRevolutionaryCalendarUnitTest.py
	python $@.py 2>&1 | tee $@_result.txt

chineseCalendarUnitTest.py: appendixCUnitTest.py
	$(NOTANGLE) -filter btdefn -R$*.py - > $*.py

.PHONY : chineseCalendarUnitTest
chineseCalendarUnitTest: chineseCalendarUnitTest.py
	python $@.py 2>&1 | tee $@_result.txt

modernHinduCalendarsUnitTest.py: appendixCUnitTest.py
	$(NOTANGLE) -filter btdefn -R$*.py - > $*.py

.PHONY : modernHinduCalendarsUnitTest
modernHinduCalendarsUnitTest: modernHinduCalendarsUnitTest.py
	python $@.py 2>&1 | tee $@_result.txt

tibetanCalendarUnitTest.py: appendixCUnitTest.py
	$(NOTANGLE) -filter btdefn -R$*.py - > $*.py

.PHONY : tibetanCalendarUnitTest
tibetanCalendarUnitTest: tibetanCalendarUnitTest.py
	python $@.py 2>&1 | tee $@_result.txt

astronomicalLunarCalendarsUnitTest.py: appendixCUnitTest.py
	$(NOTANGLE) -filter btdefn -R$*.py - > $*.py

.PHONY : astronomicalLunarCalendarsUnitTest
astronomicalLunarCalendarsUnitTest: astronomicalLunarCalendarsUnitTest.py
	python $@.py 2>&1 | tee $@_result.txt

appendixCUnitTest.py: $(NW_MAIN:.nw=.py)
	$(NOTANGLE) -filter btdefn -R$*.py - > $*.py

.PHONY : appendixCUnitTest
appendixCUnitTest: appendixCUnitTest.py
	python $@.py 2>&1 | tee $@_result.txt

.PHONY : testdata
testdata: trasformLatexDates2Cvs
	cat dates1.tex | ./trasformLatexDates2Cvs > dates1.csv
	cat dates2.tex | ./trasformLatexDates2Cvs > dates2.csv
	cat dates3.tex | ./trasformLatexDates2Cvs > dates3.csv
	cat dates4.tex | ./trasformLatexDates2Cvs > dates4.csv
	cat dates5.tex | ./trasformLatexDates2Cvs > dates5.csv

.PHONY : coverage
coverage: pycalcaltests.py appendixCUnitTest.py testdata
	coverage -e -x pycalcaltests.py    # -e drop previous
	coverage -b -i -d html pycalcal.py # HTML report for pycalcal.py



trasformLatexDates2Cvs:
	notangle -RtrasformLatexDates2Cvs $(NW_MAIN) $(CPIF) trasformLatexDates2Cvs
	-chmod ug+x trasformLatexDates2Cvs
###############################


.PHONY : missing
missing: extractcc3signatures extractcalcalsignatures
	- cat calendrica-3.0.cl | extractcc3signatures - | sort > /tmp/allcc3
	- cat $(NW_MAIN:.nw=.py) | extractcalcalsignatures - | sort > /tmp/allcalcal
	- diff /tmp/allcc3 /tmp/allcalcal

extractcc3signatures: $(NW_MAIN:.nw=.py)
	notangle -Rextractcc3signatures $(NW_MAIN) $(CPIF) extractcc3signatures
	-chmod ug+x extractcc3signatures

extractcalcalsignatures: $(NW_MAIN:.nw=.py)
	notangle -Rextractcalcalsignatures $(NW_MAIN) $(CPIF) extractcalcalsignatures
	-chmod ug+x extractcalcalsignatures

premarkup: prefix excerpt
	notangle -Rpremarkup $(NW_MAIN) $(CPIF) premarkup
	-chmod ug+x premarkup

prefix:
	notangle -Rprefix $(NW_MAIN) $(CPIF) prefix
	-chmod ug+x prefix

excerpt:
	notangle -Rexcerpt $(NW_MAIN) $(CPIF) excerpt
	-chmod ug+x excerpt

blankpremarkup: prefix blankexcerpt
	notangle -Rblankpremarkup $(NW_MAIN) $(CPIF) blankpremarkup
	-chmod ug+x blankpremarkup

blankexcerpt:
	notangle -Rblankexcerpt $(NW_MAIN) $(CPIF) blankexcerpt
	-chmod ug+x blankexcerpt



sconstruct:
	notangle -RSConstruct $(NW_MAIN) $(CPIF) SConstruct

.PHONY : clean clobber xclean
clobber: clean

clean:
	rm -fR pycalcal.tex $(NW_MAIN:.nw=.py) pycalcaltests.py *.dvi *.aux *.log \
		*.blg *.toc *.bbl *~ *.pyc *.defs *.nwi premarkup prefix \
		excerpt dates?.csv blankexcerpt blankpremarkup \
		trasformLatexDates2Cvs pycalcal.pdf extractcc3signatures \
		extractcalcalsignatures pycalcal.ind pycalcal.out pycalcal.ilg \
		pycalcal.idx *UnitTest.py *UnitTest_result.txt \
		html/ calendrica/ pycalcal*.gz figure.mpx \
		$$(ls | grep  "figure.[0-9][0-9]*") figure-*.pdf fig_*.pdf \
		*.mps *mpx fig_*.0 SConstruct

# ask to remove all files not recognized by hg
xclean: clean
	for f in $$(hg status | grep -e '^?' | sed -e 's/^? //g'); \
   do \
      rm -i $$f; \
   done


.PHONY : webapp
webapp: calendrica.py
	-mkdir -p calendrica/lib
	-cp pycalcal.py calendrica/lib
	-cp -fR $(PYTHON_SITE_PACKAGES)/mpmath calendrica/lib

calendrica.py: $(NW_MAIN:.nw=.py)
	-mkdir calendrica
	$(NOTANGLE) -filter btdefn -R$*.py - $(CPIF) calendrica/calendrica.py
	$(NOTANGLE) -filter btdefn -Rapp.yaml - $(CPIF) calendrica/app.yaml


