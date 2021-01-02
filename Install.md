# How to run PyCalCal #

# Generate code and documentation #

In order to generate pycalcal code and docs you need:

   * a Python version equal to or greater than 2.5 and less than 3.0.
   * [mpmath](http://code.google.com/p/mpmath/)
   * The following LaTeX packages:
     - tlmgr install siunitx
     - tlmgr install euro
     - tlmgr install wasysym
     - tlmgr install wasy
     - tlmgr install ulem
     - tlmgr install lettrine
     - tlmgr install minifp
     - tlmgr install textpath
     - tlmgr install astro
     - tlmgr install media9
     - tlmgr install ocgx2
     - tlmgr install epstopdf
     - tlmgr install listings
   * noweb, https://www.cs.tufts.edu/~nr/noweb/
     Remember to set TEXINPUTS accordingly, i.e.
     
     ```
     export TEXINPUTS=/usr/local/opt/noweb/tex/generic/noweb//:
     ```
   * asymptote, https://asymptote.sourceforge.io/
   
Generate the Makefile:

```
./makemake.sh
```

Make all:

```
# setting EPSTOPDF on the fly is only needed if it is not in your PATH
EPSTOPDF=/Users/espin/Library/TinyTeX//bin/x86_64-darwin/epstopdf \
   TEXINPUTS=/usr/local/opt/noweb/tex/generic/noweb//: \
   make all
```

Then it is enough to place `pycalcal.py` file in any place where python can load it, i.e. your current working directory.

## Getting started ##
The first thing to do is to load pycalcal. From your python prompt type (omit >>> of course...)
```python
   >>> import pycalcal as pcc
```

You are now ready to play with calendars, i.e. converting dates from gregorian to coptic:
```python
>>> pcc.coptic_from_fixed(pcc.fixed_from_gregorian(pcc.gregorian_date(2006, 2, 5)))
[1722, 5, 28]
```

or finding out when next year's Easter will be

```python
>>> pcc.gregorian_from_fixed(pcc.easter(2010))
[20010, 4, 4]
```

or Hindu's New Year for 2009:
```python
>>> pcc.gregorian_from_fixed(pcc.diwali(2009)[0])
[2009, 10, 19]
```
Enjoy!!!

# How to build PyCalCal #
PyCalCal is generated following the literate programming paradigm.
In order generate source code and documentation you need the following tools:
   * [noweb](http://www.cs.tufts.edu/~nr/noweb/)
     Installing noweb has never been a problem on all platforms/operating
     systems I tried: MS Windows XP (with cygwin), HP-UX, Mac OS X Snow
     Leopard. 
   * make
   * sh (unix shell)
   * LaTeX distribution, i.e. TeXLive2010 (with asymptote!)

Unit tests' line coverage metrics are gathered by [coverage](http://nedbatchelder.com/code/coverage/)

Here are the steps for buiding:

1. execute *makemake.sh* in order to create the Makefile
>   $ ./makemake.sh

2. generate the code:
>   $ make pycalca.py

3. generate the doc:
>   $ make figures
>   $ make pycalcal.pdf

4. run the tests:
>   $ make test


#summary Installation and Getting started instructions for PyCalCal.

= Installation instructions =
!PyCalCal is a fairly self contained Python library.
In order to use it you only need to have installed
  * a Python version greater or equal to 2.5.4 and lower than 3.0
  * [http://code.google.com/p/mpmath/ mpmath] package
Then it is enough to place `pycalcal.py` file in any place where python can load it, i.e. your current working directory.
You can find `pycalcal.py` in the distibution file listed in the [http://code.google.com/p/pycalcal/downloads/list Download tab]. Alternatively you can generate it from your own copy of the source distribution.


If you want to regenerate the full distribution from source you need the following additional tools:
  * [http://www.cs.tufts.edu/~nr/noweb/ noweb]
  * [http://nedbatchelder.com/code/coverage/ coverage]
Should you have a problem ask via the !PyCalCal google group.


= Try it on the web =
[http://calendrica.appspot.com]
