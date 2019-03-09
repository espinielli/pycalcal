# How to run PyCalCal #
In order to use the pycalcal you need:

   * a Python version equal to or greater than 2.5 and less than 3.0.
   * [mpmath](http://code.google.com/p/mpmath/)

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
>   $ make pycalcal.py

3. generate the doc:
>   $ make figures
>   $ make pycalcal.pdf

4. run the tests:
>   $ make check 


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
