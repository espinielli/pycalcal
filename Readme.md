*PyCalCal* is a Python library that makes it easy:

*   to convert dates from/to 31 different calendars
*   to calculate special dates/times such as holidays, astronomical events
    (equinoxes, solstices ...), sunrise/sunset
*   to try it [online][PyCalCalOnline] (_experimental !!_)

*PyCalCal* is an implementation in Python of _CALENDRICA 3.0_, which is the
 Common Lisp implementation of the functions described in the [book][CalCalBook]

    Nachum Dershowitz, Edward M. Reingold "Calendrical Calculations"
    Cambridge University Press; 3rd edition (December 10, 2007)
    Paperback; ISBN-13: 9780521702386
    E. M. Reingold and N. Dershowitz,
    3rd ed., Cambridge University Press, 2008.

Some of the algorithms are from:

    Jean Meeus "Astronomical Algorithms"
    Willmann-Bell, Inc.; 2nd edition (1998 with corrections as of June 15, 2005);
    ISBN: 0-943396-61-1


### STATUS ###
I, Enrico Spinielli, implemented PyCalCal for fun and to further glory in the
beauty of the work of the authors of the book.
Having said so, PyCalCal code is pretty well tested and usable.

See '_STATUS_' file for further details.

### Installation ###

See Install.md

### Acknowledgements ###
I want to thank:

* Prof.s Reingold and Dershowitz for their prompt replies to my (sometimes
  silly, other times pertinent) questions
* my family, Gilda and the kids, for the good they bring to my life (and the
  time they let me spend with all this)
* my parents and parents-in-law for their past and continuous support

### Notes ###
_CALENDRICAL 3.0_ is written and copyrighted by E. M. Reingold and
N. Dershowitz as described in file 'COPYRIGHT\_DERSHOWITZ_REINGOLD'

### Resources ###
Prof. Reingold mantains a [companion site for the book][BookCompanionSite].

The [resource page of the book][BookResPage] at Cambridge University Press
provides a link to a zipped version of 'calendrica-3.0.cl' which I included
unzipped in PyCalCal project repo in order to be able to reference the relevant
snippets of the original implementation.

Files '_dates[1-5].tex_', containing test data, have been kindly provided to me
by Prof. Reingold.


[CalCalBook]: http://www.cambridge.org/ch/academic/subjects/computer-science/computing-general-interest/calendrical-calculations-3rd-edition "Calendrical Calculations"
[BookResPage]: http://www.cambridge.org/ch/academic/subjects/computer-science/computing-general-interest/calendrical-calculations-3rd-edition#resources "Calendrical Calculations's resource page at Cambridge University Press"
[C3Zip]: http://www.cambridge.org/download_file/202891 "Zip file of Calendrica 3.0 source at Cambridge University Press"
[PyCalCalOnline]: http://calendrica.appspot.com "PyCalCal online application"
[BookCompanionSite]: http://emr.cs.iit.edu/home/reingold/calendar-book/third-edition/index.html "Calendrical Calculations' book companion site"
