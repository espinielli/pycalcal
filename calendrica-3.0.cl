;;;; 
;;;; CALENDRICA 3.0 -- Common Lisp
;;;; E. M. Reingold and N. Dershowitz
;;;;
;;;; ================================================================
;;;; 
;;;; The Functions (code, comments, and definitions) contained in this
;;;; file (the "Program") were written by Edward M. Reingold and Nachum
;;;; Dershowitz (the "Authors"), who retain all rights to them except as
;;;; granted in the License and subject to the warranty and liability
;;;; limitations below.  These Functions are explained in the Authors'
;;;; book, "Calendrical Calculations", 3rd ed. (Cambridge University
;;;; Press, 2008), and are subject to an international copyright.
;;;;
;;;; The Authors' public service intent is more liberal than suggested
;;;; by the License below, as are their licensing policies for otherwise
;;;; nonallowed uses such as--without limitation--those in commercial,
;;;; web-site, and large-scale academic contexts.  Please see the
;;;; web-site
;;;;
;;;;     http://www.calendarists.com
;;;;
;;;; for all uses not authorized below; in case there is cause for doubt
;;;; about whether a use you contemplate is authorized, please contact
;;;; the Authors (e-mail: reingold@iit.edu).  For commercial licensing
;;;; information, contact the first author at the Department of Computer
;;;; Science, Illinois Institute of Technology, Chicago, IL 60616-3729 USA.
;;;;
;;;; 1. LICENSE.  The Authors grant you a license for personal use.
;;;; This means that for strictly personal use you may copy and use the
;;;; code, and keep a backup or archival copy also.  The Authors grant you a 
;;;; license for re-use within non-commercial, non-profit softeare provided prominent
;;;; credit is given and the Authors' rights are preserved.  Any other uses,
;;;; including without limitation, allowing the code or its output to be
;;;; accessed, used, or available to others, is not permitted.
;;;;
;;;; 2. WARRANTY.
;;;;
;;;; (a) THE AUTHORS PROVIDE NO WARRANTIES OF ANY KIND, EITHER
;;;;     EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITING THE
;;;;     GENERALITY OF THE FOREGOING, ANY IMPLIED WARRANTY OF
;;;;     MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
;;;;
;;;; (b) THE AUTHORS SHALL NOT BE LIABLE TO YOU OR ANY THIRD PARTIES
;;;;     FOR DAMAGES OF ANY KIND, INCLUDING WITHOUT LIMITATION, ANY LOST
;;;;     PROFITS, LOST SAVINGS, OR ANY OTHER INCIDENTAL OR CONSEQUENTIAL
;;;;     DAMAGES ARISING OUT OF OR RELATED TO THE USE, INABILITY TO USE,
;;;;     OR INACCURACY OF CALCULATIONS, OF THE CODE AND FUNCTIONS
;;;;     CONTAINED HEREIN, OR THE BREACH OF ANY EXPRESS OR IMPLIED
;;;;     WARRANTY, EVEN IF THE AUTHORS OR PUBLISHER HAVE BEEN ADVISED OF
;;;;     THE POSSIBILITY OF THOSE DAMAGES.
;;;;
;;;; (c) THE FOREGOING WARRANTY MAY GIVE YOU SPECIFIC LEGAL
;;;;     RIGHTS WHICH MAY VARY FROM STATE TO STATE IN THE U.S.A.
;;;;
;;;; 3. LIMITATION OF LICENSEE REMEDIES.  You acknowledge and agree that
;;;; your exclusive remedy (in law or in equity), and Authors' entire
;;;; liability with respect to the material herein, for any breach of
;;;; representation or for any inaccuracy shall be a refund of the license
;;;; fee or service and handling charge which you paid the Authors, if any.
;;;;
;;;; SOME STATES IN THE U.S.A. DO NOT ALLOW THE EXCLUSION OR LIMITATION OF
;;;; LIABILITY FOR INCIDENTAL OR CONSEQUENTIAL DAMAGES, SO THE ABOVE
;;;; EXCLUSIONS OR LIMITATION MAY NOT APPLY TO YOU.
;;;;
;;;; 4. DISCLAIMER.  Except as expressly set forth above, the Authors:
;;;;
;;;;   (a) make no other warranties with respect to the material in the
;;;;   Program and expressly disclaim any others;
;;;;
;;;;   (b) do not warrant that the material contained in the Program will
;;;;   meet your requirements or that their operation shall be
;;;;   uninterrupted or error-free;
;;;;
;;;;   (c) license this material on an "as is" basis, and the entire risk
;;;;   as to the quality, accuracy, and performance of the Program is
;;;;   yours, should the code prove defective (except as expressly
;;;;   warranted herein).  You alone assume the entire cost of all
;;;;   necessary corrections.
;;;;
;;;; Sample values for the functions (useful for debugging) are given in
;;;; Appendix C of the book and on the accompanying CD.  
;;;; These sample values are not available electronically.

;;;; Last modified 23 August 2007.

(if (not (find-package "CC3"))
    (defpackage "CC3"))
(in-package "CC3")

(export '(true false bogus rd sunday monday tuesday wednesday
          thursday friday saturday january february march april may
          june july august september october november december kalends
          nones ides ayyam-i-ha arya-solar-year arya-solar-month
          arya-lunar-month arya-lunar-day arya-jovian-period mecca
          spring summer autumn winter new first-quarter full
          last-quarter haifa islamic-locale jerusalem tehran paris
          ujjain hindu-locale quotient day-of-week-from-fixed
          standard-month standard-day standard-year time-of-day
          hour minute seconds 
          fixed-from-moment time-from-moment clock-from-moment
          time-from-clock angle-from-degrees moment-from-jd jd-from-moment
          fixed-from-jd jd-from-fixed fixed-from-mjd mjd-from-fixed
          range start end in-range? list-range
          egyptian-date fixed-from-egyptian egyptian-from-fixed armenian-date
          fixed-from-armenian armenian-from-fixed gregorian-date
          gregorian-leap-year? fixed-from-gregorian
          gregorian-year-from-fixed gregorian-from-fixed
          gregorian-date-difference day-number days-remaining
          alt-fixed-from-gregorian alt-gregorian-from-fixed
          alt-gregorian-year-from-fixed 
          gregorian-new-year gregorian-year-end
          independence-day kday-on-or-before kday-on-or-after
          kday-nearest kday-after kday-before nth-kday first-kday last-kday
          labor-day memorial-day election-day daylight-saving-start
          daylight-saving-end christmas advent epiphany
          unlucky-fridays-in-range
          iso-date iso-week iso-day iso-year fixed-from-iso iso-from-fixed
          iso-long-year? julian-date bce ce julian-leap-year?
          fixed-from-julian julian-from-fixed roman-date roman-year roman-month
          roman-event roman-count roman-leap ides-of-month
          nones-of-month fixed-from-roman roman-from-fixed year-rome-founded
          julian-year-from-auc-year auc-year-from-julian-year
          julian-in-gregorian eastern-orthodox-christmas coptic-date
          coptic-leap-year? fixed-from-coptic coptic-from-fixed
          ethiopic-date fixed-from-ethiopic ethiopic-from-fixed
          coptic-in-gregorian coptic-christmas orthodox-easter
          alt-orthodox-easter easter pentecost islamic-date
          islamic-leap-year? fixed-from-islamic islamic-from-fixed
          islamic-in-gregorian mawlid-an-nabi bahai-date bahai-major
          bahai-cycle bahai-year bahai-month bahai-day
          fixed-from-bahai bahai-from-fixed bahai-new-year
          feast-of-ridvan hebrew-date hebrew-leap-year?
          last-month-of-hebrew-year hebrew-sabbatical-year?
          last-day-of-hebrew-month hebrew-new-year molad fixed-from-hebrew
          hebrew-from-fixed yom-kippur passover omer purim ta-anit-esther
          tishah-be-av birkath-ha-hama sh-ela yom-ha-zikaron
          hebrew-birthday-in-gregorian yahrzeit-in-gregorian
          possible-hebrew-days
          mayan-long-count-date mayan-haab-date mayan-tzolkin-date
          mayan-baktun mayan-katun mayan-tun mayan-uinal mayan-kin
          mayan-haab-month mayan-haab-day mayan-tzolkin-number
          mayan-tzolkin-name fixed-from-mayan-long-count
          mayan-long-count-from-fixed mayan-haab-from-fixed
          mayan-haab-on-or-before mayan-tzolkin-from-fixed
          mayan-tzolkin-on-or-before mayan-year-bearer-from-fixed
          mayan-calendar-round-on-or-before aztec-xihuitl-date
          aztec-xihuitl-month aztec-xihuitl-day aztec-tonalpohualli-date
          aztec-tonalpohualli-number aztec-tonalpohualli-name
          aztec-xiuhmolpilli-designation aztec-xiuhmolpilli-number
          aztec-xiuhmolpilli-name aztec-correlation aztec-xihuitl-ordinal
          aztec-xihuitl-correlation aztec-xihuitl-from-fixed
          aztec-xihuitl-on-or-before aztec-tonalpohualli-ordinal
          aztec-tonalpohualli-correlation aztec-tonalpohualli-from-fixed
          aztec-tonalpohualli-on-or-before
          aztec-xihuitl-tonalpohualli-on-or-before
          aztec-xiuhmolpilli-from-fixed old-hindu-lunar-date
          old-hindu-lunar-month old-hindu-lunar-leap old-hindu-lunar-day
          old-hindu-lunar-year hindu-solar-date hindu-day-count
          old-hindu-solar-from-fixed fixed-from-old-hindu-solar
          old-hindu-lunar-leap-year? old-hindu-lunar-from-fixed
          fixed-from-old-hindu-lunar jovian-year balinese-date
          bali-luang bali-dwiwara bali-triwara bali-caturwara
          bali-pancawara bali-sadwara bali-saptawara bali-asatawara
          bali-sangawara bali-dasawara bali-day-from-fixed
          bali-luang-from-fixed bali-dwiwara-from-fixed
          bali-triwara-from-fixed bali-caturwara-from-fixed
          bali-pancawara-from-fixed bali-sadwara-from-fixed
          bali-saptawara-from-fixed bali-asatawara-from-fixed
          bali-sangawara-from-fixed bali-dasawara-from-fixed
          bali-pawukon-from-fixed bali-week-from-fixed
          bali-on-or-before positions-in-range
          kajeng-keliwon tumpek hr sec deg mt
          angle location latitude longitude elevation zone direction
          standard-from-universal
          universal-from-standard local-from-universal
          universal-from-local standard-from-local local-from-standard
          midday midnight local-from-apparent apparent-from-local
          dawn dusk sunrise sunset islamic-sunrise
          islamic-sunset jewish-dusk jewish-sabbath-ends
          daytime-temporal-hour nighttime-temporal-hour
          standard-from-sundial jewish-morning-end asr
          universal-from-dynamical dynamical-from-universal
          sidereal-from-moment equation-of-time solar-longitude
          solar-longitude-after sidereal-solar-longitude
          lunar-longitude nth-new-moon new-moon-before new-moon-at-or-after 
          lunar-phase lunar-phase-at-or-before lunar-phase-at-or-after
          topocentric-lunar-altitude lunar-diameter
          lunar-latitude lunar-altitude lunar-distance 
          fixed-from-observational-islamic
          persian-date persian-new-year-on-or-before
          fixed-from-persian persian-from-fixed
          arithmetic-persian-leap-year? fixed-from-arithmetic-persian
          arithmetic-persian-year-from-fixed
          arithmetic-persian-from-fixed naw-ruz french-date
          french-new-year-on-or-before fixed-from-french
          french-from-fixed arithmetic-french-leap-year?
          fixed-from-arithmetic-french arithmetic-french-from-fixed
          chinese-date chinese-cycle chinese-year chinese-month
          chinese-leap chinese-day chinese-location
          chinese-solar-longitude-on-or-after current-major-solar-term
          major-solar-term-on-or-after current-minor-solar-term
          minor-solar-term-on-or-after chinese-new-year-on-or-before
          chinese-new-year chinese-from-fixed fixed-from-chinese
          chinese-name chinese-stem chinese-branch
          chinese-sexagesimal-name chinese-name-difference
          chinese-name-of-year chinese-name-of-month
          chinese-name-of-day chinese-day-name-on-or-before dragon-festival
          qing-ming chinese-age chinese-year-marriage-augury
          japanese-location korean-location korean-year
          vietnamese-location hindu-lunar-date hindu-lunar-month
          hindu-lunar-leap-month hindu-lunar-day hindu-lunar-leap-day
          hindu-lunar-year hindu-lunar-day-at-or-after hindu-solar-from-fixed
          fixed-from-hindu-solar hindu-lunar-from-fixed
          fixed-from-hindu-lunar hindu-sunrise alt-hindu-sunrise
          ayanamsha astro-hindu-sunset hindu-sunset hindu-sundial-time
          astro-hindu-solar-from-fixed fixed-from-astro-hindu-solar
          astro-hindu-lunar-from-fixed fixed-from-astro-hindu-lunar
          hindu-fullmoon-from-fixed fixed-from-hindu-fullmoon
          hindu-lunar-station hindu-solar-longitude-at-or-after
          mesha-samkranti hindu-lunar-holiday diwali shiva rama
          hindu-lunar-new-year karana yoga sacred-wednesdays
          tibetan-from-fixed fixed-from-tibetan losar tibetan-new-year
          future-bahai-new-year-on-or-before fixed-from-future-bahai
          future-bahai-from-fixed phasis-on-or-before phasis-on-or-after
          observational-islamic-from-fixed astronomical-easter
          observational-hebrew-from-fixed fixed-from-observational-hebrew
          observational-hebrew-new-year classical-passover-eve
          ))


;;;; Section: Basic Code

(defconstant true
  ;; TYPE boolean
  ;; Constant representing true.
  t)

(defconstant false
  ;; TYPE boolean
  ;; Constant representing false.
  nil)

(defconstant bogus
  ;; TYPE string
  ;; Used to denote nonexistent dates.
  "bogus")

(defun quotient (m n)
  ;; TYPE (real nonzero-real) -> integer
  ;; Whole part of $m$/$n$.
  (floor m n))

(defun amod (x y)
  ;; TYPE (real real) -> real
  ;; The value of ($x$ mod $y$) with $y$ instead of 0.
  (+ y (mod x (- y))))

(defmacro next (index initial condition)
  ;; TYPE (* integer (integer->boolean)) -> integer
  ;; First integer greater or equal to $initial$ such that
  ;; $condition$ holds.
  `(do ((,index ,initial (1+ ,index)))
       (,condition ,index)))

(defmacro final (index initial condition)
  ;; TYPE (* integer (integer->boolean)) -> integer
  ;; Last integer greater or equal to $initial$ such that
  ;; $condition$ holds.
  `(do ((,index ,initial (1+ ,index)))
       ((not ,condition) (1- ,index))))

(defmacro sum (expression index initial condition)
  ;; TYPE ((integer->real) * integer (integer->boolean))
  ;; TYPE -> real
  ;; Sum $expression$ for $index$ = $initial$ and successive
  ;; integers, as long as $condition$ holds.
  (let* ((temp (gensym)))
    `(do ((,temp 0 (+ ,temp ,expression))
          (,index ,initial (1+ ,index)))
         ((not ,condition) ,temp))))

(defmacro binary-search (l lo h hi x test end)
  ;; TYPE (* real * real * (real->boolean)
  ;; TYPE  ((real real)->boolean)) -> real
  ;; Bisection search for $x$ in [$lo$,$hi$] such that
  ;; $end$ holds.  $test$ determines when to go left.
  (let* ((left (gensym)))
    `(do* ((,x false (/ (+ ,h ,l) 2))
           (,left false ,test)
           (,l ,lo (if ,left ,l ,x))
           (,h ,hi (if ,left ,x ,h)))
          (,end (/ (+ ,h ,l) 2)))))

(defmacro invert-angular (f y a b)
  ;; TYPE (real->angle real real real) -> real 
  ;; Use bisection to find inverse of angular function
  ;; $f$ at $y$ within interval [$a$,$b$].
  (let* ((varepsilon 1/100000)); Desired accuracy
    `(binary-search l ,a u ,b x
                    (< (mod (- (,f x) ,y) 360) (deg 180))
                    (< (- u l) ,varepsilon))))

(defmacro sigma (list body)
  ;; TYPE (list-of-pairs (list-of-reals->real))
  ;; TYPE -> real
  ;; $list$ is of the form ((i1 l1)..(in ln)).
  ;; Sum of $body$ for indices i1..in
  ;; running simultaneously thru lists l1..ln.
  `(apply '+ (mapcar (function (lambda
                                 ,(mapcar 'car list)
                                 ,body))
                     ,@(mapcar 'cadr list))))

(defun poly (x a)
  ;; TYPE (real list-of-reals) -> real
  ;; Sum powers of $x$ with coefficients (from order 0 up)
  ;; in list $a$.
  (if (equal a nil)
      0
    (+ (first a) (* x (poly x (rest a))))))

(defun rd (tee)
  ;; TYPE moment -> moment
  ;; Identity function for fixed dates/moments.  If internal
  ;; timekeeping is shifted, change $epoch$ to be RD date of
  ;; origin of internal count.  $epoch$ should be an integer.
  (let* ((epoch 0))
    (- tee epoch)))

(defconstant sunday
  ;; TYPE day-of-week
  ;; Residue class for Sunday.
  0)

(defconstant monday
  ;; TYPE day-of-week
  ;; Residue class for Monday.
  (+ sunday 1))

(defconstant tuesday
  ;; TYPE day-of-week
  ;; Residue class for Tuesday.
  (+ sunday 2))

(defconstant wednesday
  ;; TYPE day-of-week
  ;; Residue class for Wednesday.
  (+ sunday 3))

(defconstant thursday
  ;; TYPE day-of-week
  ;; Residue class for Thursday.
  (+ sunday 4))

(defconstant friday
  ;; TYPE day-of-week
  ;; Residue class for Friday.
  (+ sunday 5))

(defconstant saturday
  ;; TYPE day-of-week
  ;; Residue class for Saturday.
  (+ sunday 6))

(defun day-of-week-from-fixed (date)
  ;; TYPE fixed-date -> day-of-week
  ;; The residue class of the day of the week of $date$.
  (mod (- date (rd 0) sunday) 7))

(defun standard-month (date)
  ;; TYPE standard-date -> standard-month
  ;; Month field of $date$ = (year month day).
  (second date))

(defun standard-day (date)
  ;; TYPE standard-date -> standard-day
  ;; Day field of $date$ = (year month day).
  (third date))

(defun standard-year (date)
  ;; TYPE standard-date -> standard-year
  ;; Year field of $date$ = (year month day).
  (first date))

(defun time-of-day (hour minute second)
  ;; TYPE (hour minute second) -> clock-time
  (list hour minute second))

(defun hour (clock)
  ;; TYPE clock-time -> hour
  (first clock))

(defun minute (clock)
  ;; TYPE clock-time -> minute
  (second clock))

(defun seconds (clock)
  ;; TYPE clock-time -> second
  (third clock))

(defun fixed-from-moment (tee)
  ;; TYPE moment -> fixed-date
  ;; Fixed-date from moment $tee$.
  (floor tee))

(defun time-from-moment (tee)
  ;; TYPE moment -> time
  ;; Time from moment $tee$.
  (mod tee 1))

(defun clock-from-moment (tee)
  ;; TYPE moment -> clock-time
  ;; Clock time hour:minute:second from moment $tee$.
  (let* ((time (time-from-moment tee))
         (hour (floor (* time 24)))
         (minute (floor (mod (* time 24 60) 60)))
         (second (mod (* time 24 60 60) 60)))
    (time-of-day hour minute second)))

(defun time-from-clock (hms)
  ;; TYPE clock-time -> time
  ;; Time of day from $hms$ = (hour minute second).
  (let* ((h (hour hms))
         (m (minute hms))
         (s (seconds hms)))
    (* 1/24 (+ h (/ (+ m (/ s 60)) 60)))))

(defun degrees-minutes-seconds (d m s)
  ;; TYPE (degree minute real) -> angle
  (list d m s))

(defun angle-from-degrees (alpha)
  ;; TYPE angle -> list-of-reals
  ;; List of degrees-arcminutes-arcseconds from angle
  ;; $alpha$ in degrees.
   (let* ((d (floor alpha))
          (m (floor (* 60 (mod alpha 1))))
          (s (mod (* alpha 60 60) 60)))
     (degrees-minutes-seconds d m s)))

(defconstant jd-epoch
  ;; TYPE moment
  ;; Fixed time of start of the julian day number.
  (rd -1721424.5L0))

(defun moment-from-jd (jd)
  ;; TYPE julian-day-number -> moment
  ;; Moment of julian day number $jd$.
  (+ jd jd-epoch))

(defun jd-from-moment (tee)
  ;; TYPE moment -> julian-day-number
  ;; Julian day number of moment $tee$.
  (- tee jd-epoch))

(defun fixed-from-jd (jd)
  ;; TYPE julian-day-number -> fixed-date
  ;; Fixed date of julian day number $jd$.
  (floor (moment-from-jd jd)))

(defun jd-from-fixed (date)
  ;; TYPE fixed-date -> julian-day-number
  ;; Julian day number of fixed $date$.
  (jd-from-moment date))

(defconstant mjd-epoch
  ;; TYPE fixed-date
  ;; Fixed time of start of the modified julian day number.
  (rd 678576))

(defun fixed-from-mjd (mjd)
  ;; TYPE julian-day-number -> fixed-date
  ;; Fixed date of modified julian day number $mjd$.
  (+ mjd mjd-epoch))

(defun mjd-from-fixed (date)
  ;; TYPE fixed-date -> julian-day-number
  ;; Modified julian day number of fixed $date$.
  (- date mjd-epoch))

(defun interval (t0 t1)
  ;; TYPE (moment moment) -> range
  ;; Closed interval [$t0$,$t1$].
  (list t0 t1))

(defun start (range)
  ;; TYPE range -> moment
  ;; Start $t0$ of $range$=[$t0$,$t1$].
  (first range))

(defun end (range)
  ;; TYPE range -> moment
  ;; End $t1$ of $range$=[$t0$,$t1$].
  (second range))

(defun in-range? (tee range)
  ;; TYPE (moment range) -> boolean
  ;; True if $tee$ is in $range$. 
  (<= (start range) tee (end range)))

(defun list-range (ell range)
  ;; TYPE (list-of-moments range) -> range
  ;; Those moments in list $ell$ that occur in $range$.
  (if (equal ell nil)
      nil
    (let* ((r (list-range (rest ell) range)))
      (if (in-range? (first ell) range)
          (append (list (first ell)) r)
        r))))


;;;; Section: Egyptian/Armenian Calendars

(defun egyptian-date (year month day)
  ;; TYPE (egyptian-year egyptian-month egyptian-day)
  ;; TYPE -> egyptian-date
  (list year month day))

(defconstant egyptian-epoch
  ;; TYPE fixed-date
  ;; Fixed date of start of the Egyptian (Nabonasser)
  ;; calendar.
  ;; JD 1448638 = February 26, 747 BCE (Julian).
  (fixed-from-jd 1448638))

(defun fixed-from-egyptian (e-date)
  ;; TYPE egyptian-date -> fixed-date
  ;; Fixed date of Egyptian date $e-date$.
  (let* ((month (standard-month e-date))
         (day (standard-day e-date))
         (year (standard-year e-date)))
    (+ egyptian-epoch   ; Days before start of calendar
       (* 365 (1- year)); Days in prior years
       (* 30 (1- month)); Days in prior months this year
       day -1)))        ; Days so far this month

(defun egyptian-from-fixed (date)
  ;; TYPE fixed-date -> egyptian-date
  ;; Egyptian equivalent of fixed $date$.
  (let* ((days ; Elapsed days since epoch.
          (- date egyptian-epoch))
         (year ; Year since epoch.
          (1+ (quotient days 365)))
         (month; Calculate the month by division.
          (1+ (quotient (mod days 365)
                        30)))
         (day  ; Calculate the day by subtraction.
          (- days
             (* 365 (1- year))
             (* 30 (1- month))
             -1)))
    (egyptian-date year month day)))

(defun armenian-date (year month day)
  ;; TYPE (armenian-year armenian-month armenian-day)
  ;; TYPE -> armenian-date
  (list year month day))

(defconstant armenian-epoch
  ;; TYPE fixed-date
  ;; Fixed date of start of the Armenian calendar.
  ;; = July 11, 552 CE (Julian).
  (rd 201443))

(defun fixed-from-armenian (a-date)
  ;; TYPE armenian-date -> fixed-date
  ;; Fixed date of Armenian date $a-date$.
  (let* ((month (standard-month a-date))
         (day (standard-day a-date))
         (year (standard-year a-date)))
    (+ armenian-epoch
       (- (fixed-from-egyptian
           (egyptian-date year month day))
          egyptian-epoch))))

(defun armenian-from-fixed (date)
  ;; TYPE fixed-date -> armenian-date
  ;; Armenian equivalent of fixed $date$.
  (egyptian-from-fixed
   (+ date (- egyptian-epoch armenian-epoch))))


;;;; Section: Gregorian Calendar

(defun gregorian-date (year month day)
  ;; TYPE (gregorian-year gregorian-month gregorian-day)
  ;; TYPE -> gregorian-date
  (list year month day))

(defconstant gregorian-epoch
  ;; TYPE fixed-date
  ;; Fixed date of start of the (proleptic) Gregorian
  ;; calendar.
  (rd 1))

(defconstant january
  ;; TYPE standard-month
  ;; January on Julian/Gregorian calendar.
  1)

(defconstant february
  ;; TYPE standard-month
  ;; February on Julian/Gregorian calendar.
  2)

(defconstant march
  ;; TYPE standard-month
  ;; March on Julian/Gregorian calendar.
  3)

(defconstant april
  ;; TYPE standard-month
  ;; April on Julian/Gregorian calendar.
  4)

(defconstant may
  ;; TYPE standard-month
  ;; May on Julian/Gregorian calendar.
  5)

(defconstant june
  ;; TYPE standard-month
  ;; June on Julian/Gregorian calendar.
  6)

(defconstant july
  ;; TYPE standard-month
  ;; July on Julian/Gregorian calendar.
  7)

(defconstant august
  ;; TYPE standard-month
  ;; August on Julian/Gregorian calendar.
  8)

(defconstant september
  ;; TYPE standard-month
  ;; September on Julian/Gregorian calendar.
  9)

(defconstant october
  ;; TYPE standard-month
  ;; October on Julian/Gregorian calendar.
  10)

(defconstant november
  ;; TYPE standard-month
  ;; November on Julian/Gregorian calendar.
  11)

(defconstant december
  ;; TYPE standard-month
  ;; December on Julian/Gregorian calendar.
  12)

(defun gregorian-leap-year? (g-year)
  ;; TYPE gregorian-year -> boolean
  ;; True if $g-year$ is a leap year on the Gregorian
  ;; calendar.
  (and (= (mod g-year 4) 0)
       (not (member (mod g-year 400)
                    (list 100 200 300)))))

(defun fixed-from-gregorian (g-date)
  ;; TYPE gregorian-date -> fixed-date
  ;; Fixed date equivalent to the Gregorian date $g-date$.
  (let* ((month (standard-month g-date))
         (day (standard-day g-date))
         (year (standard-year g-date)))
      (+ (1- gregorian-epoch); Days before start of calendar
         (* 365 (1- year)); Ordinary days since epoch
         (quotient (1- year)
                   4); Julian leap days since epoch...
         (-          ; ...minus century years since epoch...
          (quotient (1- year) 100))
         (quotient   ; ...plus years since epoch divisible...
          (1- year) 400)  ; ...by 400.
         (quotient        ; Days in prior months this year...
          (- (* 367 month) 362); ...assuming 30-day Feb
          12)
         (if (<= month 2) ; Correct for 28- or 29-day Feb
             0
           (if (gregorian-leap-year? year)
               -1
             -2))
         day)))          ; Days so far this month.

(defun gregorian-year-from-fixed (date)
  ;; TYPE fixed-date -> gregorian-year
  ;; Gregorian year corresponding to the fixed $date$.
  (let* ((d0        ; Prior days.
          (- date gregorian-epoch))
         (n400      ; Completed 400-year cycles.
          (quotient d0 146097))
         (d1        ; Prior days not in n400.
          (mod d0 146097))
         (n100      ; 100-year cycles not in n400.
          (quotient d1 36524))
         (d2        ; Prior days not in n400 or n100.
          (mod d1 36524))
         (n4        ; 4-year cycles not in n400 or n100.
          (quotient d2 1461))
         (d3        ; Prior days not in n400, n100, or n4.
          (mod d2 1461))
         (n1        ; Years not in n400, n100, or n4.
          (quotient d3 365))
         (year (+ (* 400 n400)
                  (* 100 n100)
                  (* 4 n4)
                  n1)))
    (if (or (= n100 4) (= n1 4))
        year      ; Date is day 366 in a leap year.
      (1+ year)))); Date is ordinal day (1+ (mod d3 365))
                  ; in (1+ year).

(defun gregorian-new-year (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of January 1 in $g-year$.
  (fixed-from-gregorian
   (gregorian-date g-year january 1)))

(defun gregorian-year-end (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of December 31 in $g-year$.
  (fixed-from-gregorian
   (gregorian-date g-year december 31)))

(defun gregorian-year-range (g-year)
  ;; TYPE gregorian-year -> range
  ;; The range of moments in Gregorian year $g-year$.
  (interval (gregorian-new-year g-year)
            (gregorian-year-end g-year)))

(defun gregorian-from-fixed (date)
  ;; TYPE fixed-date -> gregorian-date
  ;; Gregorian (year month day) corresponding to fixed $date$.
  (let* ((year (gregorian-year-from-fixed date))
         (prior-days; This year
          (- date (gregorian-new-year year)))
         (correction; To simulate a 30-day Feb
          (if (< date (fixed-from-gregorian
                       (gregorian-date year march 1)))
              0
            (if (gregorian-leap-year? year)
                1
              2)))
         (month     ; Assuming a 30-day Feb
          (quotient
           (+ (* 12 (+ prior-days correction)) 373)
           367))
         (day       ; Calculate the day by subtraction.
          (1+ (- date
                 (fixed-from-gregorian
                  (gregorian-date year month 1))))))
    (gregorian-date year month day)))

(defun gregorian-date-difference (g-date1 g-date2)
  ;; TYPE (gregorian-date gregorian-date) -> integer
  ;; Number of days from Gregorian date $g-date1$ until
  ;; $g-date2$.
  (- (fixed-from-gregorian g-date2)
     (fixed-from-gregorian g-date1)))

(defun day-number (g-date)
  ;; TYPE gregorian-date -> positive-integer
  ;; Day number in year of Gregorian date $g-date$.
  (gregorian-date-difference
   (gregorian-year-end (1- (standard-year g-date)))
   g-date))

(defun days-remaining (g-date)
  ;; TYPE gregorian-date -> nonnegative-integer
  ;; Days remaining in year after Gregorian date $g-date$.
  (gregorian-date-difference
   g-date
   (gregorian-year-end (standard-year g-date))))

(defun alt-fixed-from-gregorian (g-date)
  ;; TYPE gregorian-date -> fixed-date
  ;; Alternative calculation of fixed date equivalent to the
  ;; Gregorian date $g-date$.
  (let* ((month (standard-month g-date))
         (day (standard-day g-date))
         (year (standard-year g-date))
         (m (amod (- month 2) 12))
         (y (+ year (quotient (+ month 9) 12))))
      (+ (1- gregorian-epoch)
         -306        ; Days in March...December.
         (* 365 (1- y)); Ordinary days since epoch.
         (quotient (1- y)
                   4); Julian leap days since epoch...
         (-          ; ...minus century years since epoch...
          (quotient (1- y) 100))
         (quotient   ; ...plus years since epoch divisible...
          (1- y) 400); ...by 400.
         (quotient   ; Days in prior months this year.
          (1- (* 3 m))
          5)
         (* 30 (1- m))
         day)))       ; Days so far this month.

(defun alt-gregorian-from-fixed (date)
  ;; TYPE fixed-date -> gregorian-date
  ;; Alternative calculation of Gregorian (year month day)
  ;; corresponding to fixed $date$.
  (let* ((y (gregorian-year-from-fixed
             (+ (1- gregorian-epoch)
                date
                306)))
         (prior-days
          (- date (fixed-from-gregorian
                   (gregorian-date (1- y) march 1))))
         (month
          (amod (+ (quotient
                            (+ (* 5 prior-days) 2)
                            153)
                           3)
                        12))
         (year (- y (quotient (+ month 9) 12)))
         (day
          (1+ (- date
                 (fixed-from-gregorian
                  (gregorian-date year month 1))))))
    (gregorian-date year month day)))

(defun alt-gregorian-year-from-fixed (date)
  ;; TYPE fixed-date -> gregorian-year
  ;; Gregorian year corresponding to the fixed $date$.
  (let* ((approx ; approximate year
          (quotient (- date gregorian-epoch -2)
                    146097/400))
         (start  ; start of next year
          (+ gregorian-epoch
             (* 365 approx)
             (quotient approx 4)
             (- (quotient approx 100))
             (quotient approx 400))))
    (if (< date start)
        approx
      (1+ approx))))

(defun independence-day (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of United States Independence Day in
  ;; Gregorian year $g-yaer$.
  (fixed-from-gregorian (gregorian-date g-year july 4)))

(defun kday-on-or-before (k date)
  ;; TYPE (day-of-week fixed-date) -> fixed-date
  ;; Fixed date of the $k$-day on or before fixed $date$.
  ;; $k$=0 means Sunday, $k$=1 means Monday, and so on.
  (- date (day-of-week-from-fixed (- date k))))

(defun kday-on-or-after (k date)
  ;; TYPE (day-of-week fixed-date) -> fixed-date
  ;; Fixed date of the $k$-day on or after fixed $date$.
  ;; $k$=0 means Sunday, $k$=1 means Monday, and so on.
  (kday-on-or-before k (+ date 6)))

(defun kday-nearest (k date)
  ;; TYPE (day-of-week fixed-date) -> fixed-date
  ;; Fixed date of the $k$-day nearest fixed $date$.  
  ;; $k$=0 means Sunday, $k$=1 means Monday, and so on.
  (kday-on-or-before k (+ date 3)))

(defun kday-after (k date)
  ;; TYPE (day-of-week fixed-date) -> fixed-date
  ;; Fixed date of the $k$-day after fixed $date$.  
  ;; $k$=0 means Sunday, $k$=1 means Monday, and so on.
  (kday-on-or-before k (+ date 7)))

(defun kday-before (k date)
  ;; TYPE (day-of-week fixed-date) -> fixed-date
  ;; Fixed date of the $k$-day before fixed $date$.  
  ;; $k$=0 means Sunday, $k$=1 means Monday, and so on.
  (kday-on-or-before k (- date 1)))

(defun nth-kday (n k g-date)
  ;; TYPE (integer day-of-week gregorian-date) -> fixed-date
  ;; Fixed date of $n$-th $k$-day after Gregorian date
  ;; $g-date$.  If $n$>0, return the $n$-th $k$-day on or
  ;; after $g-date$.  If $n$<0, return the $n$-th $k$-day on
  ;; or before $g-date$.  A $k$-day of 0 means Sunday, 1
  ;; means Monday, and so on.
  (if (> n 0)
      (+ (* 7 n)
         (kday-before k (fixed-from-gregorian g-date)))
    (+ (* 7 n)
       (kday-after k (fixed-from-gregorian g-date)))))

(defun first-kday (k g-date)
  ;; TYPE (day-of-week gregorian-date) -> fixed-date
  ;; Fixed date of first $k$-day on or after Gregorian date
  ;; $g-date$. A $k$-day of 0 means Sunday, 1 means Monday,
  ;; and so on.
  (nth-kday 1 k g-date))

(defun last-kday (k g-date)
  ;; TYPE (day-of-week gregorian-date) -> fixed-date
  ;; Fixed date of last $k$-day on or before Gregorian date
  ;; $g-date$. A $k$-day of 0 means Sunday, 1 means Monday,
  ;; and so on.
  (nth-kday -1 k g-date))

(defun labor-day (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of United States Labor Day in Gregorian
  ;; year $g-year$ (the first Monday in September).
  (first-kday monday (gregorian-date g-year september 1)))

(defun memorial-day (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of United States Memorial Day in Gregorian
  ;; year $g-year$ (the last Monday in May).
  (last-kday monday (gregorian-date g-year may 31)))

(defun election-day (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of United States Election Day in Gregorian
  ;; year $g-year$ (the Tuesday after the first Monday in
  ;; November).
  (first-kday tuesday (gregorian-date g-year november 2)))

(defun daylight-saving-start (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of the start of United States daylight
  ;; saving time in Gregorian year $g-year$ (the second
  ;; Sunday in March).
  (nth-kday 2 sunday (gregorian-date g-year march 1)))

(defun daylight-saving-end (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of the end of United States daylight saving
  ;; time in Gregorian year $g-year$ (the first Sunday in
  ;; November).
  (first-kday sunday (gregorian-date g-year november 1)))

(defun christmas (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of Christmas in Gregorian year $g-year$.
  (fixed-from-gregorian
   (gregorian-date g-year december 25)))

(defun advent (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of Advent in Gregorian year $g-year$
  ;; (the Sunday closest to November 30).
  (kday-nearest sunday
                (fixed-from-gregorian
                 (gregorian-date g-year november 30))))

(defun epiphany (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of Epiphany in U.S. in Gregorian year
  ;; $g-year$ (the first Sunday after January 1).
  (first-kday sunday (gregorian-date g-year january 2)))

(defun unlucky-fridays-in-range (range)
  ;; TYPE range -> list-of-fixed-dates
  ;; List of Fridays within $range$ of dates
  ;; that are day 13 of Gregorian months.
  (let* ((a (start range))
         (b (end range))
         (fri (kday-on-or-after friday a))
         (date (gregorian-from-fixed fri)))
    (if (in-range? fri range)
      (append
       (if (= (standard-day date) 13)
           (list fri)
         nil)
       (unlucky-fridays-in-range
        (interval (1+ fri) b)))
      nil)))


;;;; Section: ISO Calendar

(defun iso-date (year week day)
  ;; TYPE (iso-year iso-week iso-day) -> iso-date
  (list year week day))

(defun iso-week (date)
  ;; TYPE iso-date -> iso-week
  (second date))

(defun iso-day (date)
  ;; TYPE iso-date -> day-of-week
  (third date))

(defun iso-year (date)
  ;; TYPE iso-date -> iso-year
  (first date))

(defun fixed-from-iso (i-date)
  ;; TYPE iso-date -> fixed-date
  ;; Fixed date equivalent to ISO $i-date$.
  (let* ((week (iso-week i-date))
         (day (iso-day i-date))
         (year (iso-year i-date)))
    ;; Add fixed date of Sunday preceding date plus day
    ;; in week.
    (+ (nth-kday
        week sunday
        (gregorian-date (1- year) december 28)) day)))

(defun iso-from-fixed (date)
  ;; TYPE fixed-date -> iso-date
  ;; ISO (year week day) corresponding to the fixed $date$.
  (let* ((approx ; Year may be one too small.
          (gregorian-year-from-fixed (- date 3)))
         (year (if (>= date
                       (fixed-from-iso
                        (iso-date (1+ approx) 1 1)))
                   (1+ approx)
                 approx))
         (week (1+ (quotient
                    (- date
                       (fixed-from-iso (iso-date year 1 1)))
                    7)))
         (day (amod (- date (rd 0)) 7)))
    (iso-date year week day)))

(defun iso-long-year? (i-year)
  ;; TYPE iso-year -> boolean
  ;; True if $i-year$ is a long (53-week) year.
  (let* ((jan1 (day-of-week-from-fixed
                (gregorian-new-year i-year)))
         (dec31 (day-of-week-from-fixed
                 (gregorian-year-end i-year))))
    (or (= jan1 thursday)
        (= dec31 thursday))))


;;;; Section: Julian Calendar

(defun julian-date (year month day)
  ;; TYPE (julian-year julian-month julian-day)
  ;; TYPE -> julian-date
  (list year month day))

(defconstant julian-epoch
  ;; TYPE fixed-date
  ;; Fixed date of start of the Julian calendar.
  (fixed-from-gregorian (gregorian-date 0 december 30)))

(defun bce (n)
  ;; TYPE standard-year -> julian-year
  ;; Negative value to indicate a BCE Julian year.
  (- n))

(defun ce (n)
  ;; TYPE standard-year -> julian-year
  ;; Positive value to indicate a CE Julian year.
  n)

(defun julian-leap-year? (j-year)
  ;; TYPE julian-year -> boolean
  ;; True if $j-year$ is a leap year on the Julian calendar.
  (= (mod j-year 4) (if (> j-year 0) 0 3)))

(defun fixed-from-julian (j-date)
  ;; TYPE julian-date -> fixed-date
  ;; Fixed date equivalent to the Julian date $j-date$.
  (let* ((month (standard-month j-date))
         (day (standard-day j-date))
         (year (standard-year j-date))
         (y (if (< year 0)
                (1+ year) ; No year zero
              year)))
    (+ (1- julian-epoch)  ; Days before start of calendar
       (* 365 (1- y))     ; Ordinary days since epoch.
       (quotient (1- y) 4); Leap days since epoch...
       (quotient          ; Days in prior months this year...
        (- (* 367 month) 362); ...assuming 30-day Feb
        12)
       (if (<= month 2)   ; Correct for 28- or 29-day Feb
           0
         (if (julian-leap-year? year)
             -1
           -2))
       day)))             ; Days so far this month.

(defun julian-from-fixed (date)
  ;; TYPE fixed-date -> julian-date
  ;; Julian (year month day) corresponding to fixed $date$.
  (let* ((approx      ; Nominal year.
          (quotient (+ (* 4 (- date julian-epoch)) 1464)
                    1461))
         (year (if (<= approx 0)
                   (1- approx) ; No year 0.
                 approx))
         (prior-days; This year
          (- date (fixed-from-julian
                   (julian-date year january 1))))
         (correction; To simulate a 30-day Feb
          (if (< date (fixed-from-julian
                       (julian-date year march 1)))
              0
            (if (julian-leap-year? year)
                1
              2)))
         (month     ; Assuming a 30-day Feb
          (quotient
           (+ (* 12 (+ prior-days correction)) 373)
           367))
         (day       ; Calculate the day by subtraction.
          (1+ (- date
                 (fixed-from-julian
                  (julian-date year month 1))))))
    (julian-date year month day)))

(defconstant kalends
  ;; TYPE roman-event
  ;; Class of Kalends.
  1)

(defconstant nones
  ;; TYPE roman-event
  ;; Class of Nones.
  2)

(defconstant ides
  ;; TYPE roman-event
  ;; Class of Ides.
  3)

(defun roman-date (year month event count leap)
  ;; TYPE (roman-year roman-month roman-event roman-count
  ;; TYPE  roman-leap) -> roman-date
  (list year month event count leap))

(defun roman-year (date)
  ;; TYPE roman-date -> roman-year
  (first date))

(defun roman-month (date)
  ;; TYPE roman-date -> roman-month
  (second date))

(defun roman-event (date)
  ;; TYPE roman-date -> roman-event
  (third date))

(defun roman-count (date)
  ;; TYPE roman-date -> roman-count
  (fourth date))

(defun roman-leap (date)
  ;; TYPE roman-date -> roman-leap
  (fifth date))

(defun ides-of-month (month)
  ;; TYPE roman-month -> ides
  ;; Date of Ides in Roman $month$.
  (if (member month (list march may july october))
      15
    13))

(defun nones-of-month (month)
  ;; TYPE roman-month -> nones
  ;; Date of Nones in Roman $month$.
  (- (ides-of-month month) 8))

(defun fixed-from-roman (r-date)
  ;; TYPE roman-date -> fixed-date
  ;; Fixed date for Roman name $r-date$.
  (let* ((leap (roman-leap r-date))
         (count (roman-count r-date))
         (event (roman-event r-date))
         (month (roman-month r-date))
         (year (roman-year r-date)))
    (+ (cond
        ((= event kalends)
         (fixed-from-julian (julian-date year month 1)))
        ((= event nones)
         (fixed-from-julian
          (julian-date year month (nones-of-month month))))
        ((= event ides)
         (fixed-from-julian
          (julian-date year month (ides-of-month month)))))
       (- count)
       (if (and (julian-leap-year? year)
                (= month march)
                (= event kalends)
                (>= 16 count 6))
           0 ; After Ides until leap day
         1) ; Otherwise
       (if leap 
           1 ; Leap day
         0)))) ; Non-leap day

(defun roman-from-fixed (date)
  ;; TYPE fixed-date -> roman-date
  ;; Roman name for fixed $date$.
  (let* ((j-date (julian-from-fixed date))
         (month (standard-month j-date))
         (day (standard-day j-date))
         (year (standard-year j-date))
         (month-prime (amod (1+ month) 12))
         (year-prime (if (/= month-prime 1)
                         year
                       (if (/= year -1)
                           (1+ year)
                         1)))
         (kalends1 (fixed-from-roman
                    (roman-date year-prime month-prime
                                kalends 1 false))))
    (cond
     ((= day 1) (roman-date year month kalends 1 false))
     ((<= day (nones-of-month month))
      (roman-date year month nones
                  (1+ (- (nones-of-month month) day)) false))
     ((<= day (ides-of-month month))
      (roman-date year month ides
                  (1+ (- (ides-of-month month) day)) false))
     ((or (/= month february)
          (not (julian-leap-year? year)))
      ; After the Ides, in a month that is not February of a
      ; leap year
      (roman-date year-prime month-prime kalends
                  (1+ (- kalends1 date)) false))
     ((< day 25)
      ; February of a leap year, before leap day
      (roman-date year march kalends (- 30 day) false))
     (true
      ; February of a leap year, on or after leap day
      (roman-date year march kalends
                  (- 31 day) (= day 25))))))

(defconstant year-rome-founded
  ;; TYPE julian-year
  ;; Year on the Julian calendar of the founding of Rome.
  (bce 753))

(defun julian-year-from-auc-year (year)
  ;; TYPE nonzero-integer -> julian-year
  ;; Julian year equivalent to AUC $year$
  (if (<= 1 year (- year-rome-founded))
      (+ year year-rome-founded -1)
    (+ year year-rome-founded)))

(defun auc-year-from-julian-year (year)
  ;; TYPE julian-year -> nonzero-integer
  ;; Year AUC equivalent to Julian $year$
  (if (<= year-rome-founded year -1)
      (- year year-rome-founded -1)
    (- year year-rome-founded)))

(defun julian-in-gregorian (j-month j-day g-year)
  ;; TYPE (julian-month julian-day gregorian-year)
  ;; TYPE -> list-of-fixed-dates
  ;; List of the fixed dates of Julian month $j-month$, day
  ;; $j-day$ that occur in Gregorian year $g-year$.
  (let* ((jan1 (gregorian-new-year g-year))
         (y (standard-year (julian-from-fixed jan1)))
         (y-prime (if (= y -1)
                     1
                   (1+ y)))
         ;; The possible occurrences in one year are
         (date1 (fixed-from-julian
                 (julian-date y j-month j-day)))
         (date2 (fixed-from-julian
                 (julian-date y-prime j-month j-day))))
    (list-range (list date1 date2) 
                (gregorian-year-range g-year))))

(defun eastern-orthodox-christmas (g-year)
  ;; TYPE gregorian-year -> list-of-fixed-dates
  ;; List of zero or one fixed dates of Eastern Orthodox
  ;; Christmas in Gregorian year $g-year$.
  (julian-in-gregorian december 25 g-year))


;;;; Section: Coptic and Ethiopic Calendars

(defun coptic-date (year month day)
  ;; TYPE (coptic-year coptic-month coptic-day) -> coptic-date
  (list year month day))

(defconstant coptic-epoch
  ;; TYPE fixed-date
  ;; Fixed date of start of the Coptic calendar.
  (fixed-from-julian (julian-date (ce 284) august 29)))

(defun coptic-leap-year? (c-year)
  ;; TYPE coptic-year -> boolean
  ;; True if $c-year$ is a leap year on the Coptic calendar.
  (= (mod c-year 4) 3))

(defun fixed-from-coptic (c-date)
  ;; TYPE coptic-date -> fixed-date
  ;; Fixed date of Coptic date $c-date$.
  (let* ((month (standard-month c-date))
         (day (standard-day c-date))
         (year (standard-year c-date)))
    (+ coptic-epoch -1  ; Days before start of calendar
       (* 365 (1- year)); Ordinary days in prior years
       (quotient year 4); Leap days in prior years
       (* 30 (1- month)); Days in prior months this year
       day)))           ; Days so far this month

(defun coptic-from-fixed (date)
  ;; TYPE fixed-date -> coptic-date
  ;; Coptic equivalent of fixed $date$.
  (let* ((year ; Calculate the year by cycle-of-years formula
          (quotient (+ (* 4 (- date coptic-epoch)) 1463)
                    1461))
         (month; Calculate the month by division.
          (1+ (quotient
               (- date (fixed-from-coptic
                        (coptic-date year 1 1)))
               30)))
         (day  ; Calculate the day by subtraction.
          (- date -1
             (fixed-from-coptic
              (coptic-date year month 1)))))
    (coptic-date year month day)))

(defun ethiopic-date (year month day)
  ;; TYPE (ethiopic-year ethiopic-month ethiopic-day)
  ;; TYPE -> ethiopic-date
  (list year month day))

(defconstant ethiopic-epoch
  ;; TYPE fixed-date
  ;; Fixed date of start of the Ethiopic calendar.
  (fixed-from-julian (julian-date (ce 8) august 29)))

(defun fixed-from-ethiopic (e-date)
  ;; TYPE ethiopic-date -> fixed-date
  ;; Fixed date of Ethiopic date $e-date$.
  (let* ((month (standard-month e-date))
         (day (standard-day e-date))
         (year (standard-year e-date)))
    (+ ethiopic-epoch
       (- (fixed-from-coptic
           (coptic-date year month day))
          coptic-epoch))))

(defun ethiopic-from-fixed (date)
  ;; TYPE fixed-date -> ethiopic-date
  ;; Ethiopic equivalent of fixed $date$.
  (coptic-from-fixed
   (+ date (- coptic-epoch ethiopic-epoch))))

(defun coptic-in-gregorian (c-month c-day g-year)
  ;; TYPE (coptic-month coptic-day gregorian-year)
  ;; TYPE -> list-of-fixed-dates
  ;; List of the fixed dates of Coptic month $c-month$, day
  ;; $c-day$ that occur in Gregorian year $g-year$.
  (let* ((jan1 (gregorian-new-year g-year))
         (y (standard-year (coptic-from-fixed jan1)))
         ;; The possible occurrences in one year are
         (date1 (fixed-from-coptic
                 (coptic-date y c-month c-day)))
         (date2 (fixed-from-coptic
                 (coptic-date (1+ y) c-month c-day))))
    (list-range (list date1 date2) 
                (gregorian-year-range g-year))))

(defun coptic-christmas (g-year)
  ;; TYPE gregorian-year -> list-of-fixed-dates
  ;; List of zero or one fixed dates of Coptic Christmas
  ;; in Gregorian year $g-year$.
  (coptic-in-gregorian 4 29 g-year))


;;;; Section: Ecclesiastical Calendars

(defun orthodox-easter (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of Orthodox Easter in Gregorian year $g-year$.
  (let* ((shifted-epact ; Age of moon for April 5.
          (mod (+ 14 (* 11 (mod g-year 19)))
               30))
         (j-year (if (> g-year 0); Julian year number.
                     g-year
                   (1- g-year)))
         (paschal-moon  ; Day after full moon on
                        ; or after March 21.
          (- (fixed-from-julian (julian-date j-year april 19))
             shifted-epact)))
    ;; Return the Sunday following the Paschal moon.
    (kday-after sunday paschal-moon)))

(defun alt-orthodox-easter (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Alternative calculation of fixed date of Orthodox Easter 
  ;; in Gregorian year $g-year$.
  (let* ((paschal-moon  ; Day after full moon on
                        ; or after March 21.
          (+ (* 354 g-year)
             (* 30 (quotient (+ (* 7 g-year) 8) 19))
             (quotient g-year 4)
             (- (quotient g-year 19))
             -272)))
    ;; Return the Sunday following the Paschal moon.
    (kday-after sunday paschal-moon)))

(defun easter (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of Easter in Gregorian year $g-year$.
  (let* ((century (1+ (quotient g-year 100)))
         (shifted-epact        ; Age of moon for April 5...
          (mod
           (+ 14 (* 11 (mod g-year 19));   ...by Nicaean rule
              (- ;...corrected for the Gregorian century rule
               (quotient (* 3 century) 4))
              (quotient; ...corrected for Metonic
                       ; cycle inaccuracy.
               (+ 5 (* 8 century)) 25))
           30))
         (adjusted-epact       ;  Adjust for 29.5 day month.
          (if (or (= shifted-epact 0)
                  (and (= shifted-epact 1)
                       (< 10 (mod g-year 19))))
              (1+ shifted-epact)
            shifted-epact))
         (paschal-moon; Day after full moon on
                      ; or after March 21.
          (- (fixed-from-gregorian
              (gregorian-date g-year april 19))
             adjusted-epact)))
    ;; Return the Sunday following the Paschal moon.
    (kday-after sunday paschal-moon)))

(defun pentecost (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of Pentecost in Gregorian year $g-year$.
  (+ (easter g-year) 49))


;;;; Section: Islamic Calendar

(defun islamic-date (year month day)
  ;; TYPE (islamic-year islamic-month islamic-day)
  ;; TYPE -> islamic-date
  (list year month day))

(defconstant islamic-epoch
  ;; TYPE fixed-date
  ;; Fixed date of start of the Islamic calendar.
  (fixed-from-julian (julian-date (ce 622) july 16)))

(defun islamic-leap-year? (i-year)
  ;; TYPE islamic-year -> boolean
  ;; True if $i-year$ is an Islamic leap year.
  (< (mod (+ 14 (* 11 i-year)) 30) 11))

(defun fixed-from-islamic (i-date)
  ;; TYPE islamic-date -> fixed-date
  ;; Fixed date equivalent to Islamic date $i-date$.
  (let* ((month (standard-month i-date))
         (day (standard-day i-date))
         (year (standard-year i-date)))
    (+ (1- islamic-epoch)    ; Days before start of calendar
       (* (1- year) 354)     ; Ordinary days since epoch.
       (quotient             ; Leap days since epoch.
        (+ 3 (* 11 year)) 30)
       (* 29 (1- month))     ; Days in prior months this year
       (quotient month 2)
       day)))                ; Days so far this month.

(defun islamic-from-fixed (date)
  ;; TYPE fixed-date -> islamic-date
  ;; Islamic date (year month day) corresponding to fixed
  ;; $date$.
  (let* ((year
          (quotient
           (+ (* 30 (- date islamic-epoch)) 10646)
           10631))
         (prior-days
          (- date (fixed-from-islamic
                   (islamic-date year 1 1))))
         (month
           (quotient
            (+ (* 11 prior-days) 330)
            325))
         (day
          (1+ (- date (fixed-from-islamic
                       (islamic-date year month 1))))))
      (islamic-date year month day)))

(defun islamic-in-gregorian (i-month i-day g-year)
  ;; TYPE (islamic-month islamic-day gregorian-year)
  ;; TYPE -> list-of-fixed-dates
  ;; List of the fixed dates of Islamic month $i-month$, day
  ;; $i-day$ that occur in Gregorian year $g-year$.
  (let* ((jan1 (gregorian-new-year g-year))
         (y (standard-year (islamic-from-fixed jan1)))
         ;; The possible occurrences in one year are
         (date1 (fixed-from-islamic
                 (islamic-date y i-month i-day)))
         (date2 (fixed-from-islamic
                 (islamic-date (1+ y) i-month i-day)))
         (date3 (fixed-from-islamic
                 (islamic-date (+ y 2) i-month i-day))))
    ;; Combine in one list those that occur in current year
    (list-range (list date1 date2 date3) 
                (gregorian-year-range g-year))))

(defun mawlid-an-nabi (g-year)
  ;; TYPE gregorian-year -> list-of-fixed-dates
  ;; List of fixed dates of Mawlid-an-Nabi occurring in
  ;; Gregorian year $g-year$.
  (islamic-in-gregorian 3 12 g-year))


;;;; Section: Hebrew Calendar

(defun hebrew-date (year month day)
  ;; TYPE (hebrew-year hebrew-month hebrew-day) -> hebrew-date
  (list year month day))

(defconstant nisan
  ;; TYPE hebrew-month
  ;; Nisan is month number 1.
  1)

(defconstant iyyar
  ;; TYPE hebrew-month
  ;; Iyyar is month number 2.
  2)

(defconstant sivan
  ;; TYPE hebrew-month
  ;; Sivan is month number 3.
  3)

(defconstant tammuz
  ;; TYPE hebrew-month
  ;; Tammuz is month number 4.
  4)

(defconstant av
  ;; TYPE hebrew-month
  ;; Av is month number 5.
  5)

(defconstant elul
  ;; TYPE hebrew-month
  ;; Elul is month number 6.
  6)

(defconstant tishri
  ;; TYPE hebrew-month
  ;; Tishri is month number 7.
  7)

(defconstant marheshvan
  ;; TYPE hebrew-month
  ;; Marheshvan is month number 8.
  8)

(defconstant kislev
  ;; TYPE hebrew-month
  ;; Kislev is month number 9.
  9)

(defconstant tevet
  ;; TYPE hebrew-month
  ;; Tevet is month number 10.
  10)

(defconstant shevat
  ;; TYPE hebrew-month
  ;; Shevat is month number 11.
  11)

(defconstant adar
  ;; TYPE hebrew-month
  ;; Adar is month number 12.
  12)

(defconstant adarii
  ;; TYPE hebrew-month
  ;; Adar II is month number 13.
  13)

(defconstant hebrew-epoch
  ;; TYPE fixed-date
  ;; Fixed date of start of the Hebrew calendar, that is,
  ;; Tishri 1, 1 AM.
  (fixed-from-julian (julian-date (bce 3761) october 7)))

(defun hebrew-leap-year? (h-year)
  ;; TYPE hebrew-year -> boolean
  ;; True if $h-year$ is a leap year on Hebrew calendar.
  (< (mod (1+ (* 7 h-year)) 19) 7))

(defun last-month-of-hebrew-year (h-year)
  ;; TYPE hebrew-year -> hebrew-month
  ;; Last month of Hebrew year.
  (if (hebrew-leap-year? h-year)
      adarii
    adar))

(defun hebrew-sabbatical-year? (h-year)
  ;; TYPE hebrew-year -> boolean
  ;; True if $h-year$ is a sabbatical year on the Hebrew
  ;; calendar.
  (= (mod h-year 7) 0))

(defun last-day-of-hebrew-month (h-month h-year)
  ;; TYPE (hebrew-month hebrew-year) -> hebrew-day
  ;; Last day of month$h-month$  in Hebrew year $h-year$.
  (if (or (member h-month
                  (list iyyar tammuz elul tevet adarii))
          (and (= h-month adar)
               (not (hebrew-leap-year? h-year)))
          (and (= h-month marheshvan)
               (not (long-marheshvan? h-year)))
          (and (= h-month kislev)
               (short-kislev? h-year)))
      29
    30))

(defun molad (h-month h-year)
  ;; TYPE (hebrew-month hebrew-year) -> moment
  ;; Moment of mean conjunction of $h-month$ in Hebrew
  ;; $h-year$.
  (let* ((y ;; Treat Nisan as start of year.
          (if (< h-month tishri)
              (1+ h-year)
            h-year))
         (months-elapsed
          (+ (- h-month tishri)  ;; Months this year.
             (quotient ;; Months until New Year.
               (- (* 235 y) 234) 
               19))))
    (+ hebrew-epoch
       -876/25920
       (* months-elapsed (+ 29 (hr 12) 793/25920)))))

(defun hebrew-calendar-elapsed-days (h-year)
  ;; TYPE hebrew-year -> integer
  ;; Number of days elapsed from the (Sunday) noon prior
  ;; to the epoch of the Hebrew calendar to the mean
  ;; conjunction (molad) of Tishri of Hebrew year $h-year$,
  ;; or one day later.
  (let* ((months-elapsed  ; Since start of Hebrew calendar.
          (quotient (- (* 235 h-year) 234) 19))
         (parts-elapsed; Fractions of days since prior noon.
          (+ 12084 (* 13753 months-elapsed)))
         (days  ; Whole days since prior noon.
          (+ (* 29 months-elapsed)
             (quotient parts-elapsed 25920)))
      ;; If (* 13753 months-elapsed) causes integers that
      ;; are too large, use instead:
      ;; (parts-elapsed
      ;;  (+ 204 (* 793 (mod months-elapsed 1080))))
      ;; (hours-elapsed
      ;;  (+ 11 (* 12 months-elapsed)
      ;;     (* 793 (quotient months-elapsed 1080))
      ;;     (quotient parts-elapsed 1080)))
      ;; (days
      ;;  (+ (* 29 months-elapsed)
      ;;     (quotient hours-elapsed 24)))
         )
    (if (< (mod (* 3 (1+ days)) 7) 3); Sun, Wed, or Fri
        (+ days 1) ; Delay one day.
      days)))

(defun hebrew-new-year (h-year)
  ;; TYPE hebrew-year -> fixed-date
  ;; Fixed date of Hebrew new year $h-year$.
  (+ hebrew-epoch
     (hebrew-calendar-elapsed-days h-year)
     (hebrew-year-length-correction h-year)))

(defun hebrew-year-length-correction (h-year)
  ;; TYPE hebrew-year -> {0,1,2}
  ;; Delays to start of Hebrew year $h-year$ to keep ordinary
  ;; year in range 353-356 and leap year in range 383-386.
  (let* ((ny0 (hebrew-calendar-elapsed-days (1- h-year)))
         (ny1 (hebrew-calendar-elapsed-days h-year))
         (ny2 (hebrew-calendar-elapsed-days (1+ h-year))))
    (cond
     ((= (- ny2 ny1) 356) ; Next year would be too long.
      2)
     ((= (- ny1 ny0) 382) ; Previous year too short.
      1)
     (t 0))))

(defun days-in-hebrew-year (h-year)
  ;; TYPE hebrew-year -> {353,354,355,383,384,385}
  ;; Number of days in Hebrew year $h-year$.
  (- (hebrew-new-year (1+ h-year))
     (hebrew-new-year h-year)))

(defun long-marheshvan? (h-year)
  ;; TYPE hebrew-year -> boolean
  ;; True if Marheshvan is long in Hebrew year $h-year$.
  (member (days-in-hebrew-year h-year) (list 355 385)))

(defun short-kislev? (h-year)
  ;; TYPE hebrew-year -> boolean
  ;; True if Kislev is short in Hebrew year $h-year$.
  (member (days-in-hebrew-year h-year) (list 353 383)))

(defun fixed-from-hebrew (h-date)
  ;; TYPE hebrew-date -> fixed-date
  ;; Fixed date of Hebrew date $h-date$.
  (let* ((month (standard-month h-date))
         (day (standard-day h-date))
         (year (standard-year h-date)))
    (+ (hebrew-new-year year)
       day -1               ; Days so far this month.
       (if ;; before Tishri
           (< month tishri)
           ;; Then add days in prior months this year before
           ;; and after Nisan.
           (+ (sum (last-day-of-hebrew-month m year)
                   m tishri
                   (<= m (last-month-of-hebrew-year year)))
              (sum (last-day-of-hebrew-month m year)
                   m nisan (< m month)))
         ;; Else add days in prior months this year
         (sum (last-day-of-hebrew-month m year)
              m tishri (< m month))))))

(defun hebrew-from-fixed (date)
  ;; TYPE fixed-date -> hebrew-date
  ;; Hebrew (year month day) corresponding to fixed $date$.
  ;; The fraction can be approximated by 365.25.
  (let* ((approx    ; Approximate year
          (1+
           (quotient (- date hebrew-epoch) 35975351/98496)))
         ;; The value 35975351/98496, the average length of
         ;; a Hebrew year, can be approximated by 365.25
         (year      ; Search forward.
          (final y (1- approx)
                 (<= (hebrew-new-year y) date)))
         (start     ; Starting month for search for month.
          (if (< date (fixed-from-hebrew
                       (hebrew-date year nisan 1)))
              tishri
            nisan))
         (month ; Search forward from either Tishri or Nisan.
          (next m start
                (<= date
                    (fixed-from-hebrew
                     (hebrew-date
                      year
                      m
                      (last-day-of-hebrew-month m year))))))
         (day   ; Calculate the day by subtraction.
          (1+ (- date (fixed-from-hebrew
                       (hebrew-date year month 1))))))
    (hebrew-date year month day)))

(defun yom-kippur (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of Yom Kippur occurring in Gregorian year
  ;; $g-year$.
  (let* ((hebrew-year
          (1+ (- g-year
                 (gregorian-year-from-fixed
                  hebrew-epoch)))))
    (fixed-from-hebrew (hebrew-date hebrew-year tishri 10))))

(defun passover (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of Passover occurring in Gregorian year
  ;; $g-year$.
  (let* ((hebrew-year
          (- g-year
             (gregorian-year-from-fixed hebrew-epoch))))
    (fixed-from-hebrew (hebrew-date hebrew-year nisan 15))))

(defun omer (date)
  ;; TYPE fixed-date -> omer-count
  ;; Number of elapsed weeks and days in the omer at $date$.
  ;; Returns bogus if that date does not fall during the
  ;; omer.
  (let* ((c (- date
               (passover
                (gregorian-year-from-fixed date)))))
    (if (<= 1 c 49)
        (list (quotient c 7) (mod c 7))
      bogus)))

(defun purim (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of Purim occurring in Gregorian year $g-year$.
  (let* ((hebrew-year
          (- g-year
             (gregorian-year-from-fixed hebrew-epoch)))
         (last-month  ; Adar or Adar II
          (last-month-of-hebrew-year hebrew-year)))
    (fixed-from-hebrew
     (hebrew-date hebrew-year last-month 14))))

(defun ta-anit-esther (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of Ta'anit Esther occurring in
  ;; Gregorian year $g-year$.
  (let* ((purim-date (purim g-year)))
    (if ; Purim is on Sunday
        (= (day-of-week-from-fixed purim-date) sunday)
        ;; Then prior Thursday
        (- purim-date 3)
      ;; Else previous day
      (1- purim-date))))

(defun tishah-be-av (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of Tishah be-Av occurring in
  ;; Gregorian year $g-year$.
  (let* ((hebrew-year
          (- g-year
             (gregorian-year-from-fixed hebrew-epoch)))
         (av9
          (fixed-from-hebrew
           (hebrew-date hebrew-year av 9))))
    (if ; Ninth of Av is Saturday
        (= (day-of-week-from-fixed av9) saturday)
        ;; Then the next day
        (1+ av9)
      av9)))

(defun birkath-ha-hama (g-year)
  ;; TYPE gregorian-year -> list-of-fixed-dates
  ;; List of fixed date of Birkath ha-Hama occurring in
  ;; Gregorian year $g-year$, if it occurs.
  (let* ((dates (coptic-in-gregorian 7 30 g-year)))
    (if (and (not (equal dates nil))
             (= (mod (standard-year
                      (coptic-from-fixed (first dates)))
                     28)
                17))
        dates
      nil)))

(defun sh-ela (g-year)
  ;; TYPE gregorian-year -> list-of-fixed-dates
  ;; List of fixed dates of Sh'ela occurring in
  ;; Gregorian year $g-year$.
  (coptic-in-gregorian 3 26 g-year))

(defun yom-ha-zikkaron (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of Yom ha-Zikkaron occurring in Gregorian
  ;; year $g-year$.
  (let* ((hebrew-year
          (- g-year
             (gregorian-year-from-fixed hebrew-epoch)))
         (iyyar4; Ordinarily Iyyar 4
          (fixed-from-hebrew
           (hebrew-date hebrew-year iyyar 4))))
    (cond ((member (day-of-week-from-fixed iyyar4)
                   (list thursday friday))
          ;; If Iyyar 4 is Friday or Saturday, then Wednesday
           (kday-before wednesday iyyar4))
          ;; If it's on Sunday, then Monday
          ((= sunday (day-of-week-from-fixed iyyar4))
           (1+ iyyar4))
          (t iyyar4))))

(defun hebrew-birthday (birthdate h-year)
  ;; TYPE (hebrew-date hebrew-year) -> fixed-date
  ;; Fixed date of the anniversary of Hebrew $birthdate$
  ;; occurring in Hebrew $h-year$.
  (let* ((birth-day (standard-day birthdate))
         (birth-month (standard-month birthdate))
         (birth-year (standard-year birthdate)))
    (if ; It's Adar in a normal Hebrew year or Adar II
        ; in a Hebrew leap year,
        (= birth-month (last-month-of-hebrew-year birth-year))
        ;; Then use the same day in last month of Hebrew year.
      (fixed-from-hebrew
       (hebrew-date h-year (last-month-of-hebrew-year h-year)
                    birth-day))
      ;; Else use the normal anniversary of the birth date,
      ;; or the corresponding day in years without that date
      (+ (fixed-from-hebrew
          (hebrew-date h-year birth-month 1))
         birth-day -1))))

(defun hebrew-birthday-in-gregorian (birthdate g-year)
  ;; TYPE (hebrew-date gregorian-year)
  ;; TYPE -> list-of-fixed-dates
  ;; List of the fixed dates of Hebrew $birthday$
  ;; that occur in Gregorian $g-year$.
  (let* ((jan1 (gregorian-new-year g-year))
         (y (standard-year (hebrew-from-fixed jan1)))
         ;; The possible occurrences in one year are
         (date1 (hebrew-birthday birthdate y))
         (date2 (hebrew-birthday birthdate (1+ y))))
    ;; Combine in one list those that occur in current year.
    (list-range (list date1 date2) 
                (gregorian-year-range g-year))))

(defun yahrzeit (death-date h-year)
  ;; TYPE (hebrew-date hebrew-year) -> fixed-date
  ;; Fixed date of the anniversary of Hebrew $death-date$
  ;; occurring in Hebrew $h-year$.
  (let* ((death-day (standard-day death-date))
         (death-month (standard-month death-date))
         (death-year (standard-year death-date)))
    (cond
     ;; If it's Marheshvan 30 it depends on the first
     ;; anniversary; if that was not Marheshvan 30, use
     ;; the day before Kislev 1.
     ((and (= death-month marheshvan)
           (= death-day 30)
           (not (long-marheshvan? (1+ death-year))))
      (1- (fixed-from-hebrew
           (hebrew-date h-year kislev 1))))
     ;; If it's Kislev 30 it depends on the first
     ;; anniversary; if that was not Kislev 30, use
     ;; the day before Tevet 1.
     ((and (= death-month kislev)
           (= death-day 30)
           (short-kislev? (1+ death-year)))
      (1- (fixed-from-hebrew
           (hebrew-date h-year tevet 1))))
     ;; If it's Adar II, use the same day in last
     ;; month of Hebrew year (Adar or Adar II).
     ((= death-month adarii)
      (fixed-from-hebrew
       (hebrew-date
             h-year (last-month-of-hebrew-year h-year)
             death-day)))
     ;; If it's the 30th in Adar I and Hebrew year is not a
     ;; Hebrew leap year (so Adar has only 29 days), use the
     ;; last day in Shevat.
     ((and (= death-day 30)
           (= death-month adar)
           (not (hebrew-leap-year? h-year)))
      (fixed-from-hebrew (hebrew-date h-year shevat 30)))
     ;; In all other cases, use the normal anniversary of
     ;; the date of death.
     (t (+ (fixed-from-hebrew
            (hebrew-date h-year death-month 1))
           death-day -1)))))

(defun yahrzeit-in-gregorian (death-date g-year)
  ;; TYPE (hebrew-date gregorian-year)
  ;; TYPE -> list-of-fixed-dates
  ;; List of the fixed dates of $death-date$ (yahrzeit)
  ;; that occur in Gregorian year $g-year$.
  (let* ((jan1 (gregorian-new-year g-year))
         (y (standard-year (hebrew-from-fixed jan1)))
         ;; The possible occurrences in one year are
         (date1 (yahrzeit death-date y))
         (date2 (yahrzeit death-date (1+ y))))
    ;; Combine in one list those that occur in current year
    (list-range (list date1 date2) 
                (gregorian-year-range g-year))))

(defun shift-days (l cap-Delta)
  ;; TYPE (list-of-weekdays integer) -> list-of-weekdays
  ;; Shift each weekday on list $l$ by $cap-Delta$ days
  (if (equal l nil)
      nil
    (append (list (day-of-week-from-fixed
                   (+ (first l) cap-Delta)))
            (shift-days (rest l) cap-Delta))))

(defun possible-hebrew-days (h-month h-day)
;; TYPE (hebrew-month hebrew-day) -> list-of-weekdays
;; Possible days of week
  (let* ((h-date0 (hebrew-date 5 nisan 1))
         ;; leap year with full pattern
         (h-year (if (> h-month elul) 6 5))
         (h-date (hebrew-date h-year h-month h-day))
         (n (- (fixed-from-hebrew h-date)
               (fixed-from-hebrew h-date0)))
         (tue-thu-sat (list tuesday thursday saturday))
         (sun-wed-fri
          (cond
           ((and (= h-day 30)
                 (member h-month (list marheshvan kislev)))
            nil)
           ((= h-month kislev)
            (list sunday wednesday friday))
           (t (list sunday))))
         (mon (if (member h-month
                          (list kislev tevet shevat adar))
                  (list monday) 
                nil)))
    (shift-days (append tue-thu-sat sun-wed-fri mon) n)))


;;;; Section: Mayan Calendars

(defun mayan-long-count-date (baktun katun tun uinal kin)
  ;; TYPE (mayan-baktun mayan-katun mayan-tun mayan-uinal
  ;; TYPE  mayan-kin) -> mayan-long-count-date
  (list baktun katun tun uinal kin))

(defun mayan-haab-date (month day)
  ;; TYPE (mayan-haab-month mayan-haab-day) -> mayan-haab-date
  (list month day))

(defun mayan-tzolkin-date (number name)
  ;; TYPE (mayan-tzolkin-number mayan-tzolkin-name)
  ;; TYPE -> mayan-tzolkin-date
  (list number name))

(defun mayan-baktun (date)
  ;; TYPE mayan-long-count-date -> mayan-baktun
  (first date))

(defun mayan-katun (date)
  ;; TYPE mayan-long-count-date -> mayan-katun
  (second date))

(defun mayan-tun (date)
  ;; TYPE mayan-long-count-date -> mayan-tun
  (third date))

(defun mayan-uinal (date)
  ;; TYPE mayan-long-count-date -> mayan-uinal
  (fourth date))

(defun mayan-kin (date)
  ;; TYPE mayan-long-count-date -> mayan-kin
  (fifth date))

(defun mayan-haab-month (date)
  ;; TYPE mayan-haab-date -> mayan-haab-month
  (first date))

(defun mayan-haab-day (date)
  ;; TYPE mayan-haab-date -> mayan-haab-day
  (second date))

(defun mayan-tzolkin-number (date)
  ;; TYPE mayan-tzolkin-date -> mayan-tzolkin-number
  (first date))

(defun mayan-tzolkin-name (date)
  ;; TYPE mayan-tzolkin-date -> mayan-tzolkin-name
  (second date))

(defconstant mayan-epoch
  ;; TYPE fixed-date
  ;; Fixed date of start of the Mayan calendar, according
  ;; to the Goodman-Martinez-Thompson correlation.
  ;; That is, August 11, -3113.
  (fixed-from-jd 584283))

(defun fixed-from-mayan-long-count (count)
  ;; TYPE mayan-long-count-date -> fixed-date
  ;; Fixed date corresponding to the Mayan long $count$,
  ;; which is a list (baktun katun tun uinal kin).
  (let* ((baktun (mayan-baktun count))
         (katun (mayan-katun count))
         (tun (mayan-tun count))
         (uinal (mayan-uinal count))
         (kin (mayan-kin count)))
    (+ mayan-epoch      ; Fixed date at Mayan 0.0.0.0.0
       (* baktun 144000); Baktun.
       (* katun 7200)   ; Katun.
       (* tun 360)      ; Tun.
       (* uinal 20)     ; Uinal.
       kin)))           ; Kin (days).

(defun mayan-long-count-from-fixed (date)
  ;; TYPE fixed-date -> mayan-long-count-date
  ;; Mayan long count date of fixed $date$.
  (let* ((long-count (- date mayan-epoch))
         (baktun (quotient long-count 144000))
         (day-of-baktun (mod long-count 144000))
         (katun (quotient day-of-baktun 7200))
         (day-of-katun (mod day-of-baktun 7200))
         (tun (quotient day-of-katun 360))
         (day-of-tun (mod day-of-katun 360))
         (uinal (quotient day-of-tun 20))
         (kin (mod day-of-tun 20)))
    (mayan-long-count-date baktun katun tun uinal kin)))

(defun mayan-haab-ordinal (h-date)
  ;; TYPE mayan-haab-date -> nonnegative-integer
  ;; Number of days into cycle of Mayan haab date $h-date$.
  (let* ((day (mayan-haab-day h-date))
         (month (mayan-haab-month h-date)))
    (+ (* (1- month) 20) day)))

(defconstant mayan-haab-epoch
  ;; TYPE fixed-date
  ;; Fixed date of start of haab cycle.
  (- mayan-epoch
     (mayan-haab-ordinal (mayan-haab-date 18 8))))

(defun mayan-haab-from-fixed (date)
  ;; TYPE fixed-date -> mayan-haab-date
  ;; Mayan haab date of fixed $date$.
  (let* ((count
          (mod (- date mayan-haab-epoch) 365))
         (day (mod count 20))
         (month (1+ (quotient count 20))))
    (mayan-haab-date month day)))

(defun mayan-haab-on-or-before (haab date)
  ;; TYPE (mayan-haab-date fixed-date) -> fixed-date
  ;; Fixed date of latest date on or before fixed $date$
  ;; that is Mayan haab date $haab$.
  (- date
     (mod (- date mayan-haab-epoch
             (mayan-haab-ordinal haab))
          365)))

(defun mayan-tzolkin-ordinal (t-date)
  ;; TYPE mayan-tzolkin-date -> nonnegative-integer
  ;; Number of days into Mayan tzolkin cycle of $t-date$.
  (let* ((number (mayan-tzolkin-number t-date))
         (name (mayan-tzolkin-name t-date)))
    (mod (+ number -1
            (* 39 (- number name)))
         260)))

(defconstant mayan-tzolkin-epoch
  ;; TYPE fixed-date
  ;; Start of tzolkin date cycle.
  (- mayan-epoch
     (mayan-tzolkin-ordinal (mayan-tzolkin-date 4 20))))

(defun mayan-tzolkin-from-fixed (date)
  ;; TYPE fixed-date -> mayan-tzolkin-date
  ;; Mayan tzolkin date of fixed $date$.
  (let* ((count (- date mayan-tzolkin-epoch -1))
         (number (amod count 13))
         (name (amod count 20)))
    (mayan-tzolkin-date number name)))

(defun mayan-tzolkin-on-or-before (tzolkin date)
  ;; TYPE (mayan-tzolkin-date fixed-date) -> fixed-date
  ;; Fixed date of latest date on or before fixed $date$
  ;; that is Mayan tzolkin date $tzolkin$.
  (- date 
     (mod (- date 
             mayan-tzolkin-epoch
             (mayan-tzolkin-ordinal tzolkin))
          260)))

(defun mayan-year-bearer-from-fixed (date)
  ;; TYPE fixed-date -> mayan-tzolkin-name
  ;; Year bearer of year containing fixed $date$.
  ;; Returns bogus for uayeb.
  (let* ((x (mayan-haab-on-or-before
             (mayan-haab-date 1 0)
             (+ date 364))))
    (if (= (mayan-haab-month (mayan-haab-from-fixed date))
           19)
        bogus
      (mayan-tzolkin-name (mayan-tzolkin-from-fixed x)))))

(defun mayan-calendar-round-on-or-before (haab tzolkin date)
  ;; TYPE (mayan-haab-date mayan-tzolkin-date fixed-date)
  ;; TYPE -> fixed-date
  ;; Fixed date of latest date on or before $date$, that is
  ;; Mayan haab date $haab$ and tzolkin date $tzolkin$.
  ;; Returns bogus for impossible combinations.
  (let* ((haab-count
          (+ (mayan-haab-ordinal haab) mayan-haab-epoch))
         (tzolkin-count
          (+ (mayan-tzolkin-ordinal tzolkin) 
             mayan-tzolkin-epoch))
         (diff (- tzolkin-count haab-count)))
    (if (= (mod diff 5) 0)
        (- date
           (mod (- date haab-count (* 365 diff))
                18980))
      bogus)));  haab-tzolkin combination is impossible.

(defun aztec-xihuitl-date (month day)
  ;; TYPE (aztec-xihuitl-month aztec-xihuitl-day) ->
  ;; TYPE aztec-xihuitl-date
  (list month day))

(defun aztec-xihuitl-month (date)
  ;; TYPE aztec-xihuitl-date -> aztec-xihuitl-month
  (first date))

(defun aztec-xihuitl-day (date)
  ;; TYPE aztec-xihuitl-date -> aztec-xihuitl-day
  (second date))

(defun aztec-tonalpohualli-date (number name)
  ;; TYPE (aztec-tonalpohualli-number aztec-tonalpohualli-name)
  ;; TYPE -> aztec-tonalpohualli-date
  (list number name))

(defun aztec-tonalpohualli-number (date)
  ;; TYPE aztec-tonalpohualli-date ->
  ;; aztec-tonalpohualli-number
  (first date))

(defun aztec-tonalpohualli-name (date)
  ;; TYPE aztec-tonalpohualli-date -> aztec-tonalpohualli-name
  (second date))

(defun aztec-xiuhmolpilli-designation (number name)
  ;; TYPE (aztec-xiuhmolpilli-number aztec-xiuhmolpilli-name)
  ;; TYPE -> aztec-xiuhmolpilli-designation
  (list number name))

(defun aztec-xiuhmolpilli-number (date)
  ;; TYPE aztec-xiuhmolpilli-designation
  ;; TYPE -> aztec-xiuhmolpilli-number
  (first date))

(defun aztec-xiuhmolpilli-name (date)
  ;; TYPE aztec-xiuhmolpilli-designation
  ;; TYPE -> aztec-xiuhmolpilli-name
  (second date))

(defconstant aztec-correlation
  ;; TYPE fixed-date
  ;; Known date of Aztec cycles (Caso's correlation)
  (fixed-from-julian (julian-date 1521 August 13)))

(defun aztec-xihuitl-ordinal (x-date)
  ;; TYPE aztec-xihuitl-date -> nonnegative-integer
  ;; Number of elapsed days into cycle of Aztec xihuitl
  ;; $x-date$.
  (let* ((day (aztec-xihuitl-day x-date))
         (month (aztec-xihuitl-month x-date)))
    (+ (* (1- month) 20) (1- day))))

(defconstant aztec-xihuitl-correlation
  ;; TYPE fixed-date
  ;; Start of a xihuitl cycle.
  (- aztec-correlation
     (aztec-xihuitl-ordinal (aztec-xihuitl-date 11 2))))

(defun aztec-xihuitl-from-fixed (date)
  ;; TYPE fixed-date -> aztec-xihuitl-date
  ;; Aztec xihuitl date of fixed $date$.
  (let* ((count (mod (- date aztec-xihuitl-correlation) 365))
         (day (1+ (mod count 20)))
         (month (1+ (quotient count 20))))
    (aztec-xihuitl-date month day)))

(defun aztec-xihuitl-on-or-before (xihuitl date)
  ;; TYPE (aztec-xihuitl-date fixed-date) -> fixed-date
  ;; Fixed date of latest date on or before fixed $date$
  ;; that is Aztec xihuitl date $xihuitl$.
  (- date
     (mod (- date aztec-xihuitl-correlation
             (aztec-xihuitl-ordinal xihuitl))
          365)))

(defun aztec-tonalpohualli-ordinal (t-date)
  ;; TYPE aztec-tonalpohualli-date -> nonnegative-integer
  ;; Number of days into Aztec tonalpohualli cycle of $t-date$.
  (let* ((number (aztec-tonalpohualli-number t-date))
         (name (aztec-tonalpohualli-name t-date)))
    (mod (+ number -1
            (* 39 (- number name)))
         260)))

(defconstant aztec-tonalpohualli-correlation
  ;; TYPE fixed-date
  ;; Start of a tonalpohualli date cycle.
  (- aztec-correlation
     (aztec-tonalpohualli-ordinal
      (aztec-tonalpohualli-date 1 5))))

(defun aztec-tonalpohualli-from-fixed (date)
  ;; TYPE fixed-date -> aztec-tonalpohualli-date
  ;; Aztec tonalpohualli date of fixed $date$.
  (let* ((count (- date aztec-tonalpohualli-correlation -1))
         (number (amod count 13))
         (name (amod count 20)))
    (aztec-tonalpohualli-date number name)))

(defun aztec-tonalpohualli-on-or-before (tonalpohualli date)
  ;; TYPE (aztec-tonalpohualli-date fixed-date) -> fixed-date
  ;; Fixed date of latest date on or before fixed $date$
  ;; that is Aztec tonalpohualli date $tonalpohualli$.
  (- date 
     (mod (- date 
             aztec-tonalpohualli-correlation
             (aztec-tonalpohualli-ordinal tonalpohualli))
          260)))

(defun aztec-xihuitl-tonalpohualli-on-or-before
  (xihuitl tonalpohualli date)
  ;; TYPE (aztec-xihuitl-date aztec-tonalpohualli-date
  ;; TYPE fixed-date) -> fixed-date
  ;; Fixed date of latest xihuitl-tonalpohualli combination
  ;; on or before $date$.  That is the date on or before
  ;; $date$ that is Aztec xihuitl date $xihuitl$ and
  ;; tonalpohualli date $tonalpohualli$.
  ;; Returns bogus for impossible combinations.
  (let* ((xihuitl-count
          (+ (aztec-xihuitl-ordinal xihuitl)
             aztec-xihuitl-correlation))
         (tonalpohualli-count
          (+ (aztec-tonalpohualli-ordinal tonalpohualli) 
             aztec-tonalpohualli-correlation))
         (diff (- tonalpohualli-count xihuitl-count)))
    (if (= (mod diff 5) 0)
        (- date
           (mod (- date xihuitl-count (* 365 diff))
                18980))
      bogus)));  xihuitl-tonalpohualli combination is
              ;  impossible.

(defun aztec-xiuhmolpilli-from-fixed (date)
  ;; TYPE fixed-date -> aztec-xiuhmolpilli-designation
  ;; Designation of year containing fixed $date$.
  ;; Returns bogus for nemontemi.
  (let* ((x (aztec-xihuitl-on-or-before
             (aztec-xihuitl-date 18 20)
             (+ date 364)))
         (month (aztec-xihuitl-month
                 (aztec-xihuitl-from-fixed date))))
    (if (= month 19)
        bogus
      (aztec-tonalpohualli-from-fixed x))))


;;;; Section: Old Hindu Calendars

(defun old-hindu-lunar-date (year month leap day)
  ;; TYPE (old-hindu-lunar-year old-hindu-lunar-month
  ;; TYPE  old-hindu-lunar-leap old-hindu-lunar-day)
  ;; TYPE -> old-hindu-lunar-date
  (list year month leap day))

(defun old-hindu-lunar-month (date)
  ;; TYPE old-hindu-lunar-date -> old-hindu-lunar-month
  (second date))

(defun old-hindu-lunar-leap (date)
  ;; TYPE old-hindu-lunar-date -> old-hindu-lunar-leap
  (third date))

(defun old-hindu-lunar-day (date)
  ;; TYPE old-hindu-lunar-date -> old-hindu-lunar-day
  (fourth date))

(defun old-hindu-lunar-year (date)
  ;; TYPE old-hindu-lunar-date -> old-hindu-lunar-year
  (first date))

(defun hindu-solar-date (year month day)
  ;; TYPE (hindu-solar-year hindu-solar-month hindu-solar-day)
  ;; TYPE -> hindu-solar-date
  (list year month day))

(defconstant hindu-epoch
  ;; TYPE fixed-date
  ;; Fixed date of start of the Hindu calendar (Kali Yuga).
  (fixed-from-julian (julian-date (bce 3102) february 18)))

(defun hindu-day-count (date)
  ;; TYPE fixed-date -> integer
  ;; Elapsed days (Ahargana) to $date$ since Hindu epoch (KY).
  (- date hindu-epoch))

(defconstant arya-solar-year
  ;; TYPE rational
  ;; Length of Old Hindu solar year.
  1577917500/4320000)

(defconstant arya-solar-month
  ;; TYPE rational
  ;; Length of Old Hindu solar month.
  (/ arya-solar-year 12))

(defun old-hindu-solar-from-fixed (date)
  ;; TYPE fixed-date -> hindu-solar-date
  ;; Old Hindu solar date equivalent to fixed $date$.
  (let* ((sun ; Sunrise on Hindu date.
          (+ (hindu-day-count date) (hr 6)))
         (year    ; Elapsed years.
          (quotient sun arya-solar-year))
         (month (1+ (mod (quotient sun arya-solar-month)
                         12)))
         (day (1+ (floor (mod sun arya-solar-month)))))
    (hindu-solar-date year month day)))

(defun fixed-from-old-hindu-solar (s-date)
  ;; TYPE hindu-solar-date -> fixed-date
  ;; Fixed date corresponding to Old Hindu solar date $s-date$.
  (let* ((month (standard-month s-date))
         (day (standard-day s-date))
         (year (standard-year s-date)))
    (ceiling
     (+ hindu-epoch ; Since start of era.
        (* year arya-solar-year) ; Days in elapsed years
        (* (1- month) arya-solar-month) ; ...in months.
        day (hr -30))))) ; Midnight of day.

(defconstant arya-lunar-month
  ;; TYPE rational
  ;; Length of Old Hindu lunar month.
  1577917500/53433336)

(defconstant arya-lunar-day
  ;; TYPE rational
  ;; Length of Old Hindu lunar day.
  (/ arya-lunar-month 30))

(defun old-hindu-lunar-leap-year? (l-year)
  ;; TYPE old-hindu-lunar-year -> boolean
  ;; True if $l-year$ is a leap year on the
  ;; old Hindu calendar.
  (>= (mod (- (* l-year arya-solar-year)
                 arya-solar-month)
           arya-lunar-month)
      23902504679/1282400064))

(defun old-hindu-lunar-from-fixed (date)
  ;; TYPE fixed-date -> old-hindu-lunar-date
  ;; Old Hindu lunar date equivalent to fixed $date$.
  (let* ((sun ; Sunrise on Hindu date.
          (+ (hindu-day-count date) (hr 6)))
         (new-moon ; Beginning of lunar month.
          (- sun (mod sun arya-lunar-month)))
         (leap ; If lunar contained in solar.
          (and (>= (- arya-solar-month arya-lunar-month)
                   (mod new-moon arya-solar-month))
               (> (mod new-moon arya-solar-month) 0)))
         (month ; Next solar month's name.
          (1+ (mod (ceiling (/ new-moon
                               arya-solar-month))
                   12)))
         (day ; Lunar days since beginning of lunar month.
          (1+ (mod (quotient sun arya-lunar-day) 30)))
         (year ; Solar year at end of lunar month(s).
          (1- (ceiling (/ (+ new-moon arya-solar-month)
                          arya-solar-year)))))
    (old-hindu-lunar-date year month leap day)))

(defun fixed-from-old-hindu-lunar (l-date)
  ;; TYPE old-hindu-lunar-date -> fixed-date
  ;; Fixed date corresponding to Old Hindu lunar date
  ;; $l-date$.
  (let* ((year (old-hindu-lunar-year l-date))
         (month (old-hindu-lunar-month l-date))
         (leap (old-hindu-lunar-leap l-date))
         (day (old-hindu-lunar-day l-date))
         (mina ; One solar month before solar new year.
          (* (1- (* 12 year)) arya-solar-month))
         (lunar-new-year ; New moon after mina.
          (* arya-lunar-month
             (1+ (quotient mina arya-lunar-month)))))
    (ceiling
     (+ hindu-epoch
        lunar-new-year
        (* arya-lunar-month
           (if ; If there was a leap month this year.
               (and (not leap)
                    (<= (ceiling (/ (- lunar-new-year mina)
                                    (- arya-solar-month
                                       arya-lunar-month)))
                        month))
                month
             (1- month)))
        (* (1- day) arya-lunar-day) ; Lunar days.
        (hr -6))))) ; Subtract 1 if phase begins before
                     ; sunrise.

(defconstant arya-jovian-period
  ;; TYPE rational
  ;; Number of days in one revolution of Jupiter around the
  ;; Sun.
  1577917500/364224)

(defun jovian-year (date)
  ;; TYPE fixed-date -> {1-60}
  ;; Year of Jupiter cycle at fixed $date$.
 (amod (+ 27 (quotient (hindu-day-count date)
                               (/ arya-jovian-period 12)))
               60))


;;;; Section: Balinese Calendar

(defun balinese-date (b1 b2 b3 b4 b5 b6 b7 b8 b9 b0)
  ;; TYPE (boolean 1-2 1-3 1-4 1-5 1-6 1-7 1-8 1-9 0-9)
  ;; TYPE -> balinese-date
  (list b1 b2 b3 b4 b5 b6 b7 b8 b9 b0))

(defun bali-luang (b-date)
  ;; TYPE balinese-date -> boolean
  (first b-date))

(defun bali-dwiwara (b-date)
  ;; TYPE balinese-date -> 1-2
  (second b-date))

(defun bali-triwara (b-date)
  ;; TYPE balinese-date -> 1-3
  (third b-date))

(defun bali-caturwara (b-date)
  ;; TYPE balinese-date -> 1-4
  (fourth b-date))

(defun bali-pancawara (b-date)
  ;; TYPE balinese-date -> 1-5
  (fifth b-date))

(defun bali-sadwara (b-date)
  ;; TYPE balinese-date -> 1-6
  (sixth b-date))

(defun bali-saptawara (b-date)
  ;; TYPE balinese-date -> 1-7
  (seventh b-date))

(defun bali-asatawara (b-date)
  ;; TYPE balinese-date -> 1-8
  (eighth b-date))

(defun bali-sangawara (b-date)
  ;; TYPE balinese-date -> 1-9
  (ninth b-date))

(defun bali-dasawara (b-date)
  ;; TYPE balinese-date -> 0-9
  (tenth b-date))

(defconstant bali-epoch
  ;; TYPE fixed-date
  ;; Fixed date of start of a Balinese Pawukon cycle.
  (fixed-from-jd 146))

(defun bali-day-from-fixed (date)
  ;; TYPE fixed-date -> 0-209
  ;; Position of $date$ in 210-day Pawukon cycle.
  (mod (- date bali-epoch) 210))

(defun bali-luang-from-fixed (date)
  ;; TYPE fixed-date -> boolean
  ;; Membership of $date$ in "1-day" Balinese cycle.
  (evenp (bali-dasawara-from-fixed date)))

(defun bali-dwiwara-from-fixed (date)
  ;; TYPE fixed-date -> 1-2
  ;; Position of $date$ in 2-day Balinese cycle.
  (amod (bali-dasawara-from-fixed date) 2))

(defun bali-triwara-from-fixed (date)
  ;; TYPE fixed-date -> 1-3
  ;; Position of $date$ in 3-day Balinese cycle.
  (1+ (mod (bali-day-from-fixed date) 3)))

(defun bali-caturwara-from-fixed (date)
  ;; TYPE fixed-date -> 1-4
  ;; Position of $date$ in 4-day Balinese cycle.
  (amod (bali-asatawara-from-fixed date) 4))

(defun bali-pancawara-from-fixed (date)
  ;; TYPE fixed-date -> 1-5
  ;; Position of $date$ in 5-day Balinese cycle.
  (amod (+ (bali-day-from-fixed date) 2) 5))

(defun bali-sadwara-from-fixed (date)
  ;; TYPE fixed-date -> 1-6
  ;; Position of $date$ in 6-day Balinese cycle.
  (1+ (mod (bali-day-from-fixed date) 6)))

(defun bali-saptawara-from-fixed (date)
  ;; TYPE fixed-date -> 1-7
  ;; Position of $date$ in Balinese week.
  (1+ (mod (bali-day-from-fixed date) 7)))

(defun bali-asatawara-from-fixed (date)
  ;; TYPE fixed-date -> 1-8
  ;; Position of $date$ in 8-day Balinese cycle.
  (let* ((day (bali-day-from-fixed date)))
    (1+ (mod
         (max 6
              (+ 4 (mod (- day 70)
                        210)))
         8))))

(defun bali-sangawara-from-fixed (date)
  ;; TYPE fixed-date -> 1-9
  ;; Position of $date$ in 9-day Balinese cycle.
  (1+ (mod (max 0
                (- (bali-day-from-fixed date) 3))
           9)))

(defun bali-dasawara-from-fixed (date)
  ;; TYPE fixed-date -> 0-9
  ;; Position of $date$ in 10-day Balinese cycle.
  (let* ((i ; Position in 5-day cycle.
          (1- (bali-pancawara-from-fixed date)))
         (j ; Weekday.
          (1- (bali-saptawara-from-fixed date))))
    (mod (+ 1 (nth i (list 5 9 7 4 8))
            (nth j (list 5 4 3 7 8 6 9)))
         10)))

(defun bali-pawukon-from-fixed (date)
  ;; TYPE fixed-date -> balinese-date
  ;; Positions of $date$ in ten cycles of Balinese Pawukon
  ;; calendar.
  (balinese-date (bali-luang-from-fixed date)
                 (bali-dwiwara-from-fixed date)
                 (bali-triwara-from-fixed date)
                 (bali-caturwara-from-fixed date)
                 (bali-pancawara-from-fixed date)
                 (bali-sadwara-from-fixed date)
                 (bali-saptawara-from-fixed date)
                 (bali-asatawara-from-fixed date)
                 (bali-sangawara-from-fixed date)
                 (bali-dasawara-from-fixed date)))

(defun bali-week-from-fixed (date)
  ;; TYPE fixed-date -> 1-30
  ;; Week number of $date$ in Balinese cycle.
  (1+ (quotient (bali-day-from-fixed date) 7)))

(defun bali-on-or-before (b-date date)
  ;; TYPE (balinese-date fixed-date) -> fixed-date
  ;; Last fixed date on or before $date$ with Pawukon $b-date$.
  (let* ((a5 ; Position in 5-day subcycle.
          (1- (bali-pancawara b-date)))
         (a6 ; Position in 6-day subcycle.
          (1- (bali-sadwara b-date)))
         (b7 ; Position in 7-day subcycle.
          (1- (bali-saptawara b-date)))
         (b35 ; Position in 35-day subcycle.
          (mod (+ a5 14 (* 15 (- b7 a5))) 35))
         (days ; Position in full cycle.
          (+ a6 (* 36 (- b35 a6))))
         (cap-Delta (bali-day-from-fixed 0)))
    (- date (mod (- (+ date cap-Delta) days) 210))))

(defun positions-in-range (n c cap-Delta range)
  ;; TYPE (positive-integer positive-integer
  ;; TYPE nonnegative-integer range)
  ;; TYPE -> list-of-fixed-dates
  ;; List of occurrences of $n$-th day of $c$-day cycle
  ;; in $range$.
  ;; $cap-Delta$ is position in cycle of RD 0.
  (let* ((a (start range))
         (b (end range))
         (pos (+ a (mod (- n a cap-Delta 1) c))))
    (if (> pos b)
      nil
      (append (list pos)
              (positions-in-range n c cap-Delta
                                  (interval (1+ pos) b))))))

(defun kajeng-keliwon (g-year)
  ;; TYPE gregorian-year -> list-of-fixed-dates
  ;; Occurrences of Kajeng Keliwon (9th day of each
  ;; 15-day subcycle of Pawukon) in Gregorian year $g-year$.
  (let* ((year (gregorian-year-range g-year))
         (cap-Delta (bali-day-from-fixed 0)))
    (positions-in-range 9 15 cap-Delta year)))

(defun tumpek (g-year)
  ;; TYPE gregorian-year -> list-of-fixed-dates
  ;; Occurrences of Tumpek (14th day of Pawukon and every
  ;; 35th subsequent day) within Gregorian year $g-year$.
  (let* ((year (gregorian-year-range g-year))
         (cap-Delta (bali-day-from-fixed 0)))
    (positions-in-range 14 35 cap-Delta year)))


;;;; Section: Time and Astronomy

(defun hr (x)
  ;; TYPE real -> duration
  ;; $x$ hours.
  (/ x 24))

(defun sec (x)
  ;; TYPE real -> duration
  ;; $x$ seconds.
  (/ x 24 60 60))

(defun mt (x)
  ;; TYPE real -> distance
  ;; $x$ meters.
  x)

(defun deg (x)
  ;; TYPE real -> angle
  ;; TYPE list-of-reals -> list-of-angles
  ;; $x$ degrees.
  x)

(defun secs (x)
  ;; TYPE real -> angle
  (/ x 3600))

(defun angle (d m s)
  ;; TYPE (nonnegative-integer
  ;; TYPE nonnegative-integer real) -> angle
  ;; $d$ degrees, $m$ arcminutes, $s$ arcseconds.
  (+ d (/ (+ m (/ s 60)) 60)))

(defun degrees (theta)
  ;; TYPE real -> angle
  ;; Normalize angle $theta$ to range [0,360) degrees.
  (mod theta 360))

(defun degrees-from-radians (theta)
  ;; TYPE radian -> angle
  ;; Convert angle $theta$ from radians to degrees.
  (degrees (/ theta pi 1/180)))

(defun radians-from-degrees (theta)
  ;; TYPE real -> radian
  ;; Convert angle $theta$ from degrees to radians.
  (* (degrees theta) pi 1/180))

(defun sin-degrees (theta)
  ;; TYPE angle -> amplitude
  ;; Sine of $theta$ (given in degrees).
  (sin (radians-from-degrees theta)))

(defun cosine-degrees (theta)
  ;; TYPE angle -> amplitude
  ;; Cosine of $theta$ (given in degrees).
  (cos (radians-from-degrees theta)))

(defun tangent-degrees (theta)
  ;; TYPE angle -> real
  ;; Tangent of $theta$ (given in degrees).
  (tan (radians-from-degrees theta)))

(defun arctan-degrees (y x)
  ;; TYPE (real real) -> angle
  ;; Arctangent of $y/x$ in degrees.
  (mod 
   (if (and (= x 0) (/= y 0))
       (* (signum y) (deg 90L0))
     (let* ((alpha (degrees-from-radians
                    (atan (/ y x)))))
       (if (>= x 0)
           alpha
         (+ alpha (deg 180L0)))))
   360))

(defun arcsin-degrees (x)
  ;; TYPE amplitude -> angle
  ;; Arcsine of $x$ in degrees.
  (degrees-from-radians (asin x)))

(defun arccos-degrees (x)
  ;; TYPE amplitude -> angle
  ;; Arccosine of $x$ in degrees.
  (degrees-from-radians (acos x)))

(defun location (latitude longitude elevation zone)
  ;; TYPE (angle angle distance real) -> location
  (list latitude longitude elevation zone))

(defun latitude (locale)
  ;; TYPE location -> angle
  (first locale))

(defun longitude (locale)
  ;; TYPE location -> angle
  (second locale))

(defun elevation (locale)
  ;; TYPE location -> distance
  (third locale))

(defun zone (locale)
  ;; TYPE location -> real
  (fourth locale))

(defconstant mecca
  ;; TYPE location
  ;; Location of Mecca.
  (location (angle 21 25 24) (angle 39 49 24)
            (mt 298) (hr 3)))

(defun direction (locale focus)
  ;; TYPE (location location) -> angle
  ;; Angle (clockwise from North) to face $focus$ when
  ;; standing in $locale$.  Subject to errors near focus and
  ;; its antipode.
  (let* ((phi (latitude locale))
         (phi-prime (latitude focus))
         (psi (longitude locale))
         (psi-prime (longitude focus))
         (y (sin-degrees (- psi-prime psi)))
         (x
          (- (* (cosine-degrees phi)
                (tangent-degrees phi-prime))
             (* (sin-degrees phi)
                (cosine-degrees
                 (- psi psi-prime))))))
    (cond ((or (= x y 0) (= phi-prime (deg 90)))
           (deg 0))
          ((= phi-prime (deg -90))
           (deg 180))
          (t (arctan-degrees y x)))))

(defun standard-from-universal (tee_rom-u locale)
  ;; TYPE (moment location) -> moment
  ;; Standard time from $tee_rom-u$ in universal time at
  ;; $locale$.
  (+ tee_rom-u (zone locale)))

(defun universal-from-standard (tee_rom-s locale)
  ;; TYPE (moment location) -> moment
  ;; Universal time from $tee_rom-s$ in standard time at
  ;; $locale$.
  (- tee_rom-s (zone locale)))

(defun zone-from-longitude (phi)
  ;; TYPE angle -> duration
  ;; Difference between UT and local mean time at longitude
  ;; $phi$ as a fraction of a day.
  (/ phi (deg 360)))

(defun local-from-universal (tee_rom-u locale)
  ;; TYPE (moment location) -> moment
  ;; Local time from universal $tee_rom-u$ at $locale$.
  (+ tee_rom-u (zone-from-longitude (longitude locale))))

(defun universal-from-local (tee_ell locale)
  ;; TYPE (moment location) -> moment
  ;; Universal time from local $tee_ell$ at $locale$.
  (- tee_ell (zone-from-longitude (longitude locale))))

(defun standard-from-local (tee_ell locale)
  ;; TYPE (moment location) -> moment
  ;; Standard time from local $tee_ell$ at $locale$.
  (standard-from-universal
   (universal-from-local tee_ell locale)
   locale))

(defun local-from-standard (tee_rom-s locale)
  ;; TYPE (moment location) -> moment
  ;; Local time from standard $tee_rom-s$ at $locale$.
  (local-from-universal
   (universal-from-standard tee_rom-s locale)
   locale))

(defun apparent-from-local (tee locale)
  ;; TYPE (moment location) -> moment
  ;; Sundial time at local time $tee$ at $locale$.
  (+ tee (equation-of-time (universal-from-local tee locale))))

(defun local-from-apparent (tee locale)
  ;; TYPE (moment location) -> moment
  ;; Local time from sundial time $tee$ at $locale$.
  (- tee (equation-of-time (universal-from-local tee locale))))

(defun midnight (date locale)
  ;; TYPE (fixed-date location) -> moment
  ;; Standard time on fixed $date$ of true (apparent)
  ;; midnight at $locale$.
  (standard-from-local
   (local-from-apparent date locale)
   locale))

(defun midday (date locale)
  ;; TYPE (fixed-date location) -> moment
  ;; Standard time on fixed $date$ of midday at $locale$.
  (standard-from-local 
   (local-from-apparent (+ date (hr 12L0)) locale)
   locale))

(defun julian-centuries (tee)
  ;; TYPE moment -> real
  ;; Julian centuries since 2000 at moment $tee$.
  (/ (- (dynamical-from-universal tee) j2000)
     36525L0))

(defun obliquity (tee)
  ;; TYPE moment -> angle
  ;; Obliquity of ecliptic at moment $tee$.
  (let* ((c (julian-centuries tee)))
    (+ (angle 23 26 21.448L0)
       (poly c (list 0L0
                     (angle 0 0 -46.8150L0)
                     (angle 0 0 -0.00059L0)
                     (angle 0 0 0.001813L0))))))

(defun declination (tee beta lambda)
  ;; TYPE (moment angle angle) -> angle
  ;; Declination at moment UT $tee$ of object at
  ;; longitude $lambda$ and latitude $beta$.
  (let* ((varepsilon (obliquity tee)))
    (arcsin-degrees (+ (* (sin-degrees beta)
                          (cosine-degrees varepsilon))
                       (* (cosine-degrees beta)
                          (sin-degrees varepsilon)
                          (sin-degrees lambda))))))

(defun right-ascension (tee beta lambda)
  ;; TYPE (moment angle angle) -> angle
  ;; Right ascension at moment UT $tee$ of object at
  ;; latitude $lambda$ and longitude $beta$
  (let* ((varepsilon (obliquity tee)))
    (arctan-degrees
     (- (* (sin-degrees lambda)
           (cosine-degrees varepsilon))
        (* (tangent-degrees beta)
           (sin-degrees varepsilon)))
     (cosine-degrees lambda))))

(defun sine-offset (tee locale alpha)
  ;; TYPE (moment location angle) -> real
  ;; Sine of angle between position of sun at 
  ;; local time $tee$ and
  ;; when its depression is $alpha$ at $locale$.
  ;; Out of range when it does not occur.
  (let* ((phi (latitude locale))
         (tee-prime (universal-from-local tee locale))
         (delta ; Declination of sun.
          (declination tee-prime (deg 0L0)
                       (solar-longitude tee-prime))))
    (+ (* (tangent-degrees phi)
          (tangent-degrees delta))
       (/ (sin-degrees alpha)
          (* (cosine-degrees delta)
             (cosine-degrees phi))))))

(defun approx-moment-of-depression (tee locale alpha early?)
  ;; TYPE (moment location angle boolean) -> moment
  ;; Moment in local time near $tee$ when depression angle
  ;; of sun is $alpha$ (negative if above horizon) at
  ;; $locale$; $early?$ is true when morning event is sought
  ;; and false for evening.  Returns bogus if depression
  ;; angle is not reached.
  (let* ((try (sine-offset tee locale alpha))
         (date (fixed-from-moment tee))
         (alt (if (>= alpha 0)
                  (if early? date (1+ date))
                (+ date (hr 12))))
         (value (if (> (abs try) 1)
                    (sine-offset alt locale alpha)
                  try)))
    (if (<= (abs value) 1) ; Event occurs
        (local-from-apparent
         (+ date (hr 12)
            (* (if early? -1 1)
               (- (mod (+ (hr 12)
                          (/ (arcsin-degrees value)
                             (deg 360)))
                       1)
                  (hr 6))))
         locale)
      bogus)))

(defun moment-of-depression (approx locale alpha early?)
  ;; TYPE (moment location angle boolean) -> moment
  ;; Moment in local time near $approx$ when depression
  ;; angle of sun is $alpha$ (negative if above horizon) at
  ;; $locale$; $early?$ is true when morning event is
  ;; sought, and false for evening.  
  ;; Returns bogus if depression angle is not reached.
   (let* ((tee (approx-moment-of-depression
                 approx locale alpha early?)))
     (if (equal tee bogus)
         bogus
       (if (< (abs (- approx tee))
              (sec 30))
           tee
         (moment-of-depression tee locale alpha early?)))))

(defconstant morning 
  ;; TYPE boolean
  ;; Signifies morning.
  true)

(defconstant evening
  ;; TYPE boolean
  ;; Signifies evening.
  false)

(defun dawn (date locale alpha)
  ;; TYPE (fixed-date location angle) -> moment
  ;; Standard time in morning on fixed $date$ at
  ;; $locale$ when depression angle of sun is $alpha$.
  ;; Returns bogus if there is no dawn on $date$.
  (let* ((result (moment-of-depression
                  (+ date (hr 6)) locale alpha morning)))
    (if (equal result bogus)
        bogus
      (standard-from-local result locale))))

(defun dusk (date locale alpha)
  ;; TYPE (fixed-date location angle) -> moment
  ;; Standard time in evening on fixed $date$ at
  ;; $locale$ when depression angle of sun is $alpha$.
  ;; Returns bogus if there is no dusk on $date$.
  (let* ((result (moment-of-depression
                  (+ date (hr 18)) locale alpha evening)))
    (if (equal result bogus)
        bogus
      (standard-from-local result locale))))

(defun sunrise (date locale)
  ;; TYPE (fixed-date location) -> moment
  ;; Standard time of sunrise on fixed $date$ at
  ;; $locale$.
  (let* ((h (max (mt 0) (elevation locale)))
         (cap-R (mt 6.372d6)) ; Radius of Earth.
         (dip ; Depression of visible horizon.
          (arccos-degrees (/ cap-R (+ cap-R h))))
         (alpha (+ (angle 0 50 0) dip
                   (* (secs 19) (sqrt h)))))
    (dawn date locale alpha)))

(defun sunset (date locale)
  ;; TYPE (fixed-date location) -> moment
  ;; Standard time of sunset on fixed $date$ at
  ;; $locale$.
  (let* ((h (max (mt 0) (elevation locale)))
         (cap-R (mt 6.372d6)) ; Radius of Earth.
         (dip ; Depression of visible horizon.
          (arccos-degrees (/ cap-R (+ cap-R h))))
         (alpha (+ (angle 0 50 0) dip
                   (* (secs 19) (sqrt h)))))
    (dusk date locale alpha)))

(defun jewish-dusk (date locale)
  ;; TYPE (fixed-date location) -> moment
  ;; Standard time of Jewish dusk on fixed $date$
  ;; at $locale$ (as per Vilna Gaon).
  (dusk date locale (angle 4 40 0)))

(defun jewish-sabbath-ends (date locale)
  ;; TYPE (fixed-date location) -> moment
  ;; Standard time of end of Jewish sabbath on fixed $date$
  ;; at $locale$ (as per Berthold Cohn).
  (dusk date locale (angle 7 5 0))) 

(defun daytime-temporal-hour (date locale)
  ;; TYPE (fixed-date location) -> real
  ;; Length of daytime temporal hour on fixed $date$ at $locale$.
  ;; Returns bogus if there no sunrise or sunset on $date$.
  (if (or (equal (sunrise date locale) bogus)
          (equal (sunset date locale) bogus))
      bogus
    (/ (- (sunset date locale)
          (sunrise date locale))
       12)))

(defun nighttime-temporal-hour (date locale)
  ;; TYPE (fixed-date location) -> real
  ;; Length of nighttime temporal hour on fixed $date$ at $locale$.
  ;; Returns bogus if there no sunrise or sunset on $date$.
  (if (or (equal (sunrise (1+ date) locale) bogus)
          (equal (sunset date locale) bogus))
      bogus
    (/ (- (sunrise (1+ date) locale)
          (sunset date locale))
       12)))

(defun standard-from-sundial (tee locale)
  ;; TYPE (moment location) -> moment
  ;; Standard time of temporal moment $tee$ at $locale$.
  ;; Returns bogus if temporal hour is undefined that day.
  (let* ((date (fixed-from-moment tee))
         (hour (* 24 (mod tee 1)))
         (h (cond ((<= 6 hour 18); daytime today
                   (daytime-temporal-hour date locale))
                  ((< hour 6)    ; early this morning
                   (nighttime-temporal-hour (1- date) locale))
                  (t             ; this evening
                   (nighttime-temporal-hour date locale)))))
    (cond ((equal h bogus) bogus)
          ((<= 6 hour 18); daytime today
           (+ (sunrise date locale) (* (- hour 6) h)))
          ((< hour 6)    ; early this morning
           (+ (sunset (1- date) locale) (* (+ hour 6) h)))
          (t             ; this evening
           (+ (sunset date locale) (* (- hour 18) h))))))

(defun jewish-morning-end (date locale)
  ;; TYPE (fixed-date location) -> moment
  ;; Standard time on fixed $date$ at $locale$ of end of
  ;; morning according to Jewish ritual.
  (standard-from-sundial (+ date (hr 10)) locale))

(defun asr (date locale)
  ;; TYPE (fixed-date location) -> moment
  ;; Standard time of asr on fixed $date$ at $locale$.
  (let* ((noon; Time when sun nearest zenith.
          (universal-from-standard
           (midday date locale)
           locale))
         (phi (latitude locale))
         (delta; Solar declination at noon.
          (declination noon (deg 0) (solar-longitude noon)))
         (altitude ; Solar altitude at noon.
          (- delta phi (deg 90)))
         (h; Sun's altitude when shadow increases by
          (arctan-degrees ; ... double its length.
           (tangent-degrees altitude)
           (1+ (* 2 (tangent-degrees altitude))))))
           ; For Shafii use instead:
           ; (1+ (tangent-degrees altitude)))
    (dusk date locale (- h))))

(defun universal-from-dynamical (tee)
  ;; TYPE moment -> moment
  ;; Universal moment from Dynamical time $tee$.
  (- tee (ephemeris-correction tee)))

(defun dynamical-from-universal (tee)
  ;; TYPE moment -> moment
  ;; Dynamical time at Universal moment $tee$.
  (+ tee (ephemeris-correction tee)))

(defconstant j2000
  ;; TYPE moment
  ;; Noon at start of Gregorian year 2000.
  (+ (hr 12L0) (gregorian-new-year 2000)))

(defun sidereal-from-moment (tee)
  ;; TYPE moment -> angle
  ;; Mean sidereal time of day from moment $tee$ expressed
  ;; as hour angle.  Adapted from "Astronomical Algorithms"
  ;; by Jean Meeus, Willmann-Bell, Inc., 1991.
  (let* ((c (/ (- tee j2000) 36525L0)))
    (mod (poly c 
               (deg (list 280.46061837L0
                          (* 36525L0 360.98564736629L0)
                          0.000387933L0 -1/38710000)))
         360)))

(defconstant mean-tropical-year
  ;; TYPE real
  365.242189L0)

(defconstant mean-sidereal-year
  ;; TYPE real
  365.25636L0)

(defconstant mean-synodic-month
  ;; TYPE real
  29.530588853L0)

(defun ephemeris-correction (tee)
  ;; TYPE moment -> fraction-of-day
  ;; Dynamical Time minus Universal Time (in days) for
  ;; moment $tee$.  Adapted from "Astronomical Algorithms"
  ;; by Jean Meeus, Willmann-Bell, Inc., 1991.
  (let* ((year (gregorian-year-from-fixed (floor tee)))
         (c (/ (gregorian-date-difference
                (gregorian-date 1900 january 1)
                (gregorian-date year july 1))
               36525L0)))
    (cond ((<= 1988 year 2019)
           (* 1/86400 (- year 1933)))
          ((<= 1900 year 1987)
           (poly c 
                 (list -0.00002L0 0.000297L0 0.025184L0
                       -0.181133L0 0.553040L0 -0.861938L0
                       0.677066L0 -0.212591L0)))
          ((<= 1800 year 1899)
           (poly c 
                 (list -0.000009L0 0.003844L0 0.083563L0
                       0.865736L0 4.867575L0 15.845535L0
                       31.332267L0 38.291999L0 28.316289L0
                       11.636204L0 2.043794L0)))
          ((<= 1700 year 1799)
           (* 1/86400
              (poly (- year 1700)
                    (list 8.118780842L0 -0.005092142L0
                          0.003336121L0 -0.0000266484L0))))
          ((<= 1620 year 1699)
           (* 1/86400
              (poly (- year 1600)
                    (list 196.58333L0 -4.0675L0 0.0219167L0))))
          (t (let* ((x (+ (hr 12L0)
                          (gregorian-date-difference
                           (gregorian-date 1810 january 1)
                           (gregorian-date year january 1)))))
               (* 1/86400 (- (/ (* x x) 41048480L0) 15)))))))

(defun equation-of-time (tee)
  ;; TYPE moment -> fraction-of-day
  ;; Equation of time (as fraction of day) for moment $tee$.
  ;; Adapted from "Astronomical Algorithms" by Jean Meeus,
  ;; Willmann-Bell, Inc., 1991.
  (let* ((c (julian-centuries tee))
         (lambda
          (poly c
                (deg (list 280.46645L0 36000.76983L0
                           0.0003032L0))))
         (anomaly
          (poly c
                (deg (list 357.52910L0 35999.05030L0
                      -0.0001559L0 -0.00000048L0))))
         (eccentricity
          (poly c
                (list 0.016708617L0 -0.000042037L0
                      -0.0000001236L0)))
         (varepsilon (obliquity tee))
         (y (expt (tangent-degrees (/ varepsilon 2)) 2))
         (equation
          (* (/ 1 2 pi)
             (+ (* y (sin-degrees (* 2 lambda)))
                (* -2 eccentricity (sin-degrees anomaly))
                (* 4 eccentricity y (sin-degrees anomaly)
                   (cosine-degrees (* 2 lambda)))
                (* -0.5 y y (sin-degrees (* 4 lambda)))
                (* -1.25 eccentricity eccentricity
                   (sin-degrees (* 2 anomaly)))))))
    (* (signum equation) (min (abs equation) (hr 12L0)))))

(defun solar-longitude (tee)
  ;; TYPE moment -> season
  ;; Longitude of sun at moment $tee$.
  ;; Adapted from "Planetary Programs and Tables from -4000
  ;; to +2800" by Pierre Bretagnon and Jean-Louis Simon,
  ;; Willmann-Bell, Inc., 1986.
  (let* ((c       ; moment in Julian centuries
          (julian-centuries tee))
         (coefficients
          (list 403406 195207 119433 112392 3891 2819 1721
                660 350 334 314 268 242 234 158 132 129 114
                99 93 86 78 72 68 64 46 38 37 32 29 28 27 27
                25 24 21 21 20 18 17 14 13 13 13 12 10 10 10
                10))
         (multipliers
          (list 0.9287892L0 35999.1376958L0 35999.4089666L0
                35998.7287385L0 71998.20261L0 71998.4403L0
                36000.35726L0 71997.4812L0 32964.4678L0
                -19.4410L0 445267.1117L0 45036.8840L0 3.1008L0
                22518.4434L0 -19.9739L0 65928.9345L0
                9038.0293L0 3034.7684L0 33718.148L0 3034.448L0
                -2280.773L0 29929.992L0 31556.493L0 149.588L0
                9037.750L0 107997.405L0 -4444.176L0 151.771L0
                67555.316L0 31556.080L0 -4561.540L0
                107996.706L0 1221.655L0 62894.167L0
                31437.369L0 14578.298L0 -31931.757L0
                34777.243L0 1221.999L0 62894.511L0
                -4442.039L0 107997.909L0 119.066L0 16859.071L0
                -4.578L0 26895.292L0 -39.127L0 12297.536L0
                90073.778L0))
         (addends
          (list 270.54861L0 340.19128L0 63.91854L0 331.26220L0
                317.843L0 86.631L0 240.052L0 310.26L0 247.23L0
                260.87L0 297.82L0 343.14L0 166.79L0 81.53L0
                3.50L0 132.75L0 182.95L0 162.03L0 29.8L0
                266.4L0 249.2L0 157.6L0 257.8L0 185.1L0 69.9L0
                8.0L0 197.1L0 250.4L0 65.3L0 162.7L0 341.5L0
                291.6L0 98.5L0 146.7L0 110.0L0 5.2L0 342.6L0
                230.9L0 256.1L0 45.3L0 242.9L0 115.2L0 151.8L0
                285.3L0 53.3L0 126.6L0 205.7L0 85.9L0
                146.1L0))
         (lambda
          (+ (deg 282.7771834L0)
             (* (deg 36000.76953744L0) c)
             (* (deg 0.000005729577951308232L0)
                (sigma ((x coefficients)
                        (y addends)
                        (z multipliers))
                       (* x (sin-degrees (+ y (* z c)))))))))
    (mod (+ lambda (aberration tee) (nutation tee))
         360)))

(defun nutation (tee)
  ;; TYPE moment -> angle
  ;; Longitudinal nutation at moment $tee$.
  (let* ((c       ; moment in Julian centuries
          (julian-centuries tee))
         (cap-A (poly c (deg (list 124.90L0 -1934.134L0
                                   0.002063L0))))
         (cap-B (poly c (deg (list 201.11L0 72001.5377L0
                                   0.00057L0)))))
    (+ (* (deg -0.004778L0) (sin-degrees cap-A))
       (* (deg -0.0003667L0) (sin-degrees cap-B)))))

(defun aberration (tee)
  ;; TYPE moment -> angle
  ;; Aberration at moment $tee$.
  (let* ((c       ; moment in Julian centuries
          (julian-centuries tee)))
    (- (* (deg 0.0000974L0)
          (cosine-degrees
           (+ (deg 177.63L0) (* (deg 35999.01848L0) c))))
       (deg 0.005575L0))))

(defun solar-longitude-after (lambda tee)
  ;; TYPE (season moment) -> moment
  ;; Moment UT of the first time at or after $tee$
  ;; when the solar longitude will be $lambda$ degrees.
  (let* ((rate ; Mean days for 1 degree change.
          (/ mean-tropical-year (deg 360)))
         (tau ; Estimate (within 5 days).
          (+ tee
             (* rate
                (mod (- lambda (solar-longitude tee)) 360))))
         (a (max tee (- tau 5))) ; At or after tee.
         (b (+ tau 5)))
    (invert-angular solar-longitude lambda a b)))

(defconstant spring
  ;; TYPE season
  ;; Longitude of sun at vernal equinox.
  (deg 0))

(defconstant summer
  ;; TYPE season
  ;; Longitude of sun at summer solstice.
  (deg 90))

(defconstant autumn
  ;; TYPE season
  ;; Longitude of sun at autumnal equinox.
  (deg 180))

(defconstant winter
  ;; TYPE season
  ;; Longitude of sun at winter solstice.
  (deg 270))

(defun precession (tee)
  ;; TYPE moment -> angle
  ;; Precession at moment $tee$ using 0,0 as J2000 coordinates.
  ;; Adapted from "Astronomical Algorithms" by Jean Meeus,
  ;; Willmann-Bell, Inc., 1991.
  (let* ((c (julian-centuries tee))
         (eta (mod
               (poly c (list 0 (secs 47.0029L0) 
                             (secs -0.03302L0)
                             (secs 0.000060L0)))
               360))
         (cap-P (mod (poly c (list (deg 174.876384L0) 
                                   (secs -869.8089L0) 
                                   (secs 0.03536L0)))
                     360))
         (p (mod (poly c (list 0 (secs 5029.0966L0)
                               (secs 1.11113L0)
                               (secs 0.000006L0)))
                 360))
         (cap-A (* (cosine-degrees eta) (sin-degrees cap-P)))
         (cap-B (cosine-degrees cap-P))
         (arg (arctan-degrees cap-A cap-B)))
    (mod (- (+ p cap-P) arg) 360)))

(defun sidereal-solar-longitude (tee)
  ;; TYPE moment -> season
  ;; Sidereal solar longitude at moment $tee$
  (mod (+ (solar-longitude tee)
          (- (precession tee))
          sidereal-start)
        360))

(defun estimate-prior-solar-longitude (lambda tee)
  ;; TYPE (season moment) -> moment
  ;; Approximate $moment$ at or before $tee$
  ;; when solar longitude just exceeded $lambda$ degrees.
  (let* ((rate ; Mean change of one degree.
          (/ mean-tropical-year (deg 360)))
         (tau ; First approximation.
          (- tee
             (* rate (mod (- (solar-longitude tee)
                             lambda)
                          360))))
         (cap-Delta ; Difference in longitude.
          (- (mod (- (solar-longitude tau)
                     lambda (- (deg 180)))
                  360)
             (deg 180))))
    (min tee (- tau (* rate cap-Delta)))))

(defun mean-lunar-longitude (c)
  ;; TYPE real -> angle
  ;; Mean longitude of moon (in degrees) at moment
  ;; given in Julian centuries $c$.
  ;; Adapted from "Astronomical Algorithms" by Jean Meeus,
  ;; Willmann-Bell, Inc., 2nd ed., 1998.
  (degrees
   (poly c
         (deg (list 218.3164477L0 481267.88123421L0
                    -0.0015786L0 1/538841 -1/65194000))))) 

(defun lunar-elongation (c)
  ;; TYPE real -> angle
  ;; Elongation of moon (in degrees) at moment
  ;; given in Julian centuries $c$.
  ;; Adapted from "Astronomical Algorithms" by Jean Meeus,
  ;; Willmann-Bell, Inc., 2nd ed., 1998.
  (degrees
   (poly c
         (deg (list 297.8501921L0 445267.1114034L0
                    -0.0018819L0 1/545868 -1/113065000)))))

(defun solar-anomaly (c)
  ;; TYPE real -> angle
  ;; Mean anomaly of sun (in degrees) at moment
  ;; given in Julian centuries $c$.
  ;; Adapted from "Astronomical Algorithms" by Jean Meeus,
  ;; Willmann-Bell, Inc., 2nd ed., 1998.
  (degrees
   (poly c
         (deg (list 357.5291092L0 35999.0502909L0
                    -0.0001536L0 1/24490000)))))

(defun lunar-anomaly (c)
  ;; TYPE real -> angle
  ;; Mean anomaly of moon (in degrees) at moment
  ;; given in Julian centuries $c$.
  ;; Adapted from "Astronomical Algorithms" by Jean Meeus,
  ;; Willmann-Bell, Inc., 2nd ed., 1998.
  (degrees
   (poly c
         (deg (list 134.9633964L0 477198.8675055L0
                    0.0087414L0 1/69699 -1/14712000)))))

(defun moon-node (c)
  ;; TYPE real -> angle
  ;; Moon's argument of latitude (in degrees) at moment
  ;; given in Julian centuries $c$.
  ;; Adapted from "Astronomical Algorithms" by Jean Meeus,
  ;; Willmann-Bell, Inc., 2nd ed., 1998.
  (degrees
   (poly c
         (deg (list 93.2720950L0 483202.0175233L0
                    -0.0036539L0 -1/3526000 1/863310000)))))

(defun lunar-longitude (tee)
  ;; TYPE moment -> angle
  ;; Longitude of moon (in degrees) at moment $tee$.
  ;; Adapted from "Astronomical Algorithms" by Jean Meeus,
  ;; Willmann-Bell, Inc., 2nd ed., 1998.
  (let* ((c (julian-centuries tee))
         (cap-L-prime (mean-lunar-longitude c))
         (cap-D (lunar-elongation c))
         (cap-M (solar-anomaly c))
         (cap-M-prime (lunar-anomaly c))
         (cap-F (moon-node c))
         (cap-E (poly c (list 1 -0.002516L0 -0.0000074L0)))
         (args-lunar-elongation
          (list 0 2 2 0 0 0 2 2 2 2 0 1 0 2 0 0 4 0 4 2 2 1
                1 2 2 4 2 0 2 2 1 2 0 0 2 2 2 4 0 3 2 4 0 2
                2 2 4 0 4 1 2 0 1 3 4 2 0 1 2))
         (args-solar-anomaly
          (list 0 0 0 0 1 0 0 -1 0 -1 1 0 1 0 0 0 0 0 0 1 1
                0 1 -1 0 0 0 1 0 -1 0 -2 1 2 -2 0 0 -1 0 0 1
                -1 2 2 1 -1 0 0 -1 0 1 0 1 0 0 -1 2 1 0))
         (args-lunar-anomaly
          (list 1 -1 0 2 0 0 -2 -1 1 0 -1 0 1 0 1 1 -1 3 -2
                -1 0 -1 0 1 2 0 -3 -2 -1 -2 1 0 2 0 -1 1 0
                -1 2 -1 1 -2 -1 -1 -2 0 1 4 0 -2 0 2 1 -2 -3
                2 1 -1 3))
         (args-moon-node
          (list 0 0 0 0 0 2 0 0 0 0 0 0 0 -2 2 -2 0 0 0 0 0
                0 0 0 0 0 0 0 2 0 0 0 0 0 0 -2 2 0 2 0 0 0 0
                0 0 -2 0 0 0 0 -2 -2 0 0 0 0 0 0 0))
         (sine-coefficients
          (list 6288774 1274027 658314 213618 -185116 -114332
                58793 57066 53322 45758 -40923 -34720 -30383
                15327 -12528 10980 10675 10034 8548 -7888
                -6766 -5163 4987 4036 3994 3861 3665 -2689
                -2602 2390 -2348 2236 -2120 -2069 2048 -1773
                -1595 1215 -1110 -892 -810 759 -713 -700 691
                596 549 537 520 -487 -399 -381 351 -340 330
                327 -323 299 294))
         (correction
          (* (deg 1/1000000)
             (sigma ((v sine-coefficients)
                     (w args-lunar-elongation)
                     (x args-solar-anomaly)
                     (y args-lunar-anomaly)
                     (z args-moon-node))
                    (* v (expt cap-E (abs x))
                       (sin-degrees
                        (+ (* w cap-D)
                           (* x cap-M)
                           (* y cap-M-prime)
                           (* z cap-F)))))))
         (venus (* (deg 3958/1000000)
                   (sin-degrees
                    (+ (deg 119.75L0) (* c (deg 131.849L0))))))
         (jupiter (* (deg 318/1000000)
                     (sin-degrees
                      (+ (deg 53.09L0)
                         (* c (deg 479264.29L0))))))
         (flat-earth
          (* (deg 1962/1000000)
             (sin-degrees (- cap-L-prime cap-F)))))
    (mod (+ cap-L-prime correction venus jupiter flat-earth
            (nutation tee))
         360)))

(defun nth-new-moon (n)
  ;; TYPE integer -> moment
  ;; Moment of $n$-th new moon after (or before) the new moon
  ;; of January 11, 1.  Adapted from "Astronomical Algorithms"
  ;; by Jean Meeus, Willmann-Bell, Inc., 2nd ed., 1998.
  (let* ((n0 24724) ; Months from RD 0 until j2000.
         (k (- n n0)) ; Months since j2000.
         (c (/ k 1236.85L0)) ; Julian centuries.
         (approx (+ j2000
                    (poly c (list 5.09765L0
                               (* mean-synodic-month
                                  1236.85L0)
                               0.0001337L0
                               -0.000000150L0
                               0.00000000073L0))))
         (cap-E (poly c (list 1 -0.002516L0 -0.0000074L0)))
         (solar-anomaly
          (poly c (deg (list 2.5534L0
                             (* 1236.85L0 29.10535669L0)
                             -0.0000218L0 -0.00000011L0))))
         (lunar-anomaly
          (poly c (deg (list 201.5643L0 (* 385.81693528L0
                                      1236.85L0)
                        0.0107438L0 0.00001239L0
                        -0.000000058L0))))
         (moon-argument ; Moon's argument of latitude.
          (poly c (deg (list 160.7108L0 (* 390.67050274L0
                                      1236.85L0)
                        -0.0016341L0 -0.00000227L0
                        0.000000011L0))))
         (cap-omega ; Longitude of ascending node.
          (poly c (list 124.7746L0 (* -1.56375580L0 1236.85L0)
                        0.0020691L0 0.00000215L0)))
         (E-factor (list 0 1 0 0 1 1 2 0 0 1 0 1 1 1 0 0 0 0
                         0 0 0 0 0 0))
         (solar-coeff (list 0 1 0 0 -1 1 2 0 0 1 0 1 1 -1 2
                            0 3 1 0 1 -1 -1 1 0))
         (lunar-coeff (list 1 0 2 0 1 1 0 1 1 2 3 0 0 2 1 2
                            0 1 2 1 1 1 3 4))
         (moon-coeff (list 0 0 0 2 0 0 0 -2 2 0 0 2 -2 0 0
                           -2 0 -2 2 2 2 -2 0 0))
         (sine-coeff
          (list -0.40720L0 0.17241L0 0.01608L0 0.01039L0
                0.00739L0 -0.00514L0 0.00208L0
                -0.00111L0 -0.00057L0 0.00056L0
                -0.00042L0 0.00042L0 0.00038L0
                -0.00024L0 -0.00007L0 0.00004L0
                0.00004L0 0.00003L0 0.00003L0
                -0.00003L0 0.00003L0 -0.00002L0
                -0.00002L0 0.00002L0))
         (correction
          (+ (* (deg -0.00017L0) (sin-degrees cap-omega))
             (sigma ((v sine-coeff)
                     (w E-factor)
                     (x solar-coeff)
                     (y lunar-coeff)
                     (z moon-coeff))
                    (* v (expt cap-E w)
                       (sin-degrees
                        (+ (* x solar-anomaly)
                           (* y lunar-anomaly)
                           (* z moon-argument)))))))
         (add-const
          (list 251.88L0 251.83L0 349.42L0 84.66L0
                141.74L0 207.14L0 154.84L0 34.52L0 207.19L0
                291.34L0 161.72L0 239.56L0 331.55L0))
         (add-coeff
          (list 0.016321L0 26.641886L0
                36.412478L0 18.206239L0 53.303771L0
                2.453732L0 7.306860L0 27.261239L0 0.121824L0
                1.844379L0 24.198154L0 25.513099L0
                3.592518L0))
         (add-factor
          (list 0.000165L0 0.000164L0 0.000126L0
                0.000110L0 0.000062L0 0.000060L0 0.000056L0
                0.000047L0 0.000042L0 0.000040L0 0.000037L0
                0.000035L0 0.000023L0))
         (extra
          (* (deg 0.000325L0)
             (sin-degrees
              (poly c
                    (deg (list 299.77L0 132.8475848L0
                          -0.009173L0))))))
         (additional
          (sigma ((i add-const)
                  (j add-coeff)
                  (l add-factor))
                 (* l (sin-degrees (+ i (* j k)))))))
    (universal-from-dynamical
     (+ approx correction extra additional))))

(defun new-moon-before (tee)
  ;; TYPE moment -> moment
  ;; Moment UT of last new moon before $tee$.
  (let* ((t0 (nth-new-moon 0))
         (phi (lunar-phase tee))
         (n (round (- (/ (- tee t0) mean-synodic-month)
                      (/ phi (deg 360))))))
    (nth-new-moon (final k (1- n) (< (nth-new-moon k) tee)))))

(defun new-moon-at-or-after (tee)
  ;; TYPE moment -> moment
  ;; Moment UT of first new moon at or after $tee$.
  (let* ((t0 (nth-new-moon 0))
         (phi (lunar-phase tee))
         (n (round (- (/ (- tee t0) mean-synodic-month)
                      (/ phi (deg 360))))))
    (nth-new-moon (next k n (>= (nth-new-moon k) tee)))))

(defun lunar-phase (tee)
  ;; TYPE moment -> phase
  ;; Lunar phase, as an angle in degrees, at moment $tee$.
  ;; An angle of 0 means a new moon, 90 degrees means the
  ;; first quarter, 180 means a full moon, and 270 degrees
  ;; means the last quarter.
  (let* ((phi (mod (- (lunar-longitude tee)
                      (solar-longitude tee))
                   360))
         (t0 (nth-new-moon 0))
         (n (round (/ (- tee t0) mean-synodic-month)))
         (phi-prime (* (deg 360)
                       (mod (/ (- tee (nth-new-moon n))
                               mean-synodic-month)
                            1))))
    (if (> (abs (- phi phi-prime)) (deg 180)) ; close call
        phi-prime
      phi)))

(defun lunar-phase-at-or-before (phi tee)
  ;; TYPE (phase moment) -> moment
  ;; Moment UT of the last time at or before $tee$
  ;; when the lunar-phase was $phi$ degrees.
  (let* ((tau ; Estimate.
          (- tee
             (* mean-synodic-month (/ 1 (deg 360))
                (mod (- (lunar-phase tee) phi) 360))))
         (a (- tau 2))
         (b (min tee (+ tau 2)))) ; At or before tee.
    (invert-angular lunar-phase phi a b)))

(defconstant new
  ;; TYPE phase
  ;; Excess of lunar longitude over solar longitude at new
  ;; moon.
  (deg 0))

(defconstant first-quarter
  ;; TYPE phase
  ;; Excess of lunar longitude over solar longitude at first
  ;; quarter moon.
  (deg 90))

(defconstant full
  ;; TYPE phase
  ;; Excess of lunar longitude over solar longitude at full
  ;; moon.
  (deg 180))

(defconstant last-quarter
  ;; TYPE phase
  ;; Excess of lunar longitude over solar longitude at last
  ;; quarter moon.
  (deg 270))

(defun lunar-phase-at-or-after (phi tee)
  ;; TYPE (phase moment) -> moment
  ;; Moment UT of the next time at or after $tee$
  ;; when the lunar-phase is $phi$ degrees.
  (let* ((tau ; Estimate.
          (+ tee
             (* mean-synodic-month (/ 1 (deg 360))
                (mod (- phi (lunar-phase tee)) 360))))
         (a (max tee (- tau 2))) ; At or after tee.
         (b (+ tau 2)))
    (invert-angular lunar-phase phi a b)))

(defun lunar-latitude (tee)
  ;; TYPE moment -> angle
  ;; Latitude of moon (in degrees) at moment $tee$.
  ;; Adapted from "Astronomical Algorithms" by Jean Meeus,
  ;; Willmann-Bell, Inc., 1998.
  (let* ((c (julian-centuries tee))
         (cap-L-prime (mean-lunar-longitude c))
         (cap-D (lunar-elongation c))
         (cap-M (solar-anomaly c))
         (cap-M-prime (lunar-anomaly c))
         (cap-F (moon-node c))
         (cap-E (poly c (list 1 -0.002516L0 -0.0000074L0)))
         (args-lunar-elongation
          (list 0 0 0 2 2 2 2 0 2 0 2 2 2 2 2 2 2 0 4 0 0 0
                1 0 0 0 1 0 4 4 0 4 2 2 2 2 0 2 2 2 2 4 2 2
                0 2 1 1 0 2 1 2 0 4 4 1 4 1 4 2))
         (args-solar-anomaly
          (list 0 0 0 0 0 0 0 0 0 0 -1 0 0 1 -1 -1 -1 1 0 1
                0 1 0 1 1 1 0 0 0 0 0 0 0 0 -1 0 0 0 0 1 1
                0 -1 -2 0 1 1 1 1 1 0 -1 1 0 -1 0 0 0 -1 -2))
         (args-lunar-anomaly
          (list 0 1 1 0 -1 -1 0 2 1 2 0 -2 1 0 -1 0 -1 -1 -1
                0 0 -1 0 1 1 0 0 3 0 -1 1 -2 0 2 1 -2 3 2 -3
                -1 0 0 1 0 1 1 0 0 -2 -1 1 -2 2 -2 -1 1 1 -2
                0 0))
         (args-moon-node
          (list 1 1 -1 -1 1 -1 1 1 -1 -1 -1 -1 1 -1 1 1 -1 -1
                -1 1 3 1 1 1 -1 -1 -1 1 -1 1 -3 1 -3 -1 -1 1
                -1 1 -1 1 1 1 1 -1 3 -1 -1 1 -1 -1 1 -1 1 -1
                -1 -1 -1 -1 -1 1))
         (sine-coefficients
          (list 5128122 280602 277693 173237 55413 46271 32573
                17198 9266 8822 8216 4324 4200 -3359 2463 2211
                2065 -1870 1828 -1794 -1749 -1565 -1491 -1475
                -1410 -1344 -1335 1107 1021 833 777 671 607
                596 491 -451 439 422 421 -366 -351 331 315
                302 -283 -229 223 223 -220 -220 -185 181
                -177 176 166 -164 132 -119 115 107))
         (beta
          (* (deg 1/1000000)
             (sigma ((v sine-coefficients)
                     (w args-lunar-elongation)
                     (x args-solar-anomaly)
                     (y args-lunar-anomaly)
                     (z args-moon-node))
                    (* v (expt cap-E (abs x))
                       (sin-degrees
                        (+ (* w cap-D)
                           (* x cap-M)
                           (* y cap-M-prime)
                           (* z cap-F)))))))
         (venus (* (deg 175/1000000)
                   (+ (sin-degrees
                       (+ (deg 119.75L0) (* c (deg 131.849L0))
                          cap-F))
                      (sin-degrees
                       (+ (deg 119.75L0) (* c (deg 131.849L0))
                          (- cap-F))))))
         (flat-earth
          (+ (* (deg -2235/1000000)
                (sin-degrees cap-L-prime))
             (* (deg 127/1000000) (sin-degrees
                             (- cap-L-prime cap-M-prime)))
             (* (deg -115/1000000) (sin-degrees
                              (+ cap-L-prime cap-M-prime)))))
         (extra (* (deg 382/1000000)
                   (sin-degrees
                    (+ (deg 313.45L0)
                       (* c (deg 481266.484L0)))))))
    (+ beta venus flat-earth extra)))

(defun lunar-altitude (tee locale)
  ;; TYPE (moment location) -> angle
  ;; Geocentric altitude of moon at $tee$ at $locale$, 
  ;; as a small positive/negative angle in degrees, ignoring
  ;; parallax and refraction.  Adapted from "Astronomical
  ;; Algorithms" by Jean Meeus, Willmann-Bell, Inc., 1998.
  (let* ((phi ; Local latitude.
          (latitude locale))
         (psi ; Local longitude.
          (longitude locale))
         (lambda ; Lunar longitude.
          (lunar-longitude tee))
         (beta ; Lunar latitude.
          (lunar-latitude tee))
         (alpha ; Lunar right ascension.
          (right-ascension tee beta lambda))
         (delta ; Lunar declination.
          (declination tee beta lambda))
         (theta0 ; Sidereal time.
          (sidereal-from-moment tee))
         (cap-H ; Local hour angle.
          (mod (- theta0 (- psi) alpha) 360))
         (altitude
          (arcsin-degrees (+ (* (sin-degrees phi)
                                (sin-degrees delta))
                             (* (cosine-degrees phi)
                                (cosine-degrees delta)
                                (cosine-degrees cap-H))))))
    (- (mod (+ altitude (deg 180)) 360) (deg 180))))
 
(defun lunar-distance (tee)
  ;; TYPE moment -> distance
  ;; Distance to moon (in meters) at moment $tee$.
  ;; Adapted from "Astronomical Algorithms" by Jean Meeus,
  ;; Willmann-Bell, Inc., 2nd ed.
  (let* ((c (julian-centuries tee))
         (cap-D (lunar-elongation c))
         (cap-M (solar-anomaly c))
         (cap-M-prime (lunar-anomaly c))
         (cap-F (moon-node c))
         (cap-E (poly c (list 1 -0.002516L0 -0.0000074L0)))
         (args-lunar-elongation
          (list 0 2 2 0 0 0 2 2 2 2 0 1 0 2 0 0 4 0 4 2 2 1
                1 2 2 4 2 0 2 2 1 2 0 0 2 2 2 4 0 3 2 4 0 2
                2 2 4 0 4 1 2 0 1 3 4 2 0 1 2 2))
         (args-solar-anomaly
          (list 0 0 0 0 1 0 0 -1 0 -1 1 0 1 0 0 0 0 0 0 1 1
                0 1 -1 0 0 0 1 0 -1 0 -2 1 2 -2 0 0 -1 0 0 1
                -1 2 2 1 -1 0 0 -1 0 1 0 1 0 0 -1 2 1 0 0))
         (args-lunar-anomaly
          (list 1 -1 0 2 0 0 -2 -1 1 0 -1 0 1 0 1 1 -1 3 -2
                -1 0 -1 0 1 2 0 -3 -2 -1 -2 1 0 2 0 -1 1 0
                -1 2 -1 1 -2 -1 -1 -2 0 1 4 0 -2 0 2 1 -2 -3
                2 1 -1 3 -1))
         (args-moon-node
          (list 0 0 0 0 0 2 0 0 0 0 0 0 0 -2 2 -2 0 0 0 0 0
                0 0 0 0 0 0 0 2 0 0 0 0 0 0 -2 2 0 2 0 0 0 0
                0 0 -2 0 0 0 0 -2 -2 0 0 0 0 0 0 0 -2))
         (cosine-coefficients
          (list -20905355 -3699111 -2955968 -569925 48888 -3149
                246158 -152138 -170733 -204586 -129620 108743
                104755 10321 0 79661 -34782 -23210 -21636 24208
                30824 -8379 -16675 -12831 -10445 -11650 14403
                -7003 0 10056 6322 -9884 5751 0 -4950 4130 0
                -3958 0 3258 2616 -1897 -2117 2354 0 0 -1423
                -1117 -1571 -1739 0 -4421 0 0 0 0 1165 0 0
                8752))
         (correction
          (sigma ((v cosine-coefficients)
                  (w args-lunar-elongation)
                  (x args-solar-anomaly)
                  (y args-lunar-anomaly)
                  (z args-moon-node))
                 (* v (expt cap-E (abs x))
                    (cosine-degrees
                        (+ (* w cap-D)
                           (* x cap-M)
                           (* y cap-M-prime)
                           (* z cap-F)))))))
    (+ (mt 385000560) correction)))

(defun lunar-parallax (tee locale)
  ;; TYPE (moment location) -> angle
  ;; Parallax of moon at $tee$ at $locale$.
  ;; Adapted from "Astronomical
  ;; Algorithms" by Jean Meeus, Willmann-Bell, Inc., 1998.
  (let* ((geo (lunar-altitude tee locale))
         (Delta (lunar-distance tee))
         (alt (/ (mt 6378140) Delta))
         (arg (* alt (cosine-degrees geo))))
    (arcsin-degrees arg)))

(defun topocentric-lunar-altitude (tee locale)
  ;; TYPE (moment location) -> angle
  ;; Topocentric altitude of moon at $tee$ at $locale$, 
  ;; as a small positive/negative angle in degrees,
  ;; ignoring refraction.
  (- (lunar-altitude tee locale)
     (lunar-parallax tee locale)))

(defun lunar-diameter (tee)
  ;; TYPE moment -> angle
  ;; Geocentric apparent lunar diameter of the moon (in
  ;; degrees) at moment $tee$.  Adapted from "Astronomical
  ;; Algorithms" by Jean Meeus, Willmann-Bell, Inc., 2nd ed.
  (/ (deg 1792367000/9) (lunar-distance tee)))


;;;; Section: Persian Calendar

(defun persian-date (year month day)
  ;; TYPE (persian-year persian-month persian-day)
  ;; TYPE -> persian-date
  (list year month day))

(defconstant persian-epoch
  ;; TYPE fixed-date
  ;; Fixed date of start of the Persian calendar.
  (fixed-from-julian (julian-date (ce 622) march 19)))

(defconstant tehran
  ;; TYPE location
  ;; Location of Tehran, Iran.
  (location (deg 35.68L0) (deg 51.42L0)
            (mt 1100) (hr (+ 3 1/2))))

(defun midday-in-tehran (date)
  ;; TYPE fixed-date -> moment
  ;; Universal time of midday on fixed $date$ in Tehran.
  (universal-from-standard
    (midday date tehran)
    tehran))

(defun persian-new-year-on-or-before (date)
  ;; TYPE fixed-date -> fixed-date
  ;; Fixed date of Astronomical Persian New Year on or
  ;; before fixed $date$.
  (let* ((approx ; Approximate time of equinox.
          (estimate-prior-solar-longitude
           spring (midday-in-tehran date))))
    (next day (- (floor approx) 1)
          (<= (solar-longitude (midday-in-tehran day))
              (+ spring (deg 2))))))

(defun fixed-from-persian (p-date)
  ;; TYPE persian-date -> fixed-date
  ;; Fixed date of Astronomical Persian date $p-date$.
  (let* ((month (standard-month p-date))
         (day (standard-day p-date))
         (year (standard-year p-date))
         (new-year
          (persian-new-year-on-or-before
           (+ persian-epoch 180; Fall after epoch.
              (floor
               (* mean-tropical-year
                  (if (< 0 year)
                      (1- year)
                    year))))))); No year zero.
    (+ (1- new-year)     ; Days in prior years.
       (if (<= month 7)  ; Days in prior months this year.
           (* 31 (1- month))
         (+ (* 30 (1- month)) 6))
       day)))            ; Days so far this month.

(defun persian-from-fixed (date)
  ;; TYPE fixed-date -> persian-date
  ;; Astronomical Persian date (year month day)
  ;; corresponding to fixed $date$.
  (let* ((new-year
          (persian-new-year-on-or-before date))
         (y (1+ (round (/ (- new-year persian-epoch)
                          mean-tropical-year))))
         (year (if (< 0 y)
                   y
                 (1- y))); No year zero
         (day-of-year (1+ (- date
                             (fixed-from-persian
                              (persian-date year 1 1)))))
         (month (if (<= day-of-year 186)
                    (ceiling (/ day-of-year 31))
                  (ceiling (/ (- day-of-year 6) 30))))
         (day           ; Calculate the day by subtraction
          (- date (1- (fixed-from-persian
                       (persian-date year month 1))))))
    (persian-date year month day)))

(defun arithmetic-persian-leap-year? (p-year)
  ;; TYPE persian-year -> boolean
  ;; True if $p-year$ is a leap year on the Persian calendar.
  (let* ((y ; Years since start of 2820-year cycles
          (if (< 0 p-year)
              (- p-year 474)
            (- p-year 473))); No year zero
         (year ; Equivalent year in the range 474...3263
          (+ (mod y 2820) 474)))
    (< (mod (* (+ year 38)
               31)
            128)
       31)))

(defun fixed-from-arithmetic-persian (p-date)
  ;; TYPE persian-date -> fixed-date
  ;; Fixed date equivalent to Persian date $p-date$.
  (let* ((day (standard-day p-date))
         (month (standard-month p-date))
         (p-year (standard-year p-date))
         (y ; Years since start of 2820-year cycle
          (if (< 0 p-year)
              (- p-year 474)
            (- p-year 473))); No year zero
         (year ; Equivalent year in the range 474...3263
          (+ (mod y 2820) 474)))
    (+ (1- persian-epoch); Days before epoch
       (* 1029983        ; Days in 2820-year cycles
                         ; before Persian year 474
          (quotient y 2820))
       (* 365 (1- year)) ; Nonleap days in prior years this
                         ; 2820-year cycle
       (quotient         ; Leap days in prior years this
                         ; 2820-year cycle
        (- (* 31 year) 5) 128)
       (if (<= month 7)  ; Days in prior months this year
           (* 31 (1- month))
         (+ (* 30 (1- month)) 6))
       day)))            ; Days so far this month

(defun arithmetic-persian-year-from-fixed (date)
  ;; TYPE fixed-date -> persian-year
  ;; Persian year corresponding to the fixed $date$.
  (let* ((d0      ; Prior days since start of 2820-year cycle
                  ; beginning in Persian year 474
          (- date (fixed-from-arithmetic-persian
                   (persian-date 475 1 1))))
         (n2820   ; Completed prior 2820-year cycles
          (quotient d0 1029983))
         (d1      ; Prior days not in n2820--that is, days
                  ; since start of last 2820-year cycle
          (mod d0 1029983))
         (y2820 ; Years since start of last 2820-year cycle
          (if (= d1 1029982)
              ;; Last day of 2820-year cycle
              2820
            ;; Otherwise use cycle of years formula
            (quotient (+ (* 128 d1) 46878)
                      46751)))
         (year    ; Years since Persian epoch
          (+ 474     ; Years before start of 2820-year cycles
             (* 2820 n2820) ; Years in prior 2820-year cycles
             y2820))); Years since start of last 2820-year
                     ; cycle
    (if (< 0 year)
        year
      (1- year)))); No year zero

(defun arithmetic-persian-from-fixed (date)
  ;; TYPE fixed-date -> persian-date
  ;; Persian date corresponding to fixed $date$.
  (let* ((year (arithmetic-persian-year-from-fixed date))
         (day-of-year (1+ (- date
                             (fixed-from-arithmetic-persian
                              (persian-date year 1 1)))))
         (month (if (<= day-of-year 186)
                    (ceiling (/ day-of-year 31))
                  (ceiling (/ (- day-of-year 6) 30))))
         (day           ; Calculate the day by subtraction
          (- date (1- (fixed-from-arithmetic-persian
                       (persian-date year month 1))))))
    (persian-date year month day)))

(defun naw-ruz (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of Persian New Year (Naw-Ruz) in Gregorian
  ;; year $g-year$.
  (let* ((persian-year
          (1+ (- g-year
                 (gregorian-year-from-fixed
                  persian-epoch))))
         (y (if (<= persian-year 0)
                ;; No Persian year 0
                (1- persian-year)
              persian-year)))
    (fixed-from-persian (persian-date y 1 1))))


;;;; Section: Bahai Calendar

(defun bahai-date (major cycle year month day)
  ;; TYPE (bahai-major bahai-cycle bahai-year
  ;; TYPE  bahai-month bahai-day) -> bahai-date
  (list major cycle year month day))

(defun bahai-major (date)
  ;; TYPE bahai-date -> bahai-major
  (first date))

(defun bahai-cycle (date)
  ;; TYPE bahai-date -> bahai-cycle
  (second date))

(defun bahai-year (date)
  ;; TYPE bahai-date -> bahai-year
  (third date))

(defun bahai-month (date)
  ;; TYPE bahai-date -> bahai-month
  (fourth date))

(defun bahai-day (date)
  ;; TYPE bahai-date -> bahai-day
  (fifth date))

(defconstant bahai-epoch
  ;; TYPE fixed-date
  ;; Fixed date of start of Bahai calendar.
  (fixed-from-gregorian (gregorian-date 1844 march 21)))

(defconstant ayyam-i-ha
  ;; TYPE bahai-month
  ;; Signifies intercalary period of 4 or 5 days.
  0)

(defun fixed-from-bahai (b-date)
  ;; TYPE bahai-date -> fixed-date
  ;; Fixed date equivalent to the Bahai date $b-date$.
  (let* ((major (bahai-major b-date))
         (cycle (bahai-cycle b-date))
         (year (bahai-year b-date))
         (month (bahai-month b-date))
         (day (bahai-day b-date))
         (g-year; Corresponding Gregorian year.
          (+ (* 361 (1- major))
             (* 19 (1- cycle)) year -1
             (gregorian-year-from-fixed bahai-epoch))))
    (+ (fixed-from-gregorian ; Prior years.
        (gregorian-date g-year march 20))
       (cond ((= month ayyam-i-ha) ; Intercalary period.
              342) ; 18 months have elapsed.
             ((= month 19); Last month of year.
              (if (gregorian-leap-year? (1+ g-year))
                  347  ; Long ayyam-i-ha.
                346)); Ordinary ayyam-i-ha.
             (t (* 19 (1- month)))); Elapsed months.
       day))) ; Days of current month.

(defun bahai-from-fixed (date)
  ;; TYPE fixed-date -> bahai-date
  ;; Bahai (major cycle year month day) corresponding to fixed
  ;; $date$.
  (let* ((g-year (gregorian-year-from-fixed date))
         (start   ; 1844
          (gregorian-year-from-fixed bahai-epoch))
         (years ; Since start of Bahai calendar.
          (- g-year start
             (if (<= date
                     (fixed-from-gregorian
                      (gregorian-date g-year march 20)))
                 1 0)))
         (major (1+ (quotient years 361)))
         (cycle (1+ (quotient (mod years 361) 19)))
         (year (1+ (mod years 19)))
         (days; Since start of year
          (- date (fixed-from-bahai
                   (bahai-date major cycle year 1 1))))
         (month
          (cond ((>= date
                     (fixed-from-bahai
                      (bahai-date major cycle year 19 1)))
                 19) ; Last month of year.
                ((>= date ; Intercalary days.
                     (fixed-from-bahai
                      (bahai-date major cycle year
                                  ayyam-i-ha 1)))
                 ayyam-i-ha) ; Intercalary period.
                (t (1+ (quotient days 19)))))
         (day (- date -1
                 (fixed-from-bahai
                  (bahai-date major cycle year month 1)))))
    (bahai-date major cycle year month day)))

(defun bahai-new-year (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of Bahai New Year in Gregorian year $g-year$.
  (fixed-from-gregorian
   (gregorian-date g-year march 21)))

(defconstant haifa
  ;; TYPE location
  ;; Location of Haifa, Israel.
  (location (deg 32.82L0) (deg 35) (mt 0) (hr 2)))

(defun sunset-in-haifa (date)
  ;; TYPE fixed-date -> moment
  ;; Universal time of sunset of evening before fixed $date$
  ;; in Haifa.
  (universal-from-standard
    (sunset date haifa)
    haifa))

(defun future-bahai-new-year-on-or-before (date)
  ;; TYPE fixed-date -> fixed-date
  ;; Fixed date of Future Bahai New Year on or before fixed
  ;; $date$.
  (let* ((approx ; Approximate time of equinox.
          (estimate-prior-solar-longitude
           spring (sunset-in-haifa date))))
    (next day (1- (floor approx))
          (<= (solar-longitude (sunset-in-haifa day))
              (+ spring (deg 2))))))

(defun fixed-from-future-bahai (b-date)
  ;; TYPE bahai-date -> fixed-date
  ;; Fixed date of Bahai date $b-date$.
  (let* ((major (bahai-major b-date))
         (cycle (bahai-cycle b-date))
         (year (bahai-year b-date))
         (month (bahai-month b-date))
         (day (bahai-day b-date))
         (years; Years from epoch
          (+ (* 361 (1- major))
             (* 19 (1- cycle))
             year)))
    (cond ((= month 19); last month of year
           (+ (future-bahai-new-year-on-or-before
               (+ bahai-epoch
                  (floor (* mean-tropical-year 
                            (+ years 1/2)))))
              -20 day))
          ((= month ayyam-i-ha)
           ; intercalary month, between 18th & 19th
           (+ (future-bahai-new-year-on-or-before
               (+ bahai-epoch
                  (floor (* mean-tropical-year
                            (- years 1/2)))))
              341 day))
          (t (+ (future-bahai-new-year-on-or-before
                 (+ bahai-epoch
                    (floor (* mean-tropical-year
                              (- years 1/2)))))
                (* (1- month) 19)
                day -1)))))

(defun future-bahai-from-fixed (date)
  ;; TYPE fixed-date -> bahai-date
  ;; Future Bahai date corresponding to fixed $date$.
  (let* ((new-year (future-bahai-new-year-on-or-before date))
         (years (round (/ (- new-year bahai-epoch)
                          mean-tropical-year)))
         (major (1+ (quotient years 361)))
         (cycle (1+ (quotient (mod years 361) 19)))
         (year (1+ (mod years 19)))
         (days; Since start of year
          (- date new-year))
         (month
          (cond
           ((>= date (fixed-from-future-bahai
                      (bahai-date major cycle year 19 1)))
            ; last month of year
            19)
           ((>= date
                (fixed-from-future-bahai
                 (bahai-date major cycle year ayyam-i-ha 1)))
            ; intercalary month
            ayyam-i-ha)
           (t (1+ (quotient days 19)))))
         (day (- date -1
                 (fixed-from-future-bahai
                  (bahai-date major cycle year month 1)))))
    (bahai-date major cycle year month day)))

(defun feast-of-ridvan (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of Feast of Ridvan in Gregorian year $g-year$.
  (let* ((years (- g-year
                   (gregorian-year-from-fixed
                    bahai-epoch)))
         (major (1+ (quotient years 361)))
         (cycle (1+ (quotient (mod years 361) 19)))
         (year (1+ (mod years 19))))
    (fixed-from-future-bahai
     (bahai-date major cycle year 2 13))))


;;;; Section: French Revolutionary Calendar

(defun french-date (year month day)
  ;; TYPE (french-year french-month french-day) -> french-date
  (list year month day))

(defconstant french-epoch
  ;; TYPE fixed-date
  ;; Fixed date of start of the French Revolutionary
  ;; calendar.
  (fixed-from-gregorian (gregorian-date 1792 september 22)))

(defconstant paris
  ;; TYPE location
  ;; Location of Paris Observatory.  Longitude corresponds
  ;; to difference of 9m 21s between Paris time zone and
  ;; Universal Time.
  (location (angle 48 50 11) (angle 2 20 15) (mt 27) (hr 1)))

(defun midnight-in-paris (date)
  ;; TYPE fixed-date -> moment
  ;; Universal time of true midnight at end of fixed $date$
  ;; in Paris.
  (universal-from-standard
   (midnight (+ date 1) paris)
   paris))

(defun french-new-year-on-or-before (date)
  ;; TYPE fixed-date -> fixed-date
  ;; Fixed date of French Revolutionary New Year on or
  ;; before fixed $date$.
  (let* ((approx ; Approximate time of solstice.
          (estimate-prior-solar-longitude
           autumn (midnight-in-paris date))))
    (next day (- (floor approx) 1)
          (<= autumn (solar-longitude
                      (midnight-in-paris day))))))

(defun fixed-from-french (f-date)
  ;; TYPE french-date -> fixed-date
  ;; Fixed date of French Revolutionary date.
  (let* ((month (standard-month f-date))
         (day (standard-day f-date))
         (year (standard-year f-date))
         (new-year
          (french-new-year-on-or-before
           (floor (+ french-epoch 180; Spring after epoch.
                     (* mean-tropical-year
                        (1- year)))))))
    (+ new-year -1      ;  Days in prior years
       (* 30 (1- month));  Days in prior months
       day)))           ;  Days this month

(defun french-from-fixed (date)
  ;; TYPE fixed-date -> french-date
  ;; French Revolutionary date of fixed $date$.
  (let* ((new-year
          (french-new-year-on-or-before date))
         (year (1+ (round (/ (- new-year french-epoch)
                             mean-tropical-year))))
         (month (1+ (quotient (- date new-year) 30)))
         (day (1+ (mod (- date new-year) 30))))
    (french-date year month day)))

(defun arithmetic-french-leap-year? (f-year)
  ;; TYPE french-year -> boolean
  ;; True if $f-year$ is a leap year on the French
  ;; Revolutionary calendar.
  (and (= (mod f-year 4) 0)
       (not (member (mod f-year 400) (list 100 200 300)))
       (not (= (mod f-year 4000) 0))))

(defun fixed-from-arithmetic-french (f-date)
  ;; TYPE french-date -> fixed-date
  ;; Fixed date of French Revolutionary date $f-date$.
  (let* ((month (standard-month f-date))
         (day (standard-day f-date))
         (year (standard-year f-date)))
    (+ french-epoch -1; Days before start of calendar.
       (* 365 (1- year)); Ordinary days in prior years.
       ; Leap days in prior years.
       (quotient (1- year) 4)
       (- (quotient (1- year) 100))
       (quotient (1- year) 400)
       (- (quotient (1- year) 4000))
       (* 30 (1- month)); Days in prior months this year.
       day))); Days this month.

(defun arithmetic-french-from-fixed (date)
  ;; TYPE fixed-date -> french-date
  ;; French Revolutionary date (year month day) of fixed
  ;; $date$.
  (let* ((approx   ; Approximate year (may be off by 1).
          (1+ (quotient (- date french-epoch -2)
                        1460969/4000)))
         (year (if (< date
                      (fixed-from-arithmetic-french
                       (french-date approx 1 1)))
                   (1- approx)
                 approx))
         (month    ; Calculate the month by division.
          (1+ (quotient
               (- date (fixed-from-arithmetic-french
                         (french-date year 1 1)))
               30)))
         (day      ; Calculate the day by subtraction.
          (1+ (- date
                 (fixed-from-arithmetic-french
                  (french-date year month 1))))))
    (french-date year month day)))


;;;; Section: Chinese Calendar

(defun chinese-date (cycle year month leap day)
  ;; TYPE (chinese-cycle chinese-year chinese-month
  ;; TYPE  chinese-leap chinese-day) -> chinese-date
  (list cycle year month leap day))

(defun chinese-cycle (date)
  ;; TYPE chinese-date -> chinese-cycle
  (first date))

(defun chinese-year (date)
  ;; TYPE chinese-date -> chinese-year
  (second date))

(defun chinese-month (date)
  ;; TYPE chinese-date -> chinese-month
  (third date))

(defun chinese-leap (date)
  ;; TYPE chinese-date -> chinese-leap
  (fourth date))

(defun chinese-day (date)
  ;; TYPE chinese-date -> chinese-day
  (fifth date))

(defun chinese-location (tee)
  ;; TYPE moment -> location
  ;; Location of Beijing; time zone varies with $tee$.
  (let* ((year (gregorian-year-from-fixed (floor tee))))
    (if (< year 1929)
        (location (angle 39 55 0) (angle 116 25 0)
                  (mt 43.5) (hr 1397/180))
      (location (angle  39 55 0) (angle 116 25 0)
                (mt 43.5) (hr 8)))))

(defun chinese-solar-longitude-on-or-after (lambda date)
  ;; TYPE (season moment) -> moment
  ;; Moment (Beijing time) of the first date on or after
  ;; fixed $date$ (Beijing time) when the solar longitude
  ;; will be $lambda$ degrees.
  (let* ((tee (solar-longitude-after
               lambda
               (universal-from-standard
                date
                (chinese-location date)))))
    (standard-from-universal
     tee
     (chinese-location tee))))

(defun current-major-solar-term (date)
  ;; TYPE fixed-date -> integer
  ;; Last Chinese major solar term (zhongqi) before fixed
  ;; $date$.
  (let* ((s (solar-longitude
             (universal-from-standard
              date
              (chinese-location date)))))
    (amod (+ 2 (quotient s (deg 30))) 12)))

(defun major-solar-term-on-or-after (date)
  ;; TYPE fixed-date -> moment
  ;; Moment (in Beijing) of the first Chinese major
  ;; solar term (zhongqi) on or after fixed $date$.  The
  ;; major terms begin when the sun's longitude is a
  ;; multiple of 30 degrees.
  (let* ((s (solar-longitude (midnight-in-china date)))
         (l (mod (* 30 (ceiling (/ s 30))) 360)))
    (chinese-solar-longitude-on-or-after l date)))

(defun current-minor-solar-term (date)
  ;; TYPE fixed-date -> integer
  ;; Last Chinese minor solar term (jieqi) before $date$.
  (let* ((s (solar-longitude
             (universal-from-standard
              date
              (chinese-location date)))))
    (amod (+ 3 (quotient (- s (deg 15)) (deg 30))) 
                  12)))

(defun minor-solar-term-on-or-after (date)
  ;; TYPE fixed-date -> moment
  ;; Moment (in Beijing) of the first Chinese minor solar
  ;; term (jieqi) on or after fixed $date$.  The minor terms
  ;; begin when the sun's longitude is an odd multiple of 15
  ;; degrees.
  (let* ((s (solar-longitude (midnight-in-china date)))
         (l (mod
             (+ (* 30
                   (ceiling
                    (/ (- s (deg 15)) 30)))
                (deg 15))
             360)))
    (chinese-solar-longitude-on-or-after l date)))

(defun chinese-new-moon-before (date)
  ;; TYPE fixed-date -> fixed-date
  ;; Fixed date (Beijing) of first new moon before fixed
  ;; $date$.
  (let* ((tee (new-moon-before
               (midnight-in-china date))))
    (floor
     (standard-from-universal
      tee
      (chinese-location tee)))))

(defun chinese-new-moon-on-or-after (date)
  ;; TYPE fixed-date -> fixed-date
  ;; Fixed date (Beijing) of first new moon on or after
  ;; fixed $date$.
  (let* ((tee (new-moon-at-or-after
               (midnight-in-china date))))
    (floor
     (standard-from-universal
      tee
      (chinese-location tee)))))

(defconstant chinese-epoch
  ;; TYPE fixed-date
  ;; Fixed date of start of the Chinese calendar.
  (fixed-from-gregorian (gregorian-date -2636 february 15)))

(defun chinese-no-major-solar-term? (date)
  ;; TYPE fixed-date -> boolean
  ;; True if Chinese lunar month starting on $date$
  ;; has no major solar term.
  (= (current-major-solar-term date)
     (current-major-solar-term
      (chinese-new-moon-on-or-after (+ date 1)))))

(defun midnight-in-china (date)
  ;; TYPE fixed-date -> moment
  ;; Universal time of (clock) midnight at start of fixed
  ;; $date$ in China.
  (universal-from-standard date (chinese-location date)))

(defun chinese-winter-solstice-on-or-before (date)
  ;; TYPE fixed-date -> fixed-date
  ;; Fixed date, in the Chinese zone, of winter solstice
  ;; on or before fixed $date$.
  (let* ((approx ; Approximate time of solstice.
          (estimate-prior-solar-longitude
           winter (midnight-in-china (+ date 1)))))
    (next day (1- (floor approx))
          (< winter (solar-longitude
                     (midnight-in-china (1+ day)))))))

(defun chinese-new-year-in-sui (date)
  ;; TYPE fixed-date -> fixed-date
  ;; Fixed date of Chinese New Year in sui (period from
  ;; solstice to solstice) containing $date$.
  (let* ((s1; prior solstice
          (chinese-winter-solstice-on-or-before date))
         (s2; following solstice
          (chinese-winter-solstice-on-or-before
           (+ s1 370)))
         (m12 ; month after 11th month--either 12 or leap 11
          (chinese-new-moon-on-or-after (1+ s1)))
         (m13 ; month after m12--either 12 (or leap 12) or 1
          (chinese-new-moon-on-or-after (1+ m12)))
         (next-m11 ; next 11th month
          (chinese-new-moon-before (1+ s2))))
    (if ; Either m12 or m13 is a leap month if there are
        ; 13 new moons (12 full lunar months) and
        ; either m12 or m13 has no major solar term
        (and (= (round (/ (- next-m11 m12)
                          mean-synodic-month))
                12)
             (or (chinese-no-major-solar-term? m12)
                 (chinese-no-major-solar-term? m13)))
        (chinese-new-moon-on-or-after (1+ m13))
      m13)))

(defun chinese-new-year-on-or-before (date)
  ;; TYPE fixed-date -> fixed-date
  ;; Fixed date of Chinese New Year on or before fixed $date$.
  (let* ((new-year (chinese-new-year-in-sui date)))
    (if (>= date new-year)
        new-year
      ; Got the New Year after--this happens if date is
      ; after the solstice but before the new year.
      ; So, go back half a year.
      (chinese-new-year-in-sui (- date 180)))))

(defun chinese-new-year (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of Chinese New Year in Gregorian year $g-year$.
  (chinese-new-year-on-or-before
   (fixed-from-gregorian
    (gregorian-date g-year july 1))))

(defun chinese-from-fixed (date)
  ;; TYPE fixed-date -> chinese-date
  ;; Chinese date (cycle year month leap day) of fixed $date$.
  (let* ((s1; Prior solstice
          (chinese-winter-solstice-on-or-before date))
         (s2; Following solstice
          (chinese-winter-solstice-on-or-before (+ s1 370)))
         (m12     ; month after last 11th month
          (chinese-new-moon-on-or-after (1+ s1)))
         (next-m11; next 11th month
          (chinese-new-moon-before (1+ s2)))
         (m      ; start of month containing date
          (chinese-new-moon-before (1+ date)))
         (leap-year; if there are 13 new moons (12 full
                   ; lunar months)
          (= (round (/ (- next-m11 m12)
                       mean-synodic-month))
             12))
         (month  ; month number
          (amod
           (-
            ;; ordinal position of month in year
            (round (/ (- m m12) mean-synodic-month))
            ;; minus 1 during or after a leap month
            (if (and leap-year
                     (chinese-prior-leap-month? m12 m))
                1
              0))
           12))
         (leap-month    ; it's a leap month if...
          (and
           leap-year; ...there are 13 months
           (chinese-no-major-solar-term?
            m)                          ; no major solar term
           (not (chinese-prior-leap-month? ; and no prior leap
                                           ; month
                     m12 (chinese-new-moon-before m)))))
         (elapsed-years  ; Approximate since the epoch
          (floor (+ 1.5L0  ; 18 months (because of truncation)
                    (- (/ month 12)); after at start of year
                    (/ (- date chinese-epoch)
                       mean-tropical-year))))
         (cycle (1+ (quotient (1- elapsed-years) 60)))
         (year (amod elapsed-years 60))
         (day (1+ (- date m))))
    (chinese-date cycle year month leap-month day)))

(defun fixed-from-chinese (c-date)
  ;; TYPE chinese-date -> fixed-date
  ;; Fixed date of Chinese date $c-date$.
  (let* ((cycle (chinese-cycle c-date))
         (year (chinese-year c-date))
         (month (chinese-month c-date))
         (leap (chinese-leap c-date))
         (day (chinese-day c-date))
         (mid-year      ;  Middle of the Chinese year
          (floor
           (+ chinese-epoch
              (* (+ (* (1- cycle) 60); years in prior cycles
                    (1- year)        ; prior years this cycle
                    1/2)           ; half a year
                 mean-tropical-year))))
         (new-year (chinese-new-year-on-or-before mid-year))
         (p; new moon before date--a month too early if
           ; there was prior leap month that year
          (chinese-new-moon-on-or-after
           (+ new-year (* (1- month) 29))))
         (d (chinese-from-fixed p))
         (prior-new-moon
          (if  ; If the months match...
              (and (= month (chinese-month d))
                   (equal leap (chinese-leap d)))
              p; ...that's the right month
            ;; otherwise, there was a prior leap month that
            ;; year, so we want the next month
            (chinese-new-moon-on-or-after (1+ p)))))
    (+ prior-new-moon day -1)))

(defun chinese-prior-leap-month? (m-prime m)
  ;; TYPE (fixed-date fixed-date) -> boolean
  ;; True if there is a Chinese leap month on or after lunar
  ;; month starting on fixed day $m-prime$ and at or before
  ;; lunar month starting at fixed date $m$.
  (and (>= m m-prime)
       (or (chinese-no-major-solar-term? m)
           (chinese-prior-leap-month? 
            m-prime
            (chinese-new-moon-before m)))))

(defun chinese-name (stem branch)
  ;; TYPE (chinese-stem chinese-branch) -> chinese-name
  ;; Returns bogus if $stem$/$branch$ combination is
  ;; impossible.
  (if (= (mod stem 2) (mod branch 2))
      (list stem branch)
    bogus))

(defun chinese-stem (name)
  ;; TYPE chinese-name -> chinese-stem
  (first name))

(defun chinese-branch (name)
  ;; TYPE chinese-name -> chinese-branch
  (second name))

(defun chinese-sexagesimal-name (n)
  ;; TYPE integer -> chinese-name
  ;; The $n$-th name of the Chinese sexagesimal cycle.
  (chinese-name (amod n 10)
                (amod n 12)))

(defun chinese-name-difference (c-name1 c-name2)
  ;; TYPE (chinese-name chinese-name) -> nonnegative-integer
  ;; Number of names from Chinese name $c-name1$ to the
  ;; next occurrence of Chinese name $c-name2$.
  (let* ((stem1 (chinese-stem c-name1))
         (stem2 (chinese-stem c-name2))
         (branch1 (chinese-branch c-name1))
         (branch2 (chinese-branch c-name2))
         (stem-difference (- stem2 stem1))
         (branch-difference (- branch2 branch1)))
    (1+ (mod (+ (1- stem-difference)
                (* 25 (- branch-difference
                         stem-difference)))
             60))))

(defun chinese-name-of-year (year)
  ;; TYPE chinese-year -> chinese-name
  ;; Sexagesimal name for Chinese $year$ of any cycle.
  (chinese-sexagesimal-name year))

(defconstant chinese-month-name-epoch
  ;; TYPE integer
  ;; Elapsed months at start of Chinese sexagesimal month
  ;; cycle.
  57)

(defun chinese-name-of-month (month year)
  ;; TYPE (chinese-month chinese-year) -> chinese-name
  ;; Sexagesimal name for month $month$ of Chinese year
  ;; $year$.
  (let* ((elapsed-months (+ (* 12 (1- year))
                            (1- month))))
    (chinese-sexagesimal-name
     (- elapsed-months chinese-month-name-epoch))))

(defconstant chinese-day-name-epoch
  ;; TYPE integer
  ;; RD date of a start of Chinese sexagesimal day cycle.
  (rd 45))

(defun chinese-name-of-day (date)
  ;; TYPE fixed-date -> chinese-name
  ;; Chinese sexagesimal name for $date$.
    (chinese-sexagesimal-name
     (- date chinese-day-name-epoch)))

(defun chinese-day-name-on-or-before (name date)
  ;; TYPE (chinese-name fixed-date) -> fixed-date
  ;; Fixed date of latest date on or before fixed $date$
  ;; that has Chinese $name$.
  (- date
     (mod (+ date
             (chinese-name-difference
              name
              (chinese-sexagesimal-name
               chinese-day-name-epoch)))
          60)))

(defun dragon-festival (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of the Dragon Festival occurring in
  ;; Gregorian year $g-year$.
  (let* ((elapsed-years
          (1+ (- g-year
                 (gregorian-year-from-fixed
                  chinese-epoch))))
         (cycle (1+ (quotient (1- elapsed-years) 60)))
         (year (amod elapsed-years 60)))
    (fixed-from-chinese (chinese-date cycle year 5 false 5))))

(defun qing-ming (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of Qingming occurring in Gregorian year
  ;; $g-year$.
  (floor
   (minor-solar-term-on-or-after
    (fixed-from-gregorian
     (gregorian-date g-year march 30)))))

(defun chinese-age (birthdate date)
  ;; TYPE (chinese-date fixed-date) -> nonnegative-integer
  ;; Age at fixed $date$, given Chinese $birthdate$,
  ;; according to the Chinese custom.  Returns bogus if
  ;; $date$ is before $birthdate$.
  (let* ((today (chinese-from-fixed date)))
    (if (>= date (fixed-from-chinese birthdate))
        (+ (* 60 (- (chinese-cycle today)
                    (chinese-cycle birthdate)))
           (- (chinese-year today)
              (chinese-year birthdate))
           1)
      bogus)))

(defun chinese-year-marriage-augury (cycle year)
  ;; TYPE (chinese-cycle chinese-year) ->  {0,1,2,3}
  ;; The marriage augury type of Chinese $year$ in $cycle$.
  ;; 0 means lichun does not occur (widow or double-blind
  ;; years), 1 means it occurs once at the end (blind), 2
  ;; means it occurs once at the start (bright), and 3 means
  ;; it occurs twice (double-bright or double-happiness).
  (let* ((new-year (fixed-from-chinese
                    (chinese-date cycle year 1 false 1)))
         (c (if (= year 60); next year's cycle
                (1+ cycle)
              cycle))
         (y (if (= year 60); next year's number
                1
              (1+ year)))
         (next-new-year (fixed-from-chinese
                         (chinese-date c y 1 false 1)))
         (first-minor-term
          (current-minor-solar-term new-year))
         (next-first-minor-term
          (current-minor-solar-term next-new-year)))
    (cond
     ((and
       (= first-minor-term 1)        ; no lichun at start...
       (= next-first-minor-term 12)) ; ...or at end
      0)    ; double blind
     ((and
       (= first-minor-term 1)        ; no lichun at start...
       (/= next-first-minor-term 12)); ...only at end
      1)     ; blind
     ((and
       (/= first-minor-term 1)       ; lichun at start...
       (= next-first-minor-term 12)) ; ... not at end
      2)     ; bright
     (t 3)))); double-bright          lichun at start and end

(defun japanese-location (tee)
  ;; TYPE moment -> location
  ;; Location for Japanese calendar; varies with $tee$.
  (let* ((year (gregorian-year-from-fixed (floor tee))))
    (if (< year 1888)
        ; Tokyo (139 deg 46 min east) local time
        (location (deg 35.7L0) (angle 139 46 0)
                  (mt 24) (hr (+ 9 143/450)))
      ; Longitude 135 time zone
      (location (deg 35) (deg 135) (mt 0) (hr 9)))))

(defun korean-location (tee)
  ;; TYPE moment -> location
  ;; Location for Korean calendar; varies with $tee$.
  ;; Seoul city hall at a varying time zone.
  (let* ((z (cond
             ((< tee
                 (fixed-from-gregorian
                  (gregorian-date 1908 april 1)))
              ;; local mean time for longitude 126 deg 58 min
              3809/450)
             ((< tee
                 (fixed-from-gregorian
                  (gregorian-date 1912 january 1)))
              8.5)
             ((< tee
                 (fixed-from-gregorian
                  (gregorian-date 1954 march 21)))
              9)
             ((< tee
                 (fixed-from-gregorian
                  (gregorian-date 1961 august 10)))
              8.5)
             (t 9))))
    (location (angle 37 34 0) (angle 126 58 0)
              (mt 0) (hr z))))

(defun korean-year (cycle year)
  ;; TYPE (chinese-cycle chinese-year) -> integer
  ;; Equivalent Korean year to Chinese $cycle$ and $year$
  (+ (* 60 cycle) year -364))

(defun vietnamese-location (tee)
  ;; TYPE moment -> location
  ;; Location for Vietnamese calendar is Hanoi; varies with
  ;; $tee$.  Time zone has changed over the years.
  (let* ((z (if (< tee
                   (gregorian-new-year 1968))
                8
              7)))
    (location (angle 21 2 0) (angle 105 51 0)
              (mt 12) (hr z))))


;;;; Section: Modern Hindu Calendars

(defun hindu-lunar-date (year month leap-month day leap-day)
  ;; TYPE (hindu-lunar-year hindu-lunar-month
  ;; TYPE  hindu-lunar-leap-month hindu-lunar-day
  ;; TYPE  hindu-lunar-leap-day) -> hindu-lunar-date
  (list year month leap-month day leap-day))

(defun hindu-lunar-month (date)
  ;; TYPE hindu-lunar-date -> hindu-lunar-month
  (second date))

(defun hindu-lunar-leap-month (date)
  ;; TYPE hindu-lunar-date -> hindu-lunar-leap-month
  (third date))

(defun hindu-lunar-day (date)
  ;; TYPE hindu-lunar-date -> hindu-lunar-day
  (fourth date))

(defun hindu-lunar-leap-day (date)
  ;; TYPE hindu-lunar-date -> hindu-lunar-leap-day
  (fifth date))

(defun hindu-lunar-year (date)
  ;; TYPE hindu-lunar-date -> hindu-lunar-year
  (first date))

(defun hindu-sine-table (entry)
  ;; TYPE integer -> rational-amplitude
  ;; This simulates the Hindu sine table.
  ;; $entry$ is an angle given as a multiplier of 225'.
  (let* ((exact (* 3438 (sin-degrees
                         (* entry (angle 0 225 0)))))
         (error (* 0.215 (signum exact)
                   (signum (- (abs exact) 1716)))))
    (/ (round (+ exact error)) 3438)))

(defun hindu-sine (theta)
  ;; TYPE angle -> rational-amplitude
  ;; Linear interpolation for $theta$ in Hindu table.
  (let* ((entry
          (/ theta (angle 0 225 0))); Interpolate in table.
         (fraction (mod entry 1)))
    (+ (* fraction
          (hindu-sine-table (ceiling entry)))
       (* (- 1 fraction)
          (hindu-sine-table (floor entry))))))

(defun hindu-arcsin (amp)
  ;; TYPE rational-amplitude -> rational-angle
  ;; Inverse of Hindu sine function of $amp$.
  (if (< amp 0) (- (hindu-arcsin (- amp)))
    (let* ((pos (next k 0 (<= amp (hindu-sine-table k))))
           (below ; Lower value in table.
            (hindu-sine-table (1- pos))))
      (* (angle 0 225 0)
         (+ pos -1  ; Interpolate.
            (/ (- amp below)
               (- (hindu-sine-table pos) below)))))))

(defconstant hindu-sidereal-year
  ;; TYPE rational
  ;; Mean length of Hindu sidereal year.
  (+ 365 279457/1080000))

(defconstant hindu-creation
  ;; TYPE fixed-date
  ;; Fixed date of Hindu creation.
  (- hindu-epoch (* 1955880000 hindu-sidereal-year)))

(defun hindu-mean-position (tee period)
  ;; TYPE (rational-moment rational) -> rational-angle
  ;; Position in degrees at moment $tee$ in uniform circular
  ;; orbit of $period$ days.
  (* (deg 360) (mod (/ (- tee hindu-creation) period) 1)))

(defconstant hindu-sidereal-month
  ;; TYPE rational
  ;; Mean length of Hindu sidereal month.
  (+ 27 4644439/14438334))

(defconstant hindu-synodic-month
  ;; TYPE rational
  ;; Mean time from new moon to new moon.
  (+ 29 7087771/13358334))

(defconstant hindu-anomalistic-year
  ;; TYPE rational
  ;; Time from aphelion to aphelion.
  (/ 1577917828000 (- 4320000000 387)))

(defconstant hindu-anomalistic-month
  ;; TYPE rational
  ;; Time from apogee to apogee, with bija correction.
  (/ 1577917828 (- 57753336 488199)))

(defun hindu-true-position (tee period size anomalistic change)
  ;; TYPE (rational-moment rational rational rational
  ;; TYPE  rational) -> rational-angle
  ;; Longitudinal position at moment $tee$.  $period$ is
  ;; period of mean motion in days.  $size$ is ratio of
  ;; radii of epicycle and deferent.  $anomalistic$ is the
  ;; period of retrograde revolution about epicycle.
  ;; $change$ is maximum decrease in epicycle size.
  (let* ((lambda ; Position of epicycle center
          (hindu-mean-position tee period))
         (offset ; Sine of anomaly
          (hindu-sine (hindu-mean-position tee anomalistic)))
         (contraction (* (abs offset) change size))
         (equation ; Equation of center
          (hindu-arcsin (* offset (- size contraction)))))
    (mod (- lambda equation) 360)))

(defun hindu-solar-longitude (tee)
  ;; TYPE rational-moment -> rational-angle
  ;; Solar longitude at moment $tee$.
  (hindu-true-position tee hindu-sidereal-year
                       14/360 hindu-anomalistic-year 1/42))

(defun hindu-zodiac (tee)
  ;; TYPE rational-moment -> hindu-solar-month
  ;; Zodiacal sign of the sun, as integer in range 1..12,
  ;; at moment $tee$.
  (1+ (quotient (hindu-solar-longitude tee) (deg 30))))

(defun hindu-lunar-longitude (tee)
  ;; TYPE rational-moment -> rational-angle
  ;; Lunar longitude at moment $tee$.
  (hindu-true-position tee hindu-sidereal-month
                       32/360 hindu-anomalistic-month 1/96))

(defun hindu-lunar-phase (tee)
  ;; TYPE rational-moment -> rational-angle
  ;; Longitudinal distance between the sun and moon
  ;; at moment $tee$.
  (mod (- (hindu-lunar-longitude tee)
          (hindu-solar-longitude tee))
       360))

(defun hindu-lunar-day-from-moment (tee)
  ;; TYPE rational-moment -> hindu-lunar-day
  ;; Phase of moon (tithi) at moment $tee$, as an integer in
  ;; the range 1..30.
  (1+ (quotient (hindu-lunar-phase tee) (deg 12))))

(defun hindu-new-moon-before (tee)
  ;; TYPE rational-moment -> rational-moment
  ;; Approximate moment of last new moon preceding moment
  ;; $tee$, close enough to determine zodiacal sign.
  (let* ((varepsilon (expt 2 -1000)) ; Safety margin.
         (tau  ; Can be off by almost a day.
          (- tee (* (/ 1 (deg 360)) (hindu-lunar-phase tee)
                    hindu-synodic-month))))
    (binary-search ; Search for phase start.
     l (1- tau)
     u (min tee (1+ tau))
     x (< (hindu-lunar-phase x) (deg 180))
     (or (= (hindu-zodiac l) (hindu-zodiac u))
         (< (- u l) varepsilon)))))

(defun hindu-lunar-day-at-or-after (k tee)
  ;; TYPE (rational rational-moment) -> rational-moment
  ;; Time lunar-day (tithi) number $k$ begins at or after
  ;; moment $tee$.  $k$ can be fractional (for karanas).
  (let* ((phase ; Degrees corresponding to k.
          (* (1- k) (deg 12)))
         (tau ; Mean occurrence of lunar-day.
          (+ tee (* (/ 1 (deg 360))
                    (mod (- phase (hindu-lunar-phase tee))
                         360)
                    hindu-synodic-month)))
         (a (max tee (- tau 2)))
         (b (+ tau 2)))
    (invert-angular hindu-lunar-phase phase a b)))
   
(defun hindu-calendar-year (tee)
  ;; TYPE rational-moment -> hindu-solar-year
  ;; Determine solar year at given moment $tee$.
  (round (- (/ (- tee hindu-epoch)
               hindu-sidereal-year)
            (/ (hindu-solar-longitude tee)
               (deg 360)))))

(defconstant hindu-solar-era
  ;; TYPE standard-year
  ;; Years from Kali Yuga until Saka era.
  3179)

(defun hindu-solar-from-fixed (date)
  ;; TYPE fixed-date -> hindu-solar-date
  ;; Hindu (Orissa) solar date equivalent to fixed $date$.
  (let* ((critical    ; Sunrise on Hindu date.
          (hindu-sunrise (1+ date)))
         (month (hindu-zodiac critical))
         (year (- (hindu-calendar-year critical)
                  hindu-solar-era))
         (approx ; 3 days before start of mean month.
          (- date 3
             (mod (floor (hindu-solar-longitude critical))
                  (deg 30))))
         (begin ; Search forward for beginning...
          (next i approx ; ... of month.
                (= (hindu-zodiac (hindu-sunrise (1+ i)))
                   month)))
         (day (- date begin -1)))
    (hindu-solar-date year month day)))

(defun fixed-from-hindu-solar (s-date)
  ;; TYPE hindu-solar-date -> fixed-date
  ;; Fixed date corresponding to Hindu solar date $s-date$
  ;; (Saka era; Orissa rule.)
  (let* ((month (standard-month s-date))
         (day (standard-day s-date))
         (year (standard-year s-date))
         (begin ; Approximate start of month
                ; by adding days...
          (+ (floor (* (+ year hindu-solar-era
                          (/ (1- month) 12))   ; in months...
                       hindu-sidereal-year))   ; ... and years
             hindu-epoch)))   ; and days before RD 0.
    ;; Search forward to correct month
    (+ day -1
       (next d (- begin 3)
             (= (hindu-zodiac (hindu-sunrise (1+ d)))
                month)))))

(defconstant hindu-lunar-era
  ;; TYPE standard-year
  ;; Years from Kali Yuga until Vikrama era.
  3044)

(defun hindu-lunar-from-fixed (date)
  ;; TYPE fixed-date -> hindu-lunar-date
  ;; Hindu lunar date, new-moon scheme, 
  ;; equivalent to fixed $date$.
  (let* ((critical (hindu-sunrise date)) ; Sunrise that day.
          (day (hindu-lunar-day-from-moment
                critical)); Day of month.
          (leap-day        ; If previous day the same.
           (= day (hindu-lunar-day-from-moment
                   (hindu-sunrise (- date 1)))))
          (last-new-moon
           (hindu-new-moon-before critical))
          (next-new-moon
           (hindu-new-moon-before
            (+ (floor last-new-moon) 35)))
          (solar-month         ; Solar month name.
           (hindu-zodiac last-new-moon))
          (leap-month       ; If begins and ends in same sign.
           (= solar-month (hindu-zodiac next-new-moon)))
          (month                     ; Month of lunar year.
           (amod (1+ solar-month) 12))
          (year ; Solar year at end of month.
           (- (hindu-calendar-year
               (if (<= month 2) ; $date$ might precede solar
                                ; new year.
                   (+ date 180)
                 date))
              hindu-lunar-era)))
     (hindu-lunar-date year month leap-month day leap-day)))

(defun fixed-from-hindu-lunar (l-date)
  ;; TYPE hindu-lunar-date -> fixed-date
  ;; Fixed date corresponding to Hindu lunar date $l-date$.
  (let* ((year (hindu-lunar-year l-date))
         (month (hindu-lunar-month l-date))
         (leap-month (hindu-lunar-leap-month l-date))
         (day (hindu-lunar-day l-date))
         (leap-day (hindu-lunar-leap-day l-date))
         (approx
          (+ hindu-epoch
             (* hindu-sidereal-year
                (+ year hindu-lunar-era
                   (/ (1- month) 12)))))
         (s (floor
             (- approx
                (* (/ 1 (deg 360)) hindu-sidereal-year
                   (- (mod (- (hindu-solar-longitude approx)
                              (* (1- month) (deg 30))
                              (- (deg 180)))
                           360)
                      (deg 180))))))
         (k (hindu-lunar-day-from-moment (+ s (hr 6))))
         (est
          (- s (- day)
             (cond
              ((< 3 k 27) ; Not borderline case.
               k)
              ((let* ((mid ; Middle of preceding solar month.
                       (hindu-lunar-from-fixed
                        (- s 15))))
                 (or ; In month starting near $s$.
                  (/= (hindu-lunar-month mid) month) 
                  (and (hindu-lunar-leap-month mid)
                       (not leap-month))))
               (- (mod (+ k 15) 30) 15))
              (t ; In preceding month.
               (+ (mod (- k 15) 30) 15)))))
         (tau ; Refined estimate.
          (- est (mod (- (hindu-lunar-day-from-moment
                          (+ est (hr 6)))
                         day -15)
                      30)
             -15))
         (date (next d (1- tau)
                     (member (hindu-lunar-day-from-moment
                              (hindu-sunrise d))
                             (list day (amod (1+ day) 30))))))
    (if leap-day (1+ date) date)))

(defun hindu-equation-of-time (date)
  ;; TYPE fixed-date -> rational-moment
  ;; Time from true to mean midnight of $date$.
  ;; (This is a gross approximation to the correct value.)
  (let* ((offset (hindu-sine
                  (hindu-mean-position
                   date
                   hindu-anomalistic-year)))
         (equation-sun ; Sun's equation of center
              ; Arcsin is not needed since small
          (* offset (angle 57 18 0)
             (- 14/360 (/ (abs offset) 1080)))))
    (* (/ (hindu-daily-motion date) (deg 360))
       (/ equation-sun (deg 360))
       hindu-sidereal-year)))

(defun hindu-ascensional-difference (date locale)
  ;; TYPE (fixed-date location) -> rational-angle
  ;; Difference between right and oblique ascension
  ;; of sun on $date$ at $locale$.
  (let* ((sin_delta
          (* 1397/3438 ; Sine of inclination.
             (hindu-sine (hindu-tropical-longitude date))))
         (phi (latitude locale))
         (diurnal-radius
          (hindu-sine (+ (deg 90) (hindu-arcsin sin_delta))))
         (tan_phi ; Tangent of latitude as rational number.
          (/ (hindu-sine phi)
             (hindu-sine (+ (deg 90) phi))))
         (earth-sine (* sin_delta tan_phi)))
    (hindu-arcsin (- (/ earth-sine diurnal-radius)))))

(defun hindu-tropical-longitude (date)
  ;; TYPE fixed-date -> rational-angle
  ;; Hindu tropical longitude on fixed $date$.
  ;; Assumes precession with maximum of 27 degrees
  ;; and period of 7200 sidereal years
  ;; (= 1577917828/600 days).
  (let* ((days (floor (- date hindu-epoch))) ; Whole days.
         (precession
          (- (deg 27)
             (abs
              (- (deg 54)
                 (mod (+ (deg 27)
                         (* (deg 108) 600/1577917828 days))
                      108))))))
    (mod (- (hindu-solar-longitude date) precession)
         360)))

(defun hindu-rising-sign (date)
  ;; TYPE fixed-date -> rational-amplitude
  ;; Tabulated speed of rising of current zodiacal sign on
  ;; $date$.
  (let* ((i  ; Index.
          (quotient (hindu-tropical-longitude date)
                    (deg 30))))
    (nth (mod i 6)
         (list 1670/1800 1795/1800 1935/1800 1935/1800
               1795/1800 1670/1800))))

(defun hindu-daily-motion (date)
  ;; TYPE fixed-date -> rational-angle
  ;; Sidereal daily motion of sun on $date$.
  (let* ((mean-motion ; Mean daily motion in degrees.
          (/ (deg 360) hindu-sidereal-year))
         (anomaly
          (hindu-mean-position date hindu-anomalistic-year))
         (epicycle ; Current size of epicycle.
          (- 14/360 (/ (abs (hindu-sine anomaly)) 1080)))
         (entry (quotient anomaly (angle 0 225 0)))
         (sine-table-step ; Marginal change in anomaly
          (- (hindu-sine-table (1+ entry))
             (hindu-sine-table entry)))
         (factor
          (* -3438/225 sine-table-step epicycle)))
    (* mean-motion (1+ factor))))

(defun hindu-solar-sidereal-difference (date)
  ;; TYPE fixed-date -> rational-angle
  ;; Difference between solar and sidereal day on $date$.
  (* (hindu-daily-motion date) (hindu-rising-sign date)))

(defconstant ujjain
  ;; TYPE location
  ;; Location of Ujjain.
  (location (angle 23 9 0) (angle 75 46 6)
            (mt 0) (hr (+ 5 461/9000))))

(defconstant hindu-locale
  ;; TYPE location
  ;; Location (Ujjain) for determining Hindu calendar.
  ujjain)

(defun hindu-sunrise (date)
  ;; TYPE fixed-date -> rational-moment
  ;; Sunrise at hindu-locale on $date$.
  (+ date (hr 6) ; Mean sunrise.
     (/ (- (longitude ujjain) (longitude hindu-locale))
        (deg 360)) ; Difference from longitude.
     (- (hindu-equation-of-time date)) ; Apparent midnight.
     (* ; Convert sidereal angle to fraction of civil day.
      (/ 1577917828/1582237828 (deg 360))
      (+ (hindu-ascensional-difference date hindu-locale)
         (* 1/4 (hindu-solar-sidereal-difference date))))))

(defun hindu-fullmoon-from-fixed (date)
  ;; TYPE fixed-date -> hindu-lunar-date
  ;; Hindu lunar date, full-moon scheme, 
  ;; equivalent to fixed $date$.
  (let* ((l-date (hindu-lunar-from-fixed date))
         (year (hindu-lunar-year l-date))
         (month (hindu-lunar-month l-date))
         (leap-month (hindu-lunar-leap-month l-date))
         (day (hindu-lunar-day l-date))
         (leap-day (hindu-lunar-leap-day l-date))
         (m (if (>= day 16)
                (hindu-lunar-month
                 (hindu-lunar-from-fixed (+ date 20)))
              month)))
    (hindu-lunar-date year m leap-month day leap-day)))

(defun hindu-expunged? (l-month l-year)
  ;; TYPE (hindu-lunar-month hindu-lunar-year) ->
  ;; TYPE boolean
  ;; True of Hindu lunar month $l-month$ in $l-year$
  ;; is expunged.
  (/= l-month
      (hindu-lunar-month
       (hindu-lunar-from-fixed
        (fixed-from-hindu-lunar
         (list l-year l-month false 15 false))))))

(defun fixed-from-hindu-fullmoon (l-date)
  ;; TYPE hindu-lunar-date -> fixed-date
  ;; Fixed date equivalent to Hindu lunar $l-date$
  ;; in full-moon scheme.
  (let* ((year (hindu-lunar-year l-date))
         (month (hindu-lunar-month l-date))
         (leap-month (hindu-lunar-leap-month l-date))
         (day (hindu-lunar-day l-date))
         (leap-day (hindu-lunar-leap-day l-date))
         (m (cond ((or leap-month (<= day 15))
                   month)
                  ((hindu-expunged? (amod (1- month) 12) year)
                   (amod (- month 2) 12))
                  (t (amod (1- month) 12)))))
    (fixed-from-hindu-lunar
     (hindu-lunar-date year m leap-month day leap-day))))
         
(defun alt-hindu-sunrise (date)
  ;; TYPE fixed-date -> rational-moment
  ;; Astronomical sunrise at Hindu locale on $date$,
  ;; per Lahiri,
  ;; rounded to nearest minute, as a rational number.
  (let* ((rise (dawn date hindu-locale (angle 0 47 0))))
    (* 1/24 1/60 (round (* rise 24 60)))))

(defun hindu-sunset (date)
  ;; TYPE fixed-date -> rational-moment
  ;; Sunset at hindu-locale on $date$.
  (+ date (hr 18) ; Mean sunset.
     (/ (- (longitude ujjain) (longitude hindu-locale))
        (deg 360)) ; Difference from longitude.
     (- (hindu-equation-of-time date)) ; Apparent midnight.
     (* ; Convert sidereal angle to fraction of civil day.
      (/ 1577917828/1582237828 (deg 360))
      (+ (- (hindu-ascensional-difference date hindu-locale))
         (* 3/4 (hindu-solar-sidereal-difference date))))))

(defun hindu-sundial-time (tee)
  ;; TYPE rational-moment -> rational-moment
  ;; Hindu local time of temporal moment $tee$.
  (let* ((date (fixed-from-moment tee))
         (time (mod tee 1))
         (q (floor (* 4 time))) ; quarter of day
         (a (cond ((= q 0)    ; early this morning
                   (hindu-sunset (1- date)))
                  ((= q 3)    ; this evening
                   (hindu-sunset date))
                  (t ;  daytime today
                   (hindu-sunrise date))))
         (b (cond ((= q 0) (hindu-sunrise date))
                  ((= q 3) (hindu-sunrise (1+ date)))
                  (t (hindu-sunset date)))))
    (+ a (* 2 (- b a) (- time
                         (cond ((= q 3) (hr 18))
                               ((= q 0) (hr -6))
                               (t (hr 6))))))))

(defun ayanamsha (tee)
  ;; TYPE moment -> angle
  ;; Difference between tropical and sidereal solar longitude.
  (- (solar-longitude tee)
     (sidereal-solar-longitude tee)))

(defun astro-hindu-sunset (date)
  ;; TYPE fixed-date -> moment
  ;; Geometrical sunset at Hindu locale on $date$.
  (dusk date hindu-locale (deg 0)))

(defun sidereal-zodiac (tee)
  ;; TYPE moment -> solar-month
  ;; Sidereal zodiacal sign of the sun, as integer in range
  ;; 1..12, at moment $tee$.
  (1+ (quotient (sidereal-solar-longitude tee) (deg 30))))

(defun astro-hindu-calendar-year (tee)
  ;; TYPE moment -> hindu-solar-year
  ;; Astronomical Hindu solar year KY at given moment $tee$.
  (round (- (/ (- tee hindu-epoch)
               mean-sidereal-year)
            (/ (sidereal-solar-longitude tee)
               (deg 360)))))

(defun astro-hindu-solar-from-fixed (date)
  ;; TYPE fixed-date -> hindu-solar-date
  ;; Astronomical Hindu (Tamil) solar date equivalent to
  ;; fixed $date$.
  (let* ((critical    ; Sunrise on Hindu date.
          (astro-hindu-sunset date))
         (month (sidereal-zodiac critical))
         (year (- (astro-hindu-calendar-year critical)
                  hindu-solar-era))
         (approx ; 3 days before start of mean month.
          (- date 3
             (mod (floor (sidereal-solar-longitude critical))
                  (deg 30))))
         (begin ; Search forward for beginning...
          (next i approx ; ... of month.
                (= (sidereal-zodiac (astro-hindu-sunset i))
                   month)))
         (day (- date begin -1)))
    (hindu-solar-date year month day)))

(defun fixed-from-astro-hindu-solar (s-date)
  ;; TYPE hindu-solar-date -> fixed-date
  ;; Fixed date corresponding to Astronomical 
  ;; Hindu solar date (Tamil rule; Saka era).
  (let* ((month (standard-month s-date))
         (day (standard-day s-date))
         (year (standard-year s-date))
         (approx ; 3 days before start of mean month.
           (+ hindu-epoch -3
              (floor (* (+ (+ year hindu-solar-era)
                           (/ (1- month) 12))
                        mean-sidereal-year))))
         (begin ; Search forward for beginning...
          (next i approx ; ... of month.
                (= (sidereal-zodiac (astro-hindu-sunset i))
                   month))))
    (+ begin day -1)))

(defun astro-lunar-day-from-moment (tee)
  ;; TYPE moment -> hindu-lunar-day
  ;; Phase of moon (tithi) at moment $tee$, as an integer in
  ;; the range 1..30.
  (1+ (quotient (lunar-phase tee) (deg 12))))

(defun astro-hindu-lunar-from-fixed (date)
  ;; TYPE fixed-date -> hindu-lunar-date
  ;; Astronomical Hindu lunar date equivalent to fixed $date$.
  (let* ((critical
          (alt-hindu-sunrise date)) ; Sunrise that day.
         (day
          (astro-lunar-day-from-moment critical)); Day of month
         (leap-day             ; If previous day the same.
          (= day (astro-lunar-day-from-moment 
                  (alt-hindu-sunrise (- date 1)))))
         (last-new-moon
          (new-moon-before critical))
         (next-new-moon
          (new-moon-at-or-after critical))
         (solar-month         ; Solar month name.
          (sidereal-zodiac last-new-moon))
         (leap-month       ; If begins and ends in same sign.
          (= solar-month (sidereal-zodiac next-new-moon)))
         (month                     ; Month of lunar year.
          (amod (1+ solar-month) 12))
         (year ; Solar year at end of month.
          (- (astro-hindu-calendar-year
              (if (<= month 2) ; $date$ might precede solar
                               ; new year.
                  (+ date 180)
                date))
             hindu-lunar-era)))
    (hindu-lunar-date year month leap-month day leap-day)))


(defun fixed-from-astro-hindu-lunar (l-date)
  ;; TYPE hindu-lunar-date -> fixed-date
  ;; Fixed date corresponding to Hindu lunar date $l-date$.
  (let* ((year (hindu-lunar-year l-date))
         (month (hindu-lunar-month l-date))
         (leap-month (hindu-lunar-leap-month l-date))
         (day (hindu-lunar-day l-date))
         (leap-day (hindu-lunar-leap-day l-date))
         (approx
          (+ hindu-epoch
             (* mean-sidereal-year
                (+ year hindu-lunar-era
                   (/ (1- month) 12)))))
         (s (floor
             (- approx
                (* (/ 1 (deg 360)) mean-sidereal-year
                   (- (mod (- (sidereal-solar-longitude approx)
                              (* (1- month) (deg 30))
                              (- (deg 180)))
                           360)
                      (deg 180))))))
         (k (astro-lunar-day-from-moment (+ s (hr 6))))
         (est
          (- s (- day)
             (cond
              ((< 3 k 27) ; Not borderline case.
               k)
              ((let* ((mid ; Middle of preceding solar month.
                       (astro-hindu-lunar-from-fixed
                        (- s 15))))
                 (or ; In month starting near $s$.
                  (/= (hindu-lunar-month mid) month) 
                  (and (hindu-lunar-leap-month mid)
                       (not leap-month))))
               (- (mod (+ k 15) 30) 15))
              (t ; In preceding month.
               (+ (mod (- k 15) 30) 15)))))
         (tau ; Refined estimate.
          (- est (mod (- (astro-lunar-day-from-moment
                          (+ est (hr 6)))
                         day -15)
                      30)
             -15))
         (date (next d (1- tau)
                     (member (astro-lunar-day-from-moment
                              (alt-hindu-sunrise d))
                             (list day (amod (1+ day) 30))))))
    (if leap-day (1+ date) date)))

(defun hindu-lunar-station (date)
  ;; TYPE fixed-date -> nakshatra
  ;; Hindu lunar station (nakshatra) at sunrise on $date$.
  (let* ((critical (hindu-sunrise date)))
    (1+ (quotient (hindu-lunar-longitude critical) 
                  (angle 0 800 0)))))

(defun hindu-solar-longitude-at-or-after (lambda tee)
  ;; TYPE (season moment) -> moment
  ;; Moment of the first time at or after $tee$
  ;; when Hindu solar longitude will be $lambda$ degrees.
  (let* ((tau ; Estimate (within 5 days).
          (+ tee
             (* hindu-sidereal-year (/ 1 (deg 360))
                (mod (- lambda (hindu-solar-longitude tee))
                     360))))
         (a (max tee (- tau 5))) ; At or after tee.
         (b (+ tau 5)))
    (invert-angular hindu-solar-longitude lambda a b)))

(defun mesha-samkranti (g-year)
  ;; TYPE gregorian-year -> rational-moment
  ;; Fixed moment of Mesha samkranti (Vernal equinox)
  ;; in Gregorian $g-year$.
  (let* ((jan1 (gregorian-new-year g-year)))
    (hindu-solar-longitude-at-or-after (deg 0) jan1)))

(defconstant sidereal-start
  ;; TYPE angle
  (precession (universal-from-local
                (mesha-samkranti (ce 285))
                hindu-locale)))

(defun hindu-lunar-new-year (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of Hindu lunisolar new year in Gregorian
  ;; $g-year$.
  (let* ((jan1 (gregorian-new-year g-year))
         (mina ; Fixed moment of solar longitude 330.
          (hindu-solar-longitude-at-or-after (deg 330) jan1))
         (new-moon ; Next new moon.
          (hindu-lunar-day-at-or-after 1 mina))
         (h-day (floor new-moon))
         (critical ; Sunrise that day.
          (hindu-sunrise h-day)))
    (+ h-day
       ;; Next day if new moon after sunrise,
       ;; unless lunar day ends before next sunrise.
       (if (or (< new-moon critical)
               (= (hindu-lunar-day-from-moment
                   (hindu-sunrise (1+ h-day))) 2))
           0 1))))

(defun hindu-lunar-on-or-before? (l-date1 l-date2)
  ;; TYPE (hindu-lunar-date hindu-lunar-date) -> boolean
  ;; True if Hindu lunar date $l-date1$ is on or before
  ;; Hindu lunar date $l-date2$.
  (let* ((month1 (hindu-lunar-month l-date1))
         (month2 (hindu-lunar-month l-date2))
         (leap1 (hindu-lunar-leap-month l-date1))
         (leap2 (hindu-lunar-leap-month l-date2))
         (day1 (hindu-lunar-day l-date1))
         (day2 (hindu-lunar-day l-date2))
         (leap-day1 (hindu-lunar-leap-day l-date1))
         (leap-day2 (hindu-lunar-leap-day l-date2))
         (year1 (hindu-lunar-year l-date1))
         (year2 (hindu-lunar-year l-date2)))
    (or (< year1 year2)
        (and (= year1 year2)
             (or (< month1 month2)
                 (and (= month1 month2)
                      (or (and leap1 (not leap2))
                          (and (equal leap1 leap2)
                               (or (< day1 day2)
                                   (and (= day1 day2)
                                        (or (not leap-day1)
                                            leap-day2)))))
                      ))))))

(defun hindu-date-occur (l-month l-day l-year)
  ;; TYPE (hindu-lunar-month hindu-lunar-day
  ;; TYPE  hindu-lunar-year) -> fixed-date
  ;; Fixed date of occurrence of Hindu lunar $l-month$,
  ;; $l-day$ in Hindu lunar year $l-year$, taking leap and
  ;; expunged days into account.  When the month is
  ;; expunged, then the following month is used.
  (let* ((lunar (hindu-lunar-date l-year l-month false
                                  l-day false))
         (try (fixed-from-hindu-lunar lunar))
         (mid (hindu-lunar-from-fixed
               (if (> l-day 15) (- try 5) try)))
         (expunged? (/= l-month (hindu-lunar-month mid)))
         (l-date ; day in next month
          (hindu-lunar-date (hindu-lunar-year mid)
                            (hindu-lunar-month mid)
                            (hindu-lunar-leap-month mid)
                            l-day false)))
    (cond (expunged?
           (1- (next d try
                     (not
                      (hindu-lunar-on-or-before?
                       (hindu-lunar-from-fixed d) l-date)))))
          ((/= l-day (hindu-lunar-day
                      (hindu-lunar-from-fixed try)))
           (1- try))
          (t try))))

(defun hindu-lunar-holiday (l-month l-day g-year)
  ;; TYPE (hindu-lunar-month hindu-lunar-day
  ;; TYPE  gregorian-year) -> list-of-fixed-dates
  ;; List of fixed dates of occurrences of Hindu lunar
  ;; $month$, $day$ in Gregorian year $g-year$.
  (let* ((l-year (hindu-lunar-year
                  (hindu-lunar-from-fixed
                   (gregorian-new-year g-year))))
         (date1 (hindu-date-occur l-month l-day l-year))
         (date2 (hindu-date-occur l-month l-day (1+ l-year))))
    (list-range (list date1 date2)
                (gregorian-year-range g-year))))

(defun diwali (g-year)
  ;; TYPE gregorian-year -> list-of-fixed-dates
  ;; List of fixed date(s) of Diwali in Gregorian year
  ;; $g-year$.
  (hindu-lunar-holiday 8 1 g-year))

(defun hindu-tithi-occur (l-month tithi tee l-year)
  ;; TYPE (hindu-lunar-month rational rational
  ;; TYPE hindu-lunar-year) -> fixed-date
  ;; Fixed date of occurrence of Hindu lunar $tithi$ prior
  ;; to sundial time $tee$, in Hindu lunar $l-month$,
  ;; $l-year$.
  (let* ((approx
          (hindu-date-occur l-month (floor tithi) l-year))
         (lunar
          (hindu-lunar-day-at-or-after tithi (- approx 2)))
         (try (fixed-from-moment lunar))
         (tee_h (standard-from-sundial (+ try tee) ujjain)))
    (if (or (<= lunar tee_h)
            (> (hindu-lunar-phase
                (standard-from-sundial (+ try 1 tee) ujjain))
               (* 12 tithi)))
        try
      (1+ try))))

(defun hindu-lunar-event (l-month tithi tee g-year)
  ;; TYPE (hindu-lunar-month rational rational
  ;; TYPE gregorian-year) -> list-of-fixed-dates
  ;; List of fixed dates of occurrences of Hindu lunar $tithi$
  ;; prior to sundial time $tee$, in Hindu lunar $l-month$,
  ;; in Gregorian year $g-year$.
  (let* ((l-year (hindu-lunar-year
                  (hindu-lunar-from-fixed
                   (gregorian-new-year g-year))))
         (date1 (hindu-tithi-occur l-month tithi tee l-year))
         (date2 (hindu-tithi-occur
                 l-month tithi tee (1+ l-year))))
    (list-range (list date1 date2)
                (gregorian-year-range g-year))))

(defun shiva (g-year)
  ;; TYPE gregorian-year -> list-of-fixed-dates
  ;; List of fixed date(s) of Night of Shiva in Gregorian
  ;; year $g-year$.
  (hindu-lunar-event 11 29 (hr 24) g-year))
 
(defun rama (g-year)
  ;; TYPE gregorian-year -> list-of-fixed-dates
  ;; List of fixed date(s) of Rama's Birthday in Gregorian
  ;; year $g-year$.
  (hindu-lunar-event 1 9 (hr 12) g-year))
 
(defun karana (n)
  ;; TYPE {1-60} -> {0-10}
  ;; Number (0-10) of the name of the $n$-th (1-60) Hindu
  ;; karana.
  (cond ((= n 1) 0)
        ((> n 57) (- n 50))
        (t (amod (1- n) 7))))

(defun yoga (date)
  ;; TYPE fixed-date -> {1-27}
  ;; Hindu yoga on $date$.
  (1+ (floor (mod (/ (+ (hindu-solar-longitude date)
                        (hindu-lunar-longitude date))
                     (angle 0 800 0))
                  27))))

(defun sacred-wednesdays (g-year)
  ;; TYPE gregorian-year -> list-of-fixed-dates
  ;; List of Wednesdays in Gregorian year $g-year$
  ;; that are day 8 of Hindu lunar months.
  (sacred-wednesdays-in-range
   (gregorian-year-range g-year)))

(defun sacred-wednesdays-in-range (range)
  ;; TYPE range -> list-of-fixed-dates
  ;; List of Wednesdays within $range$ of dates
  ;; that are day 8 of Hindu lunar months.
  (let* ((a (start range))
         (b (end range))
         (wed (kday-on-or-after wednesday a))
         (h-date (hindu-lunar-from-fixed wed)))
    (if (in-range? wed range)
        (append
         (if (= (hindu-lunar-day h-date) 8)
             (list wed)
           nil)
         (sacred-wednesdays-in-range
          (interval (1+ wed) b)))
      nil)))


;;;; Section: Tibetan Calendar

(defun tibetan-date (year month leap-month day leap-day)
  ;; TYPE (tibetan-year tibetan-month
  ;; TYPE  tibetan-leap-month tibetan-day
  ;; TYPE  tibetan-leap-day) -> tibetan-date
  (list year month leap-month day leap-day))

(defun tibetan-month (date)
  ;; TYPE tibetan-date -> tibetan-month
  (second date))

(defun tibetan-leap-month (date)
  ;; TYPE tibetan-date -> tibetan-leap-month
  (third date))

(defun tibetan-day (date)
  ;; TYPE tibetan-date -> tibetan-day
  (fourth date))

(defun tibetan-leap-day (date)
  ;; TYPE tibetan-date -> tibetan-leap-day
  (fifth date))

(defun tibetan-year (date)
  ;; TYPE tibetan-date -> tibetan-year
  (first date))

(defconstant tibetan-epoch
  ;; TYPE fixed-date
  (fixed-from-gregorian (gregorian-date -127 december 7)))

(defun tibetan-sun-equation (alpha) 
  ;; TYPE rational-angle -> rational
  ;; Interpolated tabular sine of solar anomaly $alpha$.
  (cond ((> alpha 6) (- (tibetan-sun-equation (- alpha 6))))
        ((> alpha 3) (tibetan-sun-equation (- 6 alpha)))
        ((integerp alpha)
         (nth alpha (list 0 6/60 10/60 11/60)))
        (t (+ (* (mod alpha 1)
                 (tibetan-sun-equation (ceiling alpha)))
              (* (mod (- alpha) 1)
                 (tibetan-sun-equation (floor alpha)))))))

(defun tibetan-moon-equation (alpha) 
  ;; TYPE rational-angle -> rational
  ;; Interpolated tabular sine of lunar anomaly $alpha$.
  (cond ((> alpha 14) (- (tibetan-moon-equation (- alpha 14))))
        ((> alpha 7) (tibetan-moon-equation (- 14 alpha)))
        ((integerp alpha)
         (nth alpha
              (list 0 5/60 10/60 15/60 19/60 22/60 24/60
                    25/60)))
        (t (+ (* (mod alpha 1) 
                 (tibetan-moon-equation (ceiling alpha)))
              (* (mod (- alpha) 1) 
                 (tibetan-moon-equation (floor alpha)))))))

(defun fixed-from-tibetan (t-date)
  ;; TYPE tibetan-date -> fixed-date
  ;; Fixed date corresponding to Tibetan lunar date $t-date$. 
  (let* ((year (tibetan-year t-date))
         (month (tibetan-month t-date))
         (leap-month (tibetan-leap-month t-date))
         (day (tibetan-day t-date))
         (leap-day (tibetan-leap-day t-date))
         (months ; Lunar month count.
          (floor (+ (* 804/65 (1- year)) (* 67/65 month)
                    (if leap-month -1 0) 64/65)))
         (days ; Lunar day count.
          (+ (* 30 months) day))
         (mean ; Mean civil days since epoch.
          (+ (* days 11135/11312) -30
             (if leap-day 0 -1) 1071/1616))
         (solar-anomaly 
          (mod (+ (* days 13/4824) 2117/4824) 1))
         (lunar-anomaly
          (mod (+ (* days 3781/105840) 2837/15120) 1))
         (sun (- (tibetan-sun-equation (* 12 solar-anomaly))))
         (moon (tibetan-moon-equation (* 28 lunar-anomaly))))
    (floor (+ tibetan-epoch mean sun moon))))

(defun tibetan-from-fixed (date)
  ;; TYPE fixed-date -> tibetan-date
  ;; Tibetan lunar date corresponding to fixed $date$.
  (let* ((cap-Y (+ 365 4975/18382)) ; Average Tibetan year.
         (years (ceiling (/ (- date tibetan-epoch) cap-Y)))
         (year0 ; Search for year.
          (final y years
                 (>= date
                     (fixed-from-tibetan
                      (tibetan-date y 1 false 1 false)))))
         (month0 ; Search for month.
          (final m 1
                 (>= date
                     (fixed-from-tibetan
                      (tibetan-date year0 m false 1 false)))))
         (est ; Estimated day.
          (- date (fixed-from-tibetan
                   (tibetan-date year0 month0 false 1 false))))
         (day0 ; Search for day.
          (final
           d (- est 2)
           (>= date
               (fixed-from-tibetan
                (tibetan-date year0 month0 false d false)))))
         (leap-month (> day0 30))
         (day (amod day0 30))
         (month (amod (cond ((> day day0) (1- month0))
                            (leap-month (1+ month0))
                            (t month0))
                      12))
         (year (cond ((and (> day day0) (= month0 1)) 
                      (1- year0))
                     ((and leap-month (= month0 12)) 
                      (1+ year0))
                     (t year0)))
         (leap-day
          (= date
             (fixed-from-tibetan
              (tibetan-date year month leap-month day true)))))
    (tibetan-date year month leap-month day leap-day)))

(defun tibetan-leap-month? (t-month t-year)
  ;; TYPE (tibetan-month tibetan-year) -> boolean
  ;; True if $t-month$ is leap in Tibetan year $t-year$.
  (= t-month
     (tibetan-month
      (tibetan-from-fixed
       (fixed-from-tibetan
        (tibetan-date t-year t-month true 2 false))))))

(defun losar (t-year)
  ;; TYPE tibetan-year -> fixed-date
  ;; Fixed date of Tibetan New Year (Losar)
  ;; in Tibetan year $t-year$.
  (let* ((t-leap (tibetan-leap-month? 1 t-year)))
    (fixed-from-tibetan
     (tibetan-date t-year 1 t-leap 1 false))))

(defun tibetan-new-year (g-year)
  ;; TYPE gregorian-year -> list-of-fixed-dates
  ;; List of fixed dates of Tibetan New Year in
  ;; Gregorian year $g-year$.
  (let* ((dec31 (gregorian-year-end g-year))
         (t-year (tibetan-year (tibetan-from-fixed dec31))))
    (list-range
     (list (losar (1- t-year))
           (losar t-year))
     (gregorian-year-range g-year))))


;;;; Section: Astronomical Lunar Calendars

(defun visible-crescent (date locale)
  ;; TYPE (fixed-date location) -> boolean
  ;; S. K. Shaukat's criterion for likely
  ;; visibility of crescent moon on eve of $date$ at $locale$.
  (let* ((tee ; Best viewing time prior evening.
          (universal-from-standard
           (dusk (1- date) locale (deg 4.5L0))
           locale))
         (phase (lunar-phase tee))
         (altitude (lunar-altitude tee locale))
         (arc-of-light ; Angular separation of ...
          (arccos-degrees ; ... sun and moon.
           (* (cosine-degrees (lunar-latitude tee))
              (cosine-degrees phase)))))
    (and (< new phase first-quarter)
         (<= (deg 10.6L0) arc-of-light (deg 90))
         (> altitude (deg 4.1L0)))))

(defun phasis-on-or-before (date locale)
  ;; TYPE (fixed-date location) -> fixed-date
  ;; Closest fixed date on or before $date$ when crescent
  ;; moon first became visible at $locale$.
  (let* ((mean ; Mean date of prior new moon.
          (- date
             (floor (* (/ (lunar-phase (1+ date)) (deg 360))
                       mean-synodic-month))))
         (tau ; Check if not visible yet on $date$.
          (if (and (<= (- date mean) 3)
                   (not (visible-crescent date locale)))
            (- mean 30) ; Must go back a month.
            (- mean 2))))
    (next d tau (visible-crescent d locale))))

(defconstant islamic-locale
  ;; TYPE location
  ;; Sample location for Observational Islamic calendar
  ;; (Cairo, Egypt).
  (location (deg 30.1L0) (deg 31.3L0) (mt 200) (hr 2)))

(defun fixed-from-observational-islamic (i-date)
  ;; TYPE islamic-date -> fixed-date
  ;; Fixed date equivalent to Observational Islamic date
  ;; $i-date$.
  (let* ((month (standard-month i-date))
         (day (standard-day i-date))
         (year (standard-year i-date))
         (midmonth ; Middle of given month.
          (+ islamic-epoch
             (floor (* (+ (* (1- year) 12)
                          month -1/2)
                       mean-synodic-month)))))
    (+ (phasis-on-or-before ; First day of month.
        midmonth islamic-locale)
       day -1)))

(defun observational-islamic-from-fixed (date)
  ;; TYPE fixed-date -> islamic-date
  ;; Observational Islamic date (year month day)
  ;; corresponding to fixed $date$.
  (let* ((crescent ; Most recent new moon.
          (phasis-on-or-before date islamic-locale))
         (elapsed-months
          (round (/ (- crescent islamic-epoch)
                    mean-synodic-month)))
         (year (1+ (quotient elapsed-months 12)))
         (month (1+ (mod elapsed-months 12)))
         (day (1+ (- date crescent))))
    (islamic-date year month day)))

(defconstant jerusalem
  ;; TYPE location
  ;; Location of Jerusalem.
  (location (deg 31.8L0) (deg 35.2L0) (mt 800) (hr 2)))

(defun astronomical-easter (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Date of (proposed) astronomical Easter in Gregorian
  ;; year $g-year$.
  (let* ((jan1 ; Beginning of year.
          (gregorian-new-year g-year))
         (equinox ; Spring equinox.
          (solar-longitude-after spring jan1))
         (paschal-moon ; Date of next full moon.
          (floor (apparent-from-local
                  (local-from-universal
                   (lunar-phase-at-or-after full equinox)
                   jerusalem)
                  jerusalem))))
    ;; Return the Sunday following the Paschal moon.
    (kday-after sunday paschal-moon)))

(defconstant jaffa
  ;; TYPE location
  ;; Location of Jaffa, Israel.
  (location (angle 32 1 60) (angle 34 45 0) (mt 0) (hr 2)))

(defun phasis-on-or-after (date locale)
  ;; TYPE (fixed-date location) -> fixed-date
  ;; Closest fixed date on or after $date$ on the eve
  ;; of which crescent moon first became visible at $locale$.
  (let* ((mean ; Mean date of prior new moon.
          (- date
             (floor (* (/ (lunar-phase (1+ date)) (deg 360))
                       mean-synodic-month))))
         (tau ; Check if not visible yet on $date$.
          (if (and (<= (- date mean) 3)
                   (not (visible-crescent (1- date) locale)))
              date
            (+ mean 29)))) ; next new moon
    (next d tau (visible-crescent d locale))))

(defun observational-hebrew-new-year (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of Observational (classical)
  ;; Nisan 1 occurring in Gregorian year $g-year$.
  (let* ((jan1 (gregorian-new-year g-year))
         (equinox ; Moment (UT) of spring of g-year.
          (solar-longitude-after spring jan1))
         (set ; Moment (UT) of sunset on day of equinox.
          (universal-from-standard
           (sunset (floor equinox) jaffa)
           jaffa)))
    (phasis-on-or-after
     (- (floor equinox) ; Day of equinox
        (if ; Spring starts before sunset.
            (< equinox set) 14 13))
     jaffa)))

(defun fixed-from-observational-hebrew (h-date)
  ;; TYPE hebrew-date -> fixed-date
  ;; Fixed date equivalent to Observational Hebrew date.
  (let* ((month (standard-month h-date))
         (day (standard-day h-date))
         (year (standard-year h-date))
         (year1 (if (>= month tishri) (1- year) year))
         (start (fixed-from-hebrew
                 (hebrew-date year1 nisan 1))) 
         (g-year (gregorian-year-from-fixed
                  (+ start 60)))
         (new-year (observational-hebrew-new-year g-year))
         (midmonth ; Middle of given month.
          (+ new-year (round (* 29.5 (1- month))) 15)))
    (+ (phasis-on-or-before ; First day of month.
        midmonth jaffa)
       day -1)))

(defun observational-hebrew-from-fixed (date)
  ;; TYPE fixed-date -> hebrew-date
  ;; Observational Hebrew date (year month day)
  ;; corresponding to fixed $date$.
  (let* ((crescent ; Most recent new moon.
          (phasis-on-or-before date jaffa))
         (g-year (gregorian-year-from-fixed date))
         (ny (observational-hebrew-new-year g-year))
         (new-year (if (< date ny)
                       (observational-hebrew-new-year 
                        (1- g-year))
                     ny))
         (month (1+ (round (/ (- crescent new-year) 29.5))))
         (year (+ (standard-year (hebrew-from-fixed new-year))
                  (if (>= month tishri) 1 0)))
         (day (- date crescent -1)))
    (hebrew-date year month day)))

(defun classical-passover-eve (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of Classical (observational) Passover Eve
  ;; (Nisan 14) occurring in Gregorian year $g-year$.
  (+ (observational-hebrew-new-year g-year) 13))


