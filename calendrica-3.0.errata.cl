;;
;; CALENDRICA 3.0 -- Common Lisp
;; Errata/Enhancement as (copied or inspired) from
;; http://emr.cs.iit.edu/home/reingold/calendar-book/third-edition/errata.shtml
;;
;; History at the end inorder not to shift line numbers (referenced
;; in generated python code)
;;

;; changed (Errata '4:08pm, November 15, 2009')
;; fixed values, not relative to sunday
(defconstant monday
  ;; TYPE day-of-week
  ;; Residue class for Monday.
  1)

(defconstant tuesday
  ;; TYPE day-of-week
  ;; Residue class for Tuesday.
  2)

(defconstant wednesday
  ;; TYPE day-of-week
  ;; Residue class for Wednesday.
  3)

(defconstant thursday
  ;; TYPE day-of-week
  ;; Residue class for Thursday.
  4)

(defconstant friday
  ;; TYPE day-of-week
  ;; Residue class for Friday.
  5)

(defconstant saturday
  ;; TYPE day-of-week
  ;; Residue class for Saturday.
  6)

;; changed  (Errata '4:08pm, November 15, 2009')
;; different/simpler implementation
(defun day-number (g-date)
  ;; TYPE gregorian-date -> positive-integer
  ;; Day number in year of Gregorian date $g-date$.
  (gregorian-date-difference
   (gregorian-date (1- (standard-year g-date)) december 31)
   g-date))

;; changed  (Errata '4:08pm, November 15, 2009')
;; different/simpler implementation
(defun days-remaining (g-date)
  ;; TYPE gregorian-date -> nonnegative-integer
  ;; Days remaining in year after Gregorian date $g-date$.
  (gregorian-date-difference
   g-date
   (gregorian-date (standard-year g-date) december 31)))

;; changed  (Errata '4:08pm, November 15, 2009')
;; cope with n = 0 case
(defun nth-kday (n k g-date)
  ;; TYPE (integer day-of-week gregorian-date) -> fixed-date
  ;; If $n$>0, return the $n$-th $k$-day on or after
  ;; $g-date$.  If $n$<0, return the $n$-th $k$-day on or
  ;; before $g-date$.  If $n$=0 return bogus. A $k$-day of
  ;; 0 means Sunday, 1 means Monday, and so on.
  (cond ((> n 0)
         (+ (* 7 n)
            (kday-before k (fixed-from-gregorian g-date))))
        ((< n 0)
         (+ (* 7 n)
            (kday-after k (fixed-from-gregorian g-date))))
        (t bogus)))

;; changed  (Errata '4:08pm, November 15, 2009')
;; for robustness, replaced -272 with -273 + gregorian-epoch
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
             -273
             gregorian-epoch)))
    ;; Return the Sunday following the Paschal moon.
    (kday-after sunday paschal-moon)))

;; changed  (Errata '4:08pm, November 15, 2009')
;; new value from Meeus's Astronomical Algorithms, 3rd Ed.
(defconstant mean-synodic-month
  ;; TYPE real
  29.530588861L0)

;; changed  (Errata '4:08pm, November 15, 2009')
;; modified as from Meeus's Astronomical Algorithms, 3rd Ed.
(defun nth-new-moon (n)
  ;; TYPE integer -> moment
  ;; Moment of $n$-th new moon after (or before) the new moon
  ;; of January 11, 1.  Adapted from "Astronomical Algorithms"
  ;; by Jean Meeus, Willmann-Bell, Inc., 2nd ed., 1998.
  (let* ((n0 24724) ; Months from RD 0 until j2000.
         (k (- n n0)) ; Months since j2000.
         (c (/ k 1236.85L0)) ; Julian centuries.
         (approx (+ j2000
                    (poly c (list 5.09766L0
                               (* mean-synodic-month
                                  1236.85L0)
                               0.0001437L0
                               -0.000000150L0
                               0.00000000073L0))))
         (cap-E (poly c (list 1 -0.002516L0 -0.0000074L0)))
         (solar-anomaly
          (poly c (deg (list 2.5534L0
                             (* 1236.85L0 29.10535669L0)
                             -0.0000014L0 -0.00000011L0))))
         (lunar-anomaly
          (poly c (deg (list 201.5643L0 (* 385.81693528L0
                                      1236.85L0)
                        0.0107582L0 0.00001238L0
                        -0.000000058L0))))
         (moon-argument ; Moon's argument of latitude.
          (poly c (deg (list 160.7108L0 (* 390.67050284L0
                                      1236.85L0)
                        -0.0016118L0 -0.00000227L0
                        0.000000011L0))))
         (cap-omega ; Longitude of ascending node.
          (poly c (list 124.7746L0 (* -1.56375588L0 1236.85L0)
                        0.0020672L0 0.00000215L0)))
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
          (list 0.016321L0 26.651886L0
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

;; new  (Errata '4:08pm, November 15, 2009')
(defun lunar-node (date)
  ;; TYPE fixed-date -> angle
  ;; Angular distance of the node from the equinoctal point
  ;; at fixed date $date$.
  (- (mod (+ (moon-node (julian-centuries date)) (deg 90)) 180) 90))

;; new  (Errata '4:08pm, November 15, 2009')
(defun sidereal-lunar-longitude (tee)
  ;; TYPE moment -> angle
  ;; Sidereal lunar longitude at moment $tee$.
  (mod (+ (lunar-longitude tee)
          (- (precession tee))
          sidereal-start)
        360))

;; rename (Errata '4:08pm, November 15, 2009')
;; (setq chinese-day-name chinese-name-of-day)

;; rename (Errata '4:08pm, November 15, 2009')
;; (setq chinese-month-name chinese-name-of-month)

;; rename (Errata '4:08pm, November 15, 2009')
;; (setq chinese-year-name chinese-name-of-year)

;; rename (Errata '4:08pm, November 15, 2009')
;; (setq hindu-location hindu-locale)

;; rename (Errata '4:08pm, November 15, 2009')
;; (setq islamic-location islamic-locale)

;; rename  (Errata '4:08pm, November 15, 2009')
;; args/type 'locale' --> 'location'

;; changed  (Errata '4:08pm, November 15, 2009')
;; drop floor and take moment as argument
(defun hindu-tropical-longitude (tee)
  ;; TYPE moment -> rational-angle
  ;; Hindu tropical longitude on moment $tee$.
  ;; Assumes precession with maximum of 27 degrees
  ;; and period of 7200 sidereal years
  ;; (= 1577917828/600 days).
  (let* ((days (- tee hindu-epoch)) ; days and time.
         (precession
          (- (deg 27)
             (abs
              (- (deg 54)
                 (mod (+ (deg 27)
                         (* (deg 108) 600/1577917828 days))
                      108))))))
    (mod (- (hindu-solar-longitude tee) precession)
         360)))

;; changed  (Errata '4:08pm, November 15, 2009')
;; swapped arguments for consistency --> change all calls to this!!!!
(defun tibetan-leap-month? (t-year t-month)
  ;; TYPE (tibetan-month tibetan-year) -> boolean
  ;; True if $t-month$ is leap in Tibetan year $t-year$.
  (= t-month
     (tibetan-month
      (tibetan-from-fixed
       (fixed-from-tibetan
        (tibetan-date t-year t-month true 2 false))))))

;; new  (Errata '4:08pm, November 15, 2009')
(defun tibetan-leap-day? (t-year t-month t-day)
  ;; TYPE (tibetan-day tibetan-month tibetan-year) ->
  ;; tibetan-leap-day. True if t-day is leap in Tibetan
  ;; month t-month and year t-year.
  (or
   (= t-day
      (tibetan-day
       (tibetan-from-fixed
        (fixed-from-tibetan
         (tibetan-date t-year t-month false t-day true)))))
   ;; Check also in leap month if there is one.
   (= t-day
      (tibetan-day
       (tibetan-from-fixed
        (fixed-from-tibetan
         (tibetan-date t-year t-month
                       (tibetan-leap-month? t-month t-year)
                       t-day true)))))))

;; changed  (Errata '4:08pm, November 15, 2009')
;; visible-crescent checks visibility on the eve of date
;; so it should be invoked with date not date - 1
;; (also rename locale to location)
(defun phasis-on-or-after (date location)
  ;; TYPE (fixed-date location) -> fixed-date
  ;; Closest fixed date on or after $date$ on the eve
  ;; of which crescent moon first became visible at $location$.
  (let* ((mean ; Mean date of prior new moon.
          (- date
             (floor (* (/ (lunar-phase (1+ date)) (deg 360))
                       mean-synodic-month))))
         (tau ; Check if not visible yet on $date$.
          (if (and (<= (- date mean) 3)
                   (not (visible-crescent date location)))
              date
            (+ mean 29)))) ; next new moon
    (next d tau (visible-crescent d location))))

;; new  (Errata '4:08pm, November 15, 2009')
(defconstant hebrew-location
  ;; TYPE location
  ;; Location (Haifa) for determining Observational Hebrew calendar.
  haifa)

;; changed  (Errata '4:08pm, November 15, 2009')
;; use hebrew-location (instead of jaffa)
(defun observational-hebrew-new-year (g-year)
  ;; TYPE gregorian-year -> fixed-date
  ;; Fixed date of Observational (classical)
  ;; Nisan 1 occurring in Gregorian year $g-year$.
  (let* ((jan1 (gregorian-new-year g-year))
         (equinox ; Moment (UT) of spring of g-year.
          (solar-longitude-after spring jan1))
         (set ; Moment (UT) of sunset on day of equinox.
          (universal-from-standard
           (sunset (floor equinox) hebrew-location)
           hebrew-location)))
    (phasis-on-or-after
     (- (floor equinox) ; Day of equinox
        (if ; Spring starts before sunset.
            (< equinox set) 14 13))
     hebrew-location)))

;; changed  (Errata '4:08pm, November 15, 2009')
;; use hebrew-location (instead of jaffa)
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
        midmonth hebrew-location)
       day -1)))

;; changed  (Errata '4:08pm, November 15, 2009')
;; use hebrew-location (instead of jaffa)
(defun observational-hebrew-from-fixed (date)
  ;; TYPE fixed-date -> hebrew-date
  ;; Observational Hebrew date (year month day)
  ;; corresponding to fixed $date$.
  (let* ((crescent ; Most recent new moon.
          (phasis-on-or-before date hebrew-location))
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

;; new  (Errata '4:08pm, November 15, 2009')
(defun month-length (date location)
  (let ((moon (phasis-on-or-after (1+ date) location))
        (prev (phasis-on-or-before date location)))
    (- moon prev)))

;; new  (Errata '4:08pm, November 15, 2009')
(defun alt-fixed-from-observational-islamic (i-date)
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
                       mean-synodic-month))))
         (moon (phasis-on-or-before midmonth islamic-location))
         (date (+ moon day -1)))
    (if (early-month? midmonth islamic-location) (1- date) date)))

;; new  (Errata '4:08pm, November 15, 2009')
(defun alt-observational-islamic-from-fixed (date)
  ;; TYPE fixed-date -> islamic-date
  ;; Observational Islamic date (year month day)
  ;; corresponding to fixed $date$.
  (let* ((early (early-month? date islamic-location))
         (long (and early (> (month-length date islamic-location) 29)))
         (date-prime (if long (1+ date) date))
         (moon (phasis-on-or-before date-prime islamic-location))
         (elapsed-months
          (round (/ (- moon islamic-epoch)
                    mean-synodic-month)))
         (year (1+ (quotient elapsed-months 12)))
         (month (1+ (mod elapsed-months 12)))
         (day (- date-prime moon (if (and early (not long)) -2 -1))))
    (islamic-date year month day)))

;; new  (Errata '4:08pm, November 15, 2009')
(defun alt-fixed-from-observational-hebrew (h-date)
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
          (+ new-year (round (* 29.5 (1- month))) 15))
         (moon (phasis-on-or-before midmonth hebrew-location))
         (date (+ moon day -1)))
    (if (early-month? midmonth hebrew-location) (1- date) date)))

;; new  (Errata '4:08pm, November 15, 2009')
(defun alt-observational-hebrew-from-fixed (date)
  ;; TYPE fixed-date -> hebrew-date
  ;; Observational Hebrew date (year month day)
  ;; corresponding to fixed $date$.
  (let* ((early (early-month? date hebrew-location))
         (long (and early (> (month-length date hebrew-location) 29)))
         (date-prime (if long (1+ date) date))
         (moon (phasis-on-or-before date-prime hebrew-location))
         (g-year (gregorian-year-from-fixed date-prime))
         (ny (observational-hebrew-new-year g-year))
         (new-year (if (< date-prime ny)
                       (observational-hebrew-new-year 
                        (1- g-year))
                     ny))
         (month (1+ (round (/ (- moon new-year) 29.5))))
         (year (+ (standard-year (hebrew-from-fixed new-year))
                  (if (>= month tishri) 1 0)))
         (day (- date-prime moon (if (and early (not long)) -2 -1))))
    (hebrew-date year month day)))

;; new (Errata '12:58pm, November 19, 2009')
;; could be used for sunrise/sunset
(defun refraction (tee location)
  ;; TYPE (moment location) -> angle
  ;; Angle correction due to refraction calculated at time $tee$ and $location$.
  ;; The time parameter $tee$ is not being used here, but could be used in a more 
  ;; refined calculation that takes average atmospheric conditions into account.
  (let* ((h (max (mt 0) (elevation location)))
         (cap-R (mt 6.372d6)) ; Radius of Earth.
         (dip ; Depression of visible horizon.
          (arccos-degrees (/ cap-R (+ cap-R h)))))
    (+ (angle 0 50 0) dip (* (secs 19) (sqrt h)))))

;; new (Errata '12:58pm, November 19, 2009')
(defun observed-lunar-altitude (tee location)
  ;; TYPE (moment location) -> angle
  ;; Observed altitude of moon at $tee$ at $location$
  ;; taking refraction into account.
  (+ (topocentric-lunar-altitude tee location) (refraction tee location)))

;; new (Errata '12:58pm, November 19, 2009')
(defun moonrise (date location)
  ;; TYPE (fixed-date location) -> moment
  ;; Standard time of moonrise on fixed $date$ at
  ;; $locale$.
  (let* ((t (universal-from-standard date location))
         (waning (> (lunar-phase t) (deg 180)))
         (alt (observed-lunar-altitude t location))
         (offset (/ alt 360))
         (approx (cond ((and waning (> offset 0)) (1+ t -offset))
                       (waning (- t offset))
                       (t (+ t (/ 1 2) offset))))
         (rise (binary-search
                l (- approx (hr 3))
                u (+ approx (hr 3))
                x (> (observed-lunar-altitude x location) (deg 0))
                (< (- u l) (hr (/ 1 60))))))
    (if (< rise (1+ t)) (standard-from-universal rise location) bogus)))

;; new (Errata '11:53pm, June 22, 2011'), serious error
(defun possible-hebrew-days (h-month h-day)
  ;; TYPE (hebrew-month hebrew-day) -> list-of-weekdays
  ;; Possible days of week
  (let* ((h-date0 (hebrew-date 5 nisan 1))
         ;; leap year with full pattern
         (h-year (if (> h-month elul) 6 5))
         (h-date (hebrew-date h-year h-month h-day))
         (n (- (fixed-from-hebrew h-date)
               (fixed-from-hebrew h-date0)))
         (basic (list tuesday thursday saturday))
         (extra
          (cond
           ((and (= h-month marheshvan) (= h-day 30))
            nil)
           ((and (= h-month kislev) (< h-day 30))
            (list monday wednesday friday))
           ((and (= h-month kislev) (= h-day 30))
            (list monday))
           ((member h-month (list tevet shevat))
            (list sunday monday))
           ((and (= h-month adar) (< h-day 30))
            (list sunday monday))
           (t (list sunday)))))
    (shift-days (append basic extra) n)))

;; changed  (Errata 'August 08, 2011')
(defun birkath-ha-hama (g-year)
 ;; TYPE gregorian-year -> list-of-fixed-dates
 ;; List of fixed date of Birkath ha-Hama occurring in
 ;; Gregorian year g-year, if it occurs.
  (let* ((moments (samuel-season-in-gregorian spring g-year)))
    (if (and (not (equal moments nil))
             (= (day-of-week-from-fixed
                 (fixed-from-moment (first moments))) tuesday)
             (= (time-from-moment (first moments)) (hr 18)))
         (list (1+ (fixed-from-moment (first moments))))
      nil)))

(defun sh-ela (g-year)
 ;; TYPE gregorian-year -> list-of-fixed-dates
 ;; List of fixed dates of Sh'ela occurring in
 ;; Gregorian year $g-year$.
  (fixed-from-list-of-moments
    (samuel-season-in-gregorian (angle 238 17 0) g-year)))

(defun fixed-from-list-of-moments (ell)
 ;; TYPE list-of-moments -> list-of-fixed-dates
 ;; List of fixed dates corresponding to list ell
 ;; of moments.
  (if (equal ell nil)
      nil
    (append (list (fixed-from-moment (first ell)))
            (fixed-from-list-of-moments (rest ell)))))

(defun multiples-in-range (cap-L offset range)
 ;; TYPE (real moment range) -> list-of-moments
 ;; Those offset +k *cap-L that occur in range
 ;; (but not at its very end).
  (let* ((tee ;; First occurrence.
          (+ offset (* cap-L
                       (ceiling (/ (- (start range) offset)
                                    cap-L))))))
    (if (>= tee (end range))
        nil
      (append
        (list tee)
        (multiples-in-range
          cap-L offset
          (interval (+ (start range) cap-L)
                    (end range)))))))

(defun season-in-gregorian (season g-year cap-L start)
 ;; TYPE (season gregorian-year real moment) -> list-of-moments
 ;; Moments of season season in Gregorian year g-year.
 ;; Tropical year is cap-L days, seasons are of equal length,
 ;; and an occurrence of spring was at the moment start.
  (multiples-in-range
   cap-L
   (+ start (* (/ season 360) cap-L))
   (interval (gregorian-new-year g-year)
             (gregorian-new-year (1+ g-year)))))

(defun julian-season-in-gregorian (season g-year)
 ;; TYPE (season gregorian-year) -> list-of-moments
 ;; Moment(s) of Julian season season in Gregorian year g-year.
  (season-in-gregorian season g-year
                       (+ 365 (hr 6))
                       (fixed-from-julian
                        (julian-date (bce 1) march 23))))

(defun samuel-season-in-gregorian (season g-year)
 ;; TYPE (season gregorian-year) -> list-of-moments
 ;; Moment(s) of season season in Gregorian year g-year
 ;; per Samuel.
  (season-in-gregorian season g-year
                       (+ 365 (hr 6))
                       (+ (fixed-from-hebrew
                           (hebrew-date 1 adar 21))
                          (hr 18))))

(defun adda-season-in-gregorian (season g-year)
 ;; TYPE (season gregorian-year) -> list-of-moments
 ;; Moment(s) of season season in Gregorian year g-year
 ;; per R. Adda bar Ahava.
  (season-in-gregorian season g-year
                       (+ 365 (hr (+ 5 3791/4104)))
                       (+ (fixed-from-hebrew
                           (hebrew-date 1 adar 28))
                          (hr 18))))

;; changed  (Errata 'November 10, 2012')
(defun mayan-year-bearer-from-fixed (date)
  ;; TYPE fixed-date -> mayan-tzolkin-name
  ;; Year bearer of year containing fixed $date$.
  ;; Returns bogus for uayeb.
  (let* ((x (mayan-haab-on-or-before
             (mayan-haab-date 1 0)
             date)))
    (if (= (mayan-haab-month (mayan-haab-from-fixed date))
           19)
        bogus
      (mayan-tzolkin-name (mayan-tzolkin-from-fixed x)))))

;;;============================= History =======================================
;; 2009/11/19 Enrico Spinielli
;;   Extracted changes/enhancements from
;;   Errata '4:08pm, November 15, 2009' from
;;   http://emr.cs.iit.edu/home/reingold/calendar-book/third-edition/errata.shtml
;;
;; 2009/11/20 Enrico Spinielli
;;   Re-organised so history goes at the end
;;   Completed nth-new-moon data lists
;;
;; 2009/11/21 Enrico Spinielli
;;   Added refraction and (signature of) observed-lunar-altitude from
;;   Errata '12:88pm, November 19, 2009' from
;;   http://emr.cs.iit.edu/home/reingold/calendar-book/third-edition/errata.shtml
;;   Completed nth-new-moon data lists
;;;=============================================================================
