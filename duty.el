;;; duty.el --- working with your workdays

;; Copyright (C) 2022  Karsten Klöss

;; Author: Karsten Klöss <karsten@kloess.xyz>
;; Keywords: calendar, org, agenda

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is a collection of functions to ease keeping an
;; overview on you workdays.

;; Mark workdays, sick days and vacation days in `org-agenda', start
;; the process of requesting new vacation days from `calendar' and
;; count workdays, non workdays and vacation days in a region,
;; similarly to `calendar-count-days-region'.

;; Starting the process for requesting new vacation days means marking
;; a region in the calendar and calling `duty-calendar-new-holidays'.
;; This will throw a new task with subtasks at the end of your
;; `duty-org-refile-target'.  These are tasks for requesting the just
;; selected days as vacation days from your employer, waiting for
;; acceptance/decline, a task to add the new vacation day(s) to
;; `duty-vacation-days' (including the correct diary date expression),
;; and in the default version schedules two tasks the last work day
;; before your vacation to inform colleagues about loose strings and
;; set up an absence note.  These tasks are highly subjective and you
;; should probably customize these - maybe by copying und overwriting
;; the function.  There will also be a property "DURATION", telling
;; you the number of days of your absence.  This is useful to be used
;; with `org-columns' and provides an overview over how many days you
;; took in total.  By refiling these tasks to e.g. a heading for every
;; year, you will be able to see how many vacation days you took that
;; year at a glance (and know how many you should have left to take).
;; This functionality is similarly available in `calendar' via
;; `duty-calendar-count-days-region'.

;; You'll have to maintain two functions: `duty-work-days' and
;; `duty-vacation-days'.  These should be made up of `diary-lib'
;; functions like `diary-date', `diary-block' and `diary-float'.

;; An example of `duty-work-days' would be

;; (defun duty-work-days ()
;;   "Joins all the separate work days.

;; Add to this when changing jobs."
;;   (or (old-employer-work-days)
;;       (new-employer-work-days)))

;; where `old-employer-work-days' could be

;; (defun old-employer-work-days ()
;;   "Returns workdays for for "old-employer".

;; Can be used in org agenda.
;; Example:
;; ** Task
;; <%%(old-employer-work-days)>"
;;   (and
;;    (not (duty-is-weekend-p date))
;;    (not (duty-is-official-holiday-p date))
;;    (not (vacation-days))
;;    (not (sick-days))
;;    (diary-block 2022 01 01 2030 01 01); Actual start date at the job and arbitrary end date. Adjust when finished working at the workplace or extend when reached
;;    ))

;; For the `duty-is-official-holiday-p'-check the
;; `holiday-local-holidays' variable is used. For the moment, please
;; customize this variable to contain the holidays relevant to you -
;; only days you actually do not need to work on make sense the way
;; the variable will be used, see the defun of
;; `old-employer-work-days' above.

;; In `calendar', call `duty-calendar-count-days-region' (with
;; prefix arg).  It is usable as a drop-in replacement for
;; `calendar-count-days-region'; in fact it calls this command when
;; not giving a prefix arg.

;;; TODO:

;; Use `duty-inform-about-todos' in case a function is used that
;; would require some user setup beforehand

;; Make refiles possible to headings? Maybe utilize org capture functionality?

;; Make order of `duty-calendar-count-days-region' functions
;; configurable (how many prefix args call which function)

;;; Code:

;;;; Requirements

(require 'cal-dst)
(require 'calendar)
(require 'diary-lib)

;;;; Customization

(defgroup duty nil
  "Settings for `duty'."
  :group 'org
  :link '(url-link "http://github.com/tenklo/duty"))

(defcustom duty-holiday-days 'holiday-local-holidays
  "The holidays you are not supposed to work on.

Can either be a variable containing a list of holidays, or a list itself."
  :type '(choice variable list))

(defcustom duty-org-refile-target "~/new-vacation.org"
  "Target file your entries created by `duty-calendar-new-holidays' get refiled to."
  :type 'string)

(defcustom duty-heading-addition nil
  "Text added to main heading created by `duty-calendar-new-holidays'.

For example useful useful to add some tags. Remember to add a leading Space."
  :type 'string)

;;;; Support functions

(defun duty-inform-about-todos ()
  "Inform user about how to find out how to use this package."
  (message "Refer to Commentary in M-x find-library RET duty RET for things to set up to use this package."))

(defun duty-day-in-func-p (func date1)
  "Return non-nil when `date' is on a work day according to `duty-work-days'."
  (let ((entry t)
        (date date1))
    (funcall func)))

(defun duty-work-day-p (date1)
  "Return non-nil when `date' is on a work day according to `duty-work-days'."
  (duty-day-in-func-p #'duty-work-days date1))

(defun duty-vacation-day-p (date)
  "Return non-nil when `date' is on a work day according to `duty-work-days'."
  (duty-day-in-func-p #'duty-vacation-days date))

(defun duty-days-where-next-day-is-a-work-day ()
  "Days where the next day is a work day, according to `duty-work-days'.

Useful to timestamp or schedule tasks in `org-mode' only on days
where the next day is a work day.

Example:
* 22:00 Go to bed
<%%(duty-days-where-next-day-is-a-work-day)>"
  (let ((date (duty-increment-gregorian-date date)))
    (duty-work-days)))

(defun duty-next-day-work-day-p (date)
  "Returns non-nil where the next day to `date' is a work day, according to `duty-work-days'."
  (let ((next-day (duty-increment-gregorian-date date)))
    (duty-work-day-p next-day)))

(defun duty-increment-gregorian-date (date)
  (calendar-gregorian-from-absolute (1+ (calendar-absolute-from-gregorian date))))

(defun duty-decrement-gregorian-date (date)
  (calendar-gregorian-from-absolute (1- (calendar-absolute-from-gregorian date))))

(defun duty-count-days-between-dates-in-function (date1 date2 func)
  (let ((start-date (if (< date1 date2) date1 date2))
        (end-date (if (> date1 date2) date1 date2))
        (work-days 0))
    (while (<= start-date end-date)
      (when (duty-day-in-func-p func (calendar-gregorian-from-absolute start-date))
        (cl-incf work-days))
      (cl-incf start-date))
    work-days))

(defun duty-count-days-not-between-dates-in-function (date1 date2 func)
  (let ((start-date (if (< date1 date2) date1 date2))
        (end-date (if (> date1 date2) date1 date2))
        (work-days 0))
    (while (<= start-date end-date)
      (when (not (duty-day-in-func-p func (calendar-gregorian-from-absolute start-date)))
        (cl-incf work-days))
      (cl-incf start-date))
    work-days))

(defun duty-count-work-days-between (date1 date2)
  "Count work days between `date1' and `date2'."
  (duty-count-days-between-dates-in-function date1 date2 #'duty-work-days))

(defun duty-count-vacation-days-between (date1 date2)
  "Count work days between `date1' and `date2'."
  (duty-count-days-between-dates-in-function date1 date2 #'duty-vacation-days))

(defun duty-count-days (d1 d2)
  (let* ((days (- (calendar-absolute-from-gregorian d1)
                  (calendar-absolute-from-gregorian d2)))
         (days (1+ (if (> days 0) days (- days)))))
    days))

(defun duty-count-days-not-between-dates-in-function (date1 date2 func)
  "Count non work days between `date1' and `date2'."
  (let* ((d1 (calendar-absolute-from-gregorian date1))
         (d2 (calendar-absolute-from-gregorian date2))
         (func-days (duty-count-days-between-dates-in-function d1 d2 func))
         (days (duty-count-days date1 date2))
         (diff (- days func-days)))
    diff))

(defun duty-count-non-work-days-between (date1 date2)
  (duty-count-days-not-between-dates-in-function date1 date2 #'duty-work-days))

(defun duty-count-non-work-days-between (date1 date2)
  "Count non work days between `date1' and `date2'."
  (let* ((d1 (calendar-absolute-from-gregorian date1))
         (d2 (calendar-absolute-from-gregorian date2))
         (work-days (duty-count-work-days-between d1 d2))
         (days (duty-count-days date1 date2))
         (diff (- days work-days)))
    diff))

(defun duty-find-work-day-before (date)
  "Returns the last workday before `date'."
  (message "Starting with %s" (int-to-string date))
  (let ((date (- date 1)))
    (while (not (duty-work-day-p (calendar-gregorian-from-absolute date)))
      (message (int-to-string date))
      (setq date (- date 1)))
    (message (int-to-string date))
    date))

(defun duty-id-remove-spaces (text)
  "Replace spaces in `TEXT' with '-'."
  (replace-regexp-in-string (regexp-quote " ") "-" text t t))

(defun duty-work-new-holidays-org-project-for (date1 date2)
  "Adds a new task to `duty-org-refile-target'.

These are tasks for requesting the just selected days as vacation
days from your employer, waiting for acceptance/decline, a task
to add the new vacation day(s) to `duty-vacation-days' (including
the correct diary date expression), and in the default version
schedules two tasks the last work day before your vacation to
inform colleagues about loose strings and set up an absence note.

These tasks are highly subjective and you should probably
customize these - maybe by copying und overwriting the function.

There will also be a property 'DURATION', telling you the number
of days of your absence.  This is useful to be used with
`org-columns' and provides an overview over how many days you
took in total.  By refiling these tasks to e.g. a heading for
every year, you will be able to see how many vacation days you
took that year at a glance (and know how many you should have
left to take).

This functionality (and more) is similarly available in
`calendar' via `duty-calendar-count-days-region'."
  (let* ((calendar-date-display-form (diary-date-display-form))
         (date1 (calendar-absolute-from-gregorian date1))
         (date2 (calendar-absolute-from-gregorian date2))
         (day-count (duty-count-non-weekend-or-official-holiday-days date1 date2))
         (start-date (if (< date1 date2) date1 date2))
         (end-date (if (> date1 date2) date1 date2))
         (days-string (if (equal day-count 1)
                          (format-time-string "%d. %B %Y" (calendar-time-from-absolute start-date 0))
                        (concat (format-time-string "%d. %B %Y" (calendar-time-from-absolute start-date 0))
                                " - "
                                (format-time-string "%d. %B %Y" (calendar-time-from-absolute end-date 0)))))
         (days-string-id (duty-id-remove-spaces days-string))
         (diary-date-string (if (> day-count 1)
                                (format "(diary-block %s %s)"
                                        (calendar-date-string (calendar-gregorian-from-absolute start-date) nil t)
                                        (calendar-date-string (calendar-gregorian-from-absolute end-date) nil t))
                              (format "(diary-date %s)"
                                      (calendar-date-string (calendar-gregorian-from-absolute start-date) nil t))))
         (work-day-before-start-date (duty-find-work-day-before start-date))
         (heading-addition (if duty-heading-addition
                               duty-heading-addition
                             ""))
         (work-day-before-string (format-time-string "%Y-%m-%d %a" (calendar-time-from-absolute work-day-before-start-date 0))))
    (with-current-buffer (find-file-noselect duty-org-refile-target) ; check if duty-org refile target is variable, then use its value, else use as string
      (goto-char (point-max))
      (insert
       "\n* TODO " days-string heading-addition "\n"
       ":PROPERTIES:\n"
       ":DURATION: " (number-to-string day-count) "\n"
       ":END:\n"
       "** NEXT anfragen\n"
       ":PROPERTIES:\n"
       ":TRIGGER:  " days-string-id "-reply(WAIT)\n"
       ":END:\n"
       "** TODO Bestätigung/Ablehnung abwarten\n"
       ":PROPERTIES:\n"
       ":ID:       " days-string "-reply\n"
       ":TRIGGER:  chain-siblings(NEXT)\n"
       ":END:\n"
       "** TODO ggf. Eintrag in work-days.el\n"
       diary-date-string "\n"
       "** TODO Pflichten prüfen\n"
       "SCHEDULED: <" work-day-before-string " 8:30>\n"
       "- etwas nicht fertig geworden?\n"
       "- Termine, die ich nicht wahrnehmen kann?\n"
       "** TODO Abwesenheitsbenachrichtigung aktivieren\n"
       "SCHEDULED: <" work-day-before-string " 15:30>\n")
      (org-align-all-tags))
    (find-file-other-window duty-org-refile-target)))

(defun duty-is-official-holiday-p (date)
  "Return non-nil when `date' is on a official holiday.

Official holidays in this context are considered days you are not
required to work on by law in your area. It queries
`holiday-local-holidays' for this purpose, so you should
configure your holidays there."
  (let ((calendar-holidays holiday-local-holidays))
    (calendar-check-holidays date)))

(defun duty-is-weekend-p (date)
  "Returns non-nil if `date' is on a weekend."
  (memq
   (calendar-day-of-week date) calendar-weekend-days))

(defun duty-count-non-weekend-or-official-holiday-days (d1 d2)
  "Return number of days that are neither weekend nor official holidays between `d1' and `d2'.

Weekend days are days that return non-nil to `duty-is-weekend-p'.

Official holidays are days that return non-nil to `duty-is-official-holiday-p'."
  (let* ((tmp-date (if (< d1 d2) d1 d2))
         (end-date (if (> d1 d2) d1 d2))
         (days 0))
    (while (<= tmp-date end-date)
      (let ((date-gregorian (calendar-gregorian-from-absolute tmp-date)))
        (when (and (not (duty-is-official-holiday-p date-gregorian))
                   (not (duty-is-weekend-p date-gregorian)))
          (cl-incf days))
        (cl-incf tmp-date)))
    days))

(defun duty-calendar-count-days-in-func (func kind-of-days)
  (let* ((date1 (calendar-absolute-from-gregorian
                 (calendar-cursor-to-date t)))
         (date2 (calendar-absolute-from-gregorian
                 (or (car calendar-mark-ring)
                     (error "No mark set in this buffer"))))
         (work-days (duty-count-days-between-dates-in-function date1 date2 func)))
    (message "There %s %s %s %s between %s and %s."
             (if (equal work-days 1) "is" "are")
             work-days
             kind-of-days
             (if (equal work-days 1) "day" "days")
             (calendar-date-string (calendar-gregorian-from-absolute date1))
             (calendar-date-string (calendar-gregorian-from-absolute date2)))))

(defun duty-calendar-count-days-not-in-func (func kind-of-days)
    (let* ((date1 (calendar-cursor-to-date t))
         (date2 (or (car calendar-mark-ring)
                    (error "No mark set in this buffer")))
         (non-work-days (duty-count-days-not-between-dates-in-function date1 date2 func)))
    (message "There %s %s non-%s %s between %s and %s."
             (if (equal non-work-days 1) "is" "are")
             non-work-days
             kind-of-days
             (if (equal non-work-days 1) "day" "days")
             (calendar-date-string date1)
             (calendar-date-string date2))))

;;;; Interactive functions

(defun duty-calendar-count-non-work-days-region ()
  "Count the number of days (inclusive) you are not supposed to work on between point and the mark."
  (interactive)
  (duty-calendar-count-days-not-in-func #'duty-work-days "work"))

(defun duty-calendar-count-work-days-region ()
  "Count the number of days (inclusive) you are not supposed to work on between point and the mark."
  (interactive)
  (duty-calendar-count-days-in-func #'duty-work-days "work"))

(defun duty-calendar-count-vacation-days-region ()
  "Count the number of vacation days (inclusive) between point and the mark."
  (interactive)
  (duty-calendar-count-days-in-func #'duty-vacation-days "vacation"))

(defun duty-calendar-non-weekend-or-official-holiday-days ()
  (interactive)
  (let* ((date1 (calendar-absolute-from-gregorian
                 (calendar-cursor-to-date t)))
         (date2 (calendar-absolute-from-gregorian
                 (or (car calendar-mark-ring)
                     (error "No mark set in this buffer"))))
         (days (duty-count-non-weekend-or-official-holiday-days date1 date2)))
    (message "There %s %s %s between %s and %s that %s neither on a weekend or a holiday."
             (if (equal days 1) "is" "are")
             days
             (if (equal days 1) "day" "days")
             (calendar-date-string (calendar-gregorian-from-absolute date1))
             (calendar-date-string (calendar-gregorian-from-absolute date2))
             (if (equal days 1) "is" "are"))))

(defun duty-calendar-count-days-region (&optional arg)
  "Count days between point and the mark.

Without prefix argument, simply call `calendar-count-days-region'
to count the days in a region.  Because of this, this command
makes a fine replacement for `calendar-count-days-region', by
default bound to M-= in `calendar-mode-map'.

With C-u prefix arg, call `duty-calendar-count-work-days-region'
to count the days you are/were supposed to work on in a region according to `duty-work-days'.

With C-u C-u prefix arg, call
`duty-calendar-count-non-work-days-region' to count the days you
are/were not supposed to work on in a region.

With C-u C-u C-u prefix arg, call
`duty-calendar-count-vacation-days-region' to count the days you
are on vacation according to `duty-vacation-days'.
"
  (interactive "P")
  (pcase arg
    ('nil
     (calendar-count-days-region))
    ('(4)
     (duty-calendar-count-work-days-region))
    ('(16)
     (duty-calendar-count-non-work-days-region))
    ('(64)
     (duty-calendar-count-vacation-days-region))))

(defun duty-calendar-new-holidays ()
  (interactive)
  (if (equal major-mode 'calendar-mode)
      (let* ((date1 (calendar-cursor-to-date t))
             (date2 (or (car calendar-mark-ring)
                        (error "No mark set in this buffer"))))
        (duty-work-new-holidays-org-project-for date1 date2))
    (error "This command is supposed to be used in calendar-mode.")))

(provide 'duty)
;;; duty.el ends here

