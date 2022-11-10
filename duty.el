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

;; Customize option for `duty-holiday-days', defaulting to holiday-local-holidays(?).

;; Make refiles possible to headings? Maybe utilize org capture functionality?

;;; Code:

;;;; Requirements

(require 'cal-dst)
(require 'calendar)
(require 'diary-lib)

;;;; Support functions

(defun duty-inform-about-todos ()
  (message "Refer to M-x find-library RET duty RET for things to set up to use this package."))

(defun duty-work-day-p (date1)
  "Return non-nil when `date' is on a work day according to `duty-work-days'."
  (let ((entry t)
        (date date1))
    (duty-work-days)))

(defun duty-vacation-day-p (date)
  "Return non-nil when `date' is on a work day according to `duty-work-days'."
  (let ((entry t))
    (duty-vacation-days)))

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
  (let ((next-day (duty-increment-gregorian-date date)))
    (duty-work-day-p next-day)))

(defun duty-increment-gregorian-date (date)
  (calendar-gregorian-from-absolute (1+ (calendar-absolute-from-gregorian date))))

(defun duty-decrement-gregorian-date (date)
  (calendar-gregorian-from-absolute (1- (calendar-absolute-from-gregorian date))))

(defun duty-count-work-days-between (date1 date2)
  "Count work days between `date1' and `date2'."
  (let ((start-date (if (< date1 date2) date1 date2))
        (end-date (if (> date1 date2) date1 date2))
        (work-days 0))
    (while (<= start-date end-date)
      (when (duty-work-day-p (calendar-gregorian-from-absolute start-date))
        (cl-incf work-days))
      (cl-incf start-date))
    work-days))

(defun duty-count-vacation-days-between (date1 date2)
  "Count work days between `date1' and `date2'."
  (let ((start-date (if (< date1 date2) date1 date2))
        (end-date (if (> date1 date2) date1 date2))
        (vacation-days 0))
    (while (<= start-date end-date)
      (when (duty-vacation-day-p (calendar-gregorian-from-absolute start-date))
        (cl-incf vacation-days))
      (cl-incf start-date))
    vacation-days))

(defun duty-count-days (d1 d2)
  (let* ((days (- (calendar-absolute-from-gregorian d1)
                  (calendar-absolute-from-gregorian d2)))
         (days (1+ (if (> days 0) days (- days)))))
    days))

(defun duty-count-non-work-days-between (date1 date2)
  "Count non work days between `date1' and `date2'."
  (let* ((d1 (calendar-absolute-from-gregorian date1))
         (d2 (calendar-absolute-from-gregorian date2))
         (work-days (duty-count-work-days-between d1 d2))
         (days (duty-count-days date1 date2))
         (diff (- days work-days)))
    diff))

(defun duty-find-work-day-before (date)
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
  (let* ((calendar-date-display-form (diary-date-display-form))
         (date1 (calendar-absolute-from-gregorian date1))
         (date2 (calendar-absolute-from-gregorian date2))
         (day-count (duty-count-non-weekend-or-official-holiday-days date1 date2))
         (start-date (if (< date1 date2) date1 date2))
         (end-date (if (> date1 date2) date1 date2))
         (days-string (if (equal day-count 1)
                          (format-time-string "%d. %B %Y" start-date)
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
         (work-day-before-string (format-time-string "%Y-%m-%d %a" (calendar-time-from-absolute work-day-before-start-date 0))))
    (with-current-buffer (find-file-noselect duty-org-refile-target)
      (goto-char (point-max))
      (insert
       "\n* TODO " days-string "\n"
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
       "SCHEDULED: <" work-day-before-string " 15:30>\n"))))

(defun duty-is-official-holiday-p (date)
  (let ((calendar-holidays holiday-local-holidays))
    (calendar-check-holidays date)))

(defun duty-is-weekend-p (date)
  (memq
   (calendar-day-of-week date) calendar-weekend-days))

(defun duty-count-non-weekend-or-official-holiday-days (d1 d2)
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

;;;; Interactive functions

(defun duty-calendar-count-non-work-days-region ()
  (interactive)
  (let* ((date1 (calendar-cursor-to-date t))
         (date2 (or (car calendar-mark-ring)
                    (error "No mark set in this buffer")))
         (non-work-days (duty-count-non-work-days-between date1 date2)))
    (message "There %s %s non-work %s between %s and %s."
             (if (equal non-work-days 1) "is" "are")
             non-work-days
             (if (equal non-work-days 1) "day" "days")
             (calendar-date-string date1)
             (calendar-date-string date2))))

(defun duty-calendar-count-work-days-region ()
  "Count the number of work days (inclusive) between point and the mark."
  (interactive)
  (let* ((date1 (calendar-absolute-from-gregorian
                 (calendar-cursor-to-date t)))
         (date2 (calendar-absolute-from-gregorian
                 (or (car calendar-mark-ring)
                     (error "No mark set in this buffer"))))
         (work-days (duty-count-work-days-between date1 date2)))
    (message "There %s %s work %s between %s and %s."
             (if (equal work-days 1) "is" "are")
             work-days
             (if (equal work-days 1) "day" "days")
             (calendar-date-string (calendar-gregorian-from-absolute date1))
             (calendar-date-string (calendar-gregorian-from-absolute date2)))))

(defun duty-calendar-count-vacation-days-region ()
  "Count the number of vacation days (inclusive) between point and the mark."
  (interactive)
  (let* ((date1 (calendar-absolute-from-gregorian
                 (calendar-cursor-to-date t)))
         (date2 (calendar-absolute-from-gregorian
                 (or (car calendar-mark-ring)
                     (error "No mark set in this buffer"))))
         (vacation-days (duty-count-vacation-days-between date1 date2)))
    (message "There %s %s vacation %s between %s and %s."
             (if (equal vacation-days 1) "is" "are")
             vacation-days
             (if (equal vacation-days 1) "day" "days")
             (calendar-date-string (calendar-gregorian-from-absolute date1))
             (calendar-date-string (calendar-gregorian-from-absolute date2)))))

(defun duty-calendar-count-days-region (&optional arg)
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

(defun duty-calendar-non-weekend-or-official-holiday-days ()
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

(provide 'duty)
;;; duty.el ends here

