;;; org-recur.el --- Recurring org-mode tasks. -*- lexical-binding: t; -*-
;;
;; Filename:    org-recur.el
;; Description: Recurring org-mode tasks.
;; Author:      Marcin Swieczkowski <marcin.swieczkowski@gmail.com>
;; Created:     Fri Feb 15 2017
;; Version:     1.1
;; Package-Requires: ((emacs "24") (org "9.0"))
;; URL:         https://github.com/m-cat/org-recur
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; About:
;;
;; See README.md.
;;
;; Installing:
;;
;; See README.md for the most up-to-date installation instructions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; Requirements

(require 'org)
(require 'org-agenda)

;; Customize group

(defgroup org-recur nil
  "Recurring org-mode tasks."
  :group 'faces)

(defface org-recur
  '((t :inherit org-tag))
  "Face to highlight org-recur dates."
  :group 'org-recur)

(defcustom org-recur-finish-archive nil
  "Non-nil if calling `org-recur-finish' on a task without org-recur syntax should archive it."
  :type 'boolean
  :group 'org-recur)

(defcustom org-recur-finish-done nil
  "Non-nil if calling `org-recur-finish' on a task without org-recur syntax should mark it as DONE."
  :type 'boolean
  :group 'org-recur)

(defcustom org-recur-weekday "wkdy"
  "Date string for org-recur that indicates the next weekday.
This is similar to how e.g. 'fri' indicates the next Friday. Non
case-sensitive. What is considered a weekday can be customized --
see `org-recur-weekday-recurrence'."
  :type 'string
  :group 'org-recur)

(defcustom org-recur-weekday-recurrence "mon,tue,wed,thu,fri"
  "The recurrence string that `org-recur-weekday' expands to.
`org-recur-finish' will pick the soonest of any of the dates
between commas."
  :type 'string
  :group 'org-recur)

;; Internals

;; Simple regexp for extracting the date string from headings, and highlighting
;; in org-agenda.
(defconst org-recur--regexp "|\\([^|]*\\)|")
;; More complex regexp for highlighting in org-mode, without also highlighting
;; tables.
(defconst org-recur--regexp-full "^\\*+ +[^|\n]*\\(|[^|]*|\\)")

(defconst org-recur--full-keywords `((,org-recur--regexp-full 1 'org-recur t)))

(defvar org-recur--buffer-keywords nil)

(defun org-recur--turn-on ()
  "Turn on font-locking."
  (let ((keywords org-recur--full-keywords))
    (set (make-local-variable 'org-recur--buffer-keywords) keywords)
    (font-lock-add-keywords nil keywords t)))
(defun org-recur--turn-off ()
  "Remove font-locking."
  (when org-recur--buffer-keywords
    (font-lock-remove-keywords nil org-recur--buffer-keywords)
    (kill-local-variable 'org-recur--buffer-keywords)))

(defun org-recur--date-string-to-time (org-date-string)
  "Convert ORG-DATE-STRING to a time value."
  (let ((time (org-read-date-analyze org-date-string nil nil)))
    (encode-time 0 0 0 (nth 3 time) (nth 4 time) (nth 5 time))))
(defun org-recur--date-less-p (D1 D2)
  "Return non-nil if date string D1 is earlier than date string D2.
A nil value is always considered greater than any date string.
See ‘org-read-date’ for the various forms of a date string."
  (if (or (not D2) (string= "" D2))
      t
    (if (or (not D1) (string= "" D1))
        nil
      (time-less-p (org-recur--date-string-to-time D1)
                   (org-recur--date-string-to-time D2)))))

(defun org-recur--get-next-date (heading)
  "Return the next date to reschedule to based on HEADING.
NIL if no recurrence found."
  (cond ((string-match org-recur--regexp heading)
         (let* (
                ;; Get the recurrence string.
                ;; Replace any occurrence of "wkdy" (case-insensitive).
                (recurrence (replace-regexp-in-string
                             org-recur-weekday org-recur-weekday-recurrence
                             (match-string 1 heading)))
                ;; Split the recurrence as it may contain multiple options.
                (options (split-string recurrence ","))
                ;; Get the earliest option.
                (next-date
                 (let (value)
                   (dolist (elt options value)
                     (setq value (if (org-recur--date-less-p elt value) elt value))))))
           next-date))
        (t nil)))

;; Autoloads

;;;###autoload
(defun org-recur-finish ()
  "Reschedule an `org-mode' task according to its org-recur date string.
The org-recur syntax is '|DATE|', where DATE can be either an
absolute date or more commonly a delta, e.g. a task heading
containing '|+2|' indicates to `org-recur-finish' to reschedule
the task to two days from now.

All date strings supported by `org-read-date' are available. Also
available is 'wkdy' (customizable with `org-recur-weekday') which
schedules the task to the next weekday (customizable with
`org-recur-weekday-recurrence'). Also possible is the 'N1,N2,...'
syntax, wherein the earliest date string among the set of N is
selected. For example, '|Mon,Fri|' indicates that the task should
recur every Monday and Friday, and the soonest among them is
chosen when calling `org-recur-finish'.

If the task does not contain org-recur syntax, then depending on
the values of `org-recur-finish-done' and
`org-recur-finish-archive' change the task status to DONE and/or
archive it, respectively"
  (interactive)
  (let* ((is-agenda (string= "org-agenda-mode" major-mode))
         (heading (if is-agenda
                      ;; FIXME: Find a more robust way of getting the header
                      ;; from org-agenda view? This approach seems sufficient so
                      ;; far though.
                      (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))
                    (substring-no-properties (org-get-heading))))
         (next-date (org-recur--get-next-date heading)))
    (cond (next-date
           (if is-agenda
               (org-agenda-schedule nil next-date)
             (org-schedule nil next-date)))
          (t
           (when org-recur-finish-done
             (if is-agenda
                 (org-agenda-todo 'done)
               (org-todo 'done)))
           (when org-recur-finish-archive
             (if is-agenda
                 (org-agenda-archive)
               (org-archive-subtree)))))))

;;;###autoload
(add-hook 'org-mode-hook 'org-recur-mode)
;;;###autoload
(add-hook 'org-agenda-mode-hook 'org-recur-mode)

;;;###autoload
(add-hook 'org-agenda-finalize-hook
          (lambda ()
            (highlight-regexp org-recur--regexp 'org-recur)))

;;;###autoload
(define-minor-mode org-recur-mode
  "Highlight org-recur dates and add commands for rescheduling tasks.

With a prefix argument ARG, enable org-recur mode if ARG is positive, and
disable it otherwise. If called from Lisp, enable the mode if ARG is omitted or
nil, and toggle it if ARG is `toggle'."
  :init-value nil
  :lighter ""
  :keymap nil
  (org-recur--turn-off)
  (when org-recur-mode
    (org-recur--turn-on))
  (when font-lock-mode
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (with-no-warnings (font-lock-fontify-buffer)))))

(provide 'org-recur)
;;; org-recur.el ends here
