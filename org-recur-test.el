;;; org-recur-test.el --- Tests for org-recur -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Unit tests for the package `org-recur'.
;;
;; How to run:
;; <https://www.gnu.org/software/emacs/manual/html_node/ert/Running-Tests-Interactively.html>
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

(require 'org-recur)

;; Tests

(ert-deftest recurrence-to-options-test ()
  (should (equal (org-recur--recurrence-to-options
                  "Tue")
           '("Tue")))
  (should (equal (org-recur--recurrence-to-options
                  "Tue,Wed")
           '("Tue" "Wed")))
  (should (equal (org-recur--recurrence-to-options
                  "14:00 Tue,Wed 15:00")
           '("14:00 Tue" "Wed 15:00")))
  (should (equal (org-recur--recurrence-to-options
                  "Tue 14:00,15:00 Wed")
           '("Tue 14:00" "15:00 Wed")))
  (should (equal (org-recur--recurrence-to-options
                  "Sun,Wkdy")
           '("Sun" "mon" "tue" "wed" "thu" "fri")))
  (should (equal (org-recur--recurrence-to-options
                  "13:00 Sun,10:00 Wkdy")
           '("13:00 Sun" "10:00 mon" "10:00 tue" "10:00 wed" "10:00 thu" "10:00 fri")))
  (should (equal (org-recur--recurrence-to-options
                  "Wkdy 10:00,Sat 13:00")
           '("mon 10:00" "tue 10:00" "wed 10:00" "thu 10:00" "fri 10:00" "Sat 13:00")))
  (should (equal (org-recur--recurrence-to-options
                  "10:00 Wkdy,Wkdy 12:00")
           '("10:00 mon" "10:00 tue" "10:00 wed" "10:00 thu" "10:00 fri" "mon 12:00" "tue 12:00" "wed 12:00" "thu 12:00" "fri 12:00")))
  )

(ert-deftest recurrence-to-options-case-fold-test ()
  ;; Wkdy must expand regardless of the user's case-fold-search setting.
  (let ((case-fold-search nil))
    (should (equal (org-recur--recurrence-to-options
                    "Sun,WKDY")
             '("Sun" "mon" "tue" "wed" "thu" "fri")))))

(ert-deftest recurrence-to-options-weekday-literal-test ()
  ;; A customized weekday string is matched literally, not as a regexp.
  (let ((org-recur-weekday "w.d"))
    (should (equal (org-recur--recurrence-to-options
                    "wxd")
             '("wxd")))
    (should (equal (org-recur--recurrence-to-options
                    "w.d")
             '("mon" "tue" "wed" "thu" "fri"))))
  ;; The weekday string only matches as a whole word.
  (should (equal (org-recur--recurrence-to-options
                  "wkdyy")
           '("wkdyy"))))

(ert-deftest date-less-p-test ()
  ;; A nil or empty D2 is considered greater than any date string.
  (should (org-recur--date-less-p "+1" nil))
  (should (org-recur--date-less-p "+1" ""))
  ;; A nil or empty D1 is never less than a date string.
  (should-not (org-recur--date-less-p nil "+1"))
  (should-not (org-recur--date-less-p "" "+1"))
  ;; Ordinary comparisons.
  (should (org-recur--date-less-p "+1" "+2"))
  (should-not (org-recur--date-less-p "+2" "+1"))
  (should-not (org-recur--date-less-p "+1" "+1")))

(ert-deftest get-next-date-test ()
  ;; No recurrence syntax.
  (should-not (org-recur--get-next-date "no recurrence here"))
  ;; Empty recurrence syntax is treated as no recurrence.
  (should-not (org-recur--get-next-date "task || foo"))
  ;; Single option.
  (should (equal (org-recur--get-next-date "task |+2| foo") "+2"))
  ;; The earliest option wins, regardless of order.
  (should (equal (org-recur--get-next-date "task |+1,+2| foo") "+1"))
  (should (equal (org-recur--get-next-date "task |+2,+1| foo") "+1")))

(provide 'org-recur-test)
;;; org-recur-test.el ends here
