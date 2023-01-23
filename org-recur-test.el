;;; org-recur-test.el --- Tests for org-recur.
;;
;;; Commentary:
;;
;; Unit tests for the package `org-recur'.
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
           '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri")))
  ;; (should (equal (org-recur--recurrence-to-options
  ;;                 "13:00 Sun,10:00 Wkdy")
  ;;          '("13:00 Sun" "10:00 Mon" "10:00 Tue" "10:00 Wed" "10:00 Thu" "10:00 Fri")))
  ;; (should (equal (org-recur--recurrence-to-options
  ;;                 "Wkdy 10:00,Sat 13:00")
  ;;          '("Mon 10:00" "Tue 10:00" "Wed 10:00" "Thu 10:00" "Fri 10:00" "Sat 13:00")))
  )

(provide 'org-recur-test)
;;; org-recur-test.el ends here
