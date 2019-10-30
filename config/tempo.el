;;; tempo.el --- Utilities for temp report -*- lexical-binding: t -*-

;; Author: John Eastman
;; Created: 06 Jan 2019

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this file; see the file COPYING.  If not, see see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:
(require 'org-element)
(require 'org-clock)
(require 'dash)

(defun jme:org-clock-data (tstart tend)
  "Retrieve clock data (headlines only) between TSTART and TEND."
  (let ((files '("calendar.org" "task_archive.txt" "tasks.org"))
        (params (org-combine-plists
                 org-clocktable-defaults
                 `(:scope agenda-with-archive :tags t :tstart ,tstart :tend ,tend :maxlevel 3))))
    (-mapcat #'(lambda (f)
                 (with-current-buffer f
                   (nth 2 (org-clock-get-table-data f params)))) ; 3rd element is headline entries
             files)))

(defun jme:pplmp-code-to-name (code table)
  "Lookup PPLMP CODE and convert to a name from TABLE."
  (or
   (nth
    (cl-position code
                 (mapcar #'(lambda (s) (substring-no-properties s))
                         (org-table-get-remote-range table "@<$1..@>$1"))
                 :test 'equal)
    (mapcar #'(lambda (s) (substring-no-properties s))
            (org-table-get-remote-range table "@<$2..@>$2")))
   code))

(defun jme:org-tags-from-clock-data (data)
  "Return list of tags from clock DATA.
DATA is expected to be the result of call to `jme:org-clock-data'."
  (-distinct
   (-flatten
    (-keep #'(lambda (tag)
               (if (and (stringp tag) (equal "PPLM" (substring tag 0 4)))
                   tag))
           (-flatten
            (-non-nil
             (delete-dups
              (mapcar #'(lambda (x)
                          (nth 2 x)) ; 3rd element is tags - list
                      data))))))))

(defun jme:tempo-org-tags-from-entry (entry)
  "Return list of tags from ENTRY."
  (nth 1 (nth 2 entry)))

(defun jme:org-events-from-clock-data (data &optional tag)
  "Return list of events in DATA possibly filtered by TAG.
DATA is expected to be result of call to `jme:org-clock-data'."
  (-filter #'(lambda (x)
               (if tag
                   (if (member tag (nth 2 x))
                       x
                     nil)
                 x))
           data))

(defun jme:org-duration-from-clock-data-entry (entry)
  "Return the duration of an ENTRY."
  (nth 4 entry))

(defun jme:org-clock-with-duration (date duration)
  "Return a clock string from DATE with DURATION in minutes."
  (let ((fmt (concat "[" (substring (cdr org-time-stamp-formats) 1 -1) "]")))
    (format "CLOCK: %s--%s" (format-time-string fmt date)
            (format-time-string fmt (time-add date (* 60 duration))))))

(defun jme:org-insert-clock-for-clock-data-entry (entry date)
  "Insert clock for ENTRY with DATE."
  (insert (jme:org-clock-with-duration date (jme:org-duration-from-clock-data-entry entry))))

(defun jme:org-total-duration-for-clock-data (data)
  "Return the total duration for events in DATA."
  (seq-reduce #'(lambda (acc x)
                  (+ acc (jme:org-duration-from-clock-data-entry x)))
              data 0))

(defun jme:org-insert-tempo-task-time (data date)
  "For each entry in DATA, insert a clock with DATE."
  (mapc #'(lambda (x)
            (indent-according-to-mode)
            (jme:org-insert-clock-for-clock-data-entry x (apply 'encode-time (org-parse-time-string date)))
            (insert "\n"))
        data)
   ;; round up block time to 30 minute mark
   (indent-according-to-mode)
   (insert (jme:org-clock-with-duration
            (apply 'encode-time (org-parse-time-string date))
            (- 30 (% (jme:org-total-duration-for-clock-data data) 30))))
   (insert "\n"))

(defun jme:org-insert-week-number (datestr)
  "Insert a headline for week number based on DATESTR."
  (org-insert-heading-respect-content)
  (org-demote)
  (insert (concat "Week " (format-time-string "%V" (apply 'encode-time (org-parse-time-string datestr))))))

(defun jme:org-insert-tempo-initial-time-entry (data datestr)
  "Insert a time entry block based on DATA for tempo report specified by DATESTR."
  (insert "\n")
  (indent-according-to-mode)
  (insert ":LOGBOOK:\n")
  (indent-according-to-mode)
  (jme:org-insert-tempo-task-time data datestr)
  (insert "\n")
  (indent-according-to-mode)
  (insert ":END:\n"))

(defun jme:org-insert-tempo-task (data datestr code)
  "Insert a task block for DATA for DATESTR filtered by CODE."
  (let (labelpos)
    (goto-char (point-max))
    (insert "\n**** ")
    (insert (jme:pplmp-code-to-name code "projects"))
    (setq labelpos (point))
    (jme:org-insert-tempo-initial-time-entry data datestr)
    (goto-char labelpos)
    (org-set-tags code)))

(defun jme:org-insert-tempo-date (&optional date)
  "Insert a tempo block based on DATE."
  (interactive)
  (setq date (or date (org-read-date)))
  (let* ((tend (format-time-string " %Y-%m-%d"
                                   (time-add (apply 'encode-time
                                                    (org-parse-time-string date))
                                             (* 60 60 24))))
         (alldata (jme:org-clock-data date tend))
         (tags (jme:org-tags-from-clock-data alldata)))
    (goto-char (point-max))
    (insert "\n*** ")
    (insert date)
    ;; TODO  iterate over each tag and produce a tempo task block
    (mapc #'(lambda (tag)
              (jme:org-insert-tempo-task
                (jme:org-events-from-clock-data alldata tag)
                date tag))
          tags))
  (org-update-all-dblocks))

(defun jme:tempo-org-insert-week (&optional date)
  "Insert a weel block based on DATE."
  (interactive)
  (setq date (or date (org-read-date)))
   (dotimes (day 5)
    (let ((thisday (format-time-string " %Y-%m-%d"
                                       (time-add (apply 'encode-time
                                                        (org-parse-time-string date))
                                                 (* 60 60 24 day)))))
      (jme:org-insert-tempo-date thisday))))

;;; tempo.el ends here
