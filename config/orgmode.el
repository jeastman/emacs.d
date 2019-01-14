;;; orgmode.el --- Org-mode Configuration -*- lexical-binding: t -*-

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

;; Original value of org-modules
;; (org-w3m org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-eww)

(custom-set-variables
 '(org-modules '(org-annotate-file
                 org-bibtex
                 org-eww
                 org-list))
 '(org-agenda-files (list (concat org-directory "/tasks.org")
                          (concat org-directory "/notes.org")
                          (concat org-directory "/calendar.org")))
 '(org-default-notes-file (concat org-directory "/notes.org"))
 '(org-startup-folded nil)
 '(org-catch-invisible-edits 'smart)
 '(org-src-fontify-natively t)
 '(org-src-tab-acts-natively t)
 '(org-refile-targets '((org-agenda-files . (:maxlevel . 5))))
 '(org-refile-use-outline-path 'file)
 '(org-outline-path-complete-in-steps t)
 '(org-refile-allow-creating-parent-nodes 'confirm)
 '(org-log-done 'time)
 '(org-log-into-drawer "LOGBOOK")
 '(org-global-properties
      '(("Effort_ALL" .
         "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")))
 '(org-columns-default-format "%50ITEM(Task) %TODO %2PRIORITY %10Effort(Effort){:} %10CLOCKSUM %TAGS")
 '(org-babel-load-languages
   '((emacs-lisp . t)
     (ditaa . t)
     (dot . t)
     (gnuplot . t)
     (plantuml . t)
     (python . t)
     (js . t)
     (lisp . t)
     (shell . t))))

(org-babel-do-load-languages
 'org-babal-load-languages
 'org-babal-load-languages)

;; TODO Keywords

(custom-set-variables '(org-todo-keywords
                        '((sequence "TODO(t)" "STARTED(s!)" "WAITING(w@/!)" "STALLED(x@/!)" "|" "DONE(d!)" "CANCELLED(c@)")
                          (sequence "TASK(f)" "|" "DONE(d!)")
                          (sequence "MAYBE(m)" "|" "DONE(d!)" "CANCELLED(c@)")
                          (sequence "RISK(r)" "|" "MITIGATED(i@)")))
                      '(org-todo-keyword-faces
                        '(("TODO" . (:foreground "DarkOrange" :weight bold))
                          ("STARTED" . (:foreground "DarkOrange" :weight bold))
                          ("WAITING" . (:foreground "gold" :weight bold))
                          ("MAYBE" . (:foreground "spring green"))
                          ("DONE" . (:foreground "dark sea green"))
                          ("STALLED" . (:foreground "slate grey"))
                          ("CANCELLED" . (:foreground "slate grey"))
                          ("TASK" . (:foreground "DeepSkyBlue"))
                          ("RISK" . (:foreground "white" :background "orange red"))
                          ("MITIGATED" . (:foreground "white" :background "dark green")))))

(use-package org-bullets
  :commands (org-bullets-mode)
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

(use-package org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (custom-set-variables
   '(org-priority-faces (quote ((65 . "red") (66 . "yellow") (67 . "orange"))))
   '(org-fancy-priorities-list '( "!" "⌚" "⌛" ))))

(use-package org-sticky-header
  :commands (org-sticky-header-mode)
  :hook (org-mode . org-sticky-header-mode)
  :custom
  (org-sticky-header-full-path 'full "Show the full outline path.")
  (org-sticky-heading-star "-"))

(defvar jme:org-archive-expiry-days 7
  "The number of days after which a completed task should be auto-archived.
This can be 0 for immediate, or a floating point value.")

(defun jme:archive-p (p)
  "Determine if the headline at point P needs to be archived."
  (let* ((props (org-entry-properties p))
         (closed (assoc "CLOSED" props)))
    (if closed
        (let ((when-closed (org-parse-time-string (cdr closed))))
          (if (>= (time-to-number-of-days (time-subtract (current-time)
                                                         (apply #'encode-time when-closed)))
                  jme:org-archive-expiry-days)
              t)))))

(defun jme:does-item-need-archive? ()
  "Does item at point need to be archived?"
  (interactive)
  (if (jme:archive-p (point))
      (message "Yes")
    (message "No")))

(defun jme:org-archive-done-tasks ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((done-regexp
           (concat "\\* \\(" (regexp-opt org-done-keywords) "\\) "))
          (state-regexp
           (concat "- State \"\\(" (regexp-opt org-done-keywords)
                   "\\)\"\\s-*\\[\\([^]\n]+\\)\\]")))
      (while (re-search-forward done-regexp nil t)
        (let ((end (save-excursion
                     (outline-next-heading)
                     (point)))
              begin)
          (goto-char (line-beginning-position))
          (setq begin (point))
          (if (jme:archive-p begin)
              (org-archive-subtree)
            (goto-char end)))))
    (save-buffer)))

(custom-set-variables '(safe-local-variable-values
                        (quote ((after-save-hook archive-done-tasks)))))

(defalias 'archive-done-tasks 'jme:org-archive-done-tasks)

(defun jme:org-archive-and-publish ()
  "Archive old tasks and publish as html."
  (interactive)
  (archive-done-tasks)
  (org-html-export-to-html))

;; Clocking

(org-clock-persistence-insinuate)  ; set clocks up to persist

;; TODO - Some keybindings are needed
(use-package counsel-org-clock)

;; Key bindings

;; Org mode
(bind-key "C-c t" 'org-time-stamp-inactive org-mode-map)

;; Global
(bind-key "C-c l" 'org-store-link)
(bind-key "C-c L" 'org-insert-link-global)
(bind-key "C-c a" 'org-agenda)
(bind-key "C-c c" 'org-capture)

;;; orgmode.el ends here
