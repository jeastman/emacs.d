;; jme-org.el --- OrgMode configuration. -*- lexical-binding: t; -*-

;; Author: John Eastman
;; Created: 12 Feb 2022

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

;;; Code:
(require 'straight)
(require 'jme-common)

(straight-use-package 'org)
(straight-use-package 'org-contrib)
(straight-use-package 'htmlize)
(straight-use-package '(org-modern :type git :host github :repo "minad/org-modern"))

(defalias 'archive-done-tasks 'jme-org-archive-done-tasks)

(defcustom jme-org-archive-expiry-days 14
  "The number of days after which a completed task should be auto-archived.
This can be 0 for immediate, or a floating point value."
  :type 'integer
  :tag "Completed Task Expiry Days"
  :group 'jme-customizations)

;;; See the following - friendly handling of capture-specific frame
;;; https://www.diegoberrocal.com/blog/2015/08/19/org-protocol/
(defadvice org-capture
    (after make-full-window-frame activate)
  "Advise capture to be the only window when used as a popup."
  (if (equal "emacs-capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture to close the frame when done."
  (if (equal "emacs-capture" (frame-parameter nil 'name))
      (delete-frame)))

(defun jme-org--configure-latex ()
  "Configue support for LaTeX documents."
  (defvar org-latex-classes)
  (defvar org-latex-pdf-process)
  (require 'ox-latex)
  (add-to-list 'org-latex-classes
               '("synacororgspec" "\\documentclass[10pt,oneside,article]{synacororgspec}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagrah*{%s}")))
  (add-to-list 'org-latex-classes
               '("zimbraorgspec" "\\documentclass[10pt,oneside,article]{zimbraorgspec}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagrah*{%s}")))
  (setq  org-latex-pdf-process
         '("latexmk -shell-escape -bibtex -pdf %f")))

(defun jme-org--configure-capture ()
  "Org capture configuration."
  (defvar org-directory)
  (defvar org-capture-templates)
  (let ((task-file (expand-file-name (concat org-directory "/tasks.org")))
        (calendar-file (expand-file-name (concat org-directory "/calendar.org")))
        (notes-file (expand-file-name (concat org-directory "/notes.org"))))
    (setq org-capture-templates
          `(("t" "Task" entry (file+headline ,task-file "Inbox")
             "* TODO %^{What} %^g\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n" :immediate-finish t)
            ("e" "Task from Email" entry (file+headline ,task-file "Email")
             "* TODO [#A] %?\nSCHEDULED: %(org-insert-timestamp (org-read-date nil t \"+0d\"))\n%a\n")
            ("d" "Deadline" entry (file+headline ,task-file "Tasks")
             "* TODO %^{What} %^g\nDEADLINE: %^{Deadline}t\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n" :immediate-finish t)
            ("m" "Ad-hoc meeting" entry (file+olp ,calendar-file "Meetings" "Other Discussions")
             "* TODO Discussion with %^{Who} %^g\n" :clock-in t :clock-keep t :immediate-finish t)
            ("c" "Item to Current Clocked Task" item (clock)
             "%i" :immediate-finish :empty-lines 1)
            ("C" "Contents to Current Clocked Task" plain (clock)
             "%i" :immediate-finish t :empty-lines 1)
            ("k" "Kill-ring to Current Clocked Task" plain (clock)
             "%c" :immediate-finish t :empty-lines 1)
            ("F" "Code Reference with Comments to Current Task" plain (clock)
             "%(jme:org-capture-code-snippet \"%F\")\n\n   %?"
             :empty-lines 1)
            ("f" "Code Reference to Current Task" plain (clock)
             "%(jme:org-capture-code-snippet \"%F\")"
             :empty-lines 1 :immediate-finish t)
            ("1" "One-on-one meeting" entry (file+olp ,calendar-file "Meetings" "Other Discussions")
             "* TODO Discussion with %^{Who} %^g\nSCHEDULED: %^{When}T\n" :immediate-finish t)
            ("P" "Project meeting" entry (file+olp ,calendar-file "Meetings" "Project Meetings")
             "* TODO %^{Project} %^g\nSCHEDULED: %^{When}T\n" :immediate-finish t)
            ("e" "Event" entry (file+olp ,calendar-file "Meetings" "Events")
             "* TODO %^{What} %^g\nSCHEDULED: %^{When}T\n:PROPERTIES:\n:CATEGORY: %^{Type|Meeting|Event|Other}\n:END:\n" :immediate-finish t)
            ("p" "Protocol" entry (file+headline ,task-file "Inbox")
             "* %^{Title}\n:PROPERTIES:\n:CREATED:  %U\n:END:\nSource: %:annotation\n #+begin_quote\n%i\n#+end_quote\n\n%?")
            ("x" "Protocol with url" entry (file+headline ,notes-file "Inbox")
             "* %:description\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\nLink: %l\n\n#+begin_quote\n%i\n#+end_quote\n\n%?")
            ("z" "Protocol no prompt" entry (file+headline ,task-file "Inbox")
             "* %:description\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n#+begin_quote\n%i\n#+end_quote\n\n%?")
            ("s" "Protocol to clock" plain (clock)
             "Source: %u, %:annotation\n#+begin_quote\n%i#+end_quote\n" :immediate-finish t :empty-lines 1)
            ("L" "Prrotocol Link" entry (file+headline ,task-file "Inbox")
             "* %? [[%:link][%:description]] \n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n")
            ))))

(defun jme-org-archive-p (p)
  "Determine if the headline at point P needs to be archived."
  (declare-function org-entry-properties "org" (&optional POM WHICH))
  (declare-function org-parse-time-string "org" (S &optional NODEFAULT))
  (declare-function time-to-number-of-days "time-date" (TIME))
  (let* ((props (org-entry-properties p))
         (closed (assoc "CLOSED" props)))
    (if closed
        (let ((when-closed (org-parse-time-string (cdr closed))))
          (if (>= (time-to-number-of-days (time-subtract (current-time)
                                                         (apply #'encode-time when-closed)))
                  jme-org-archive-expiry-days)
              t)))))

(defun jme-org-does-item-need-archive? ()
  "Does item at point need to be archived?"
  (interactive)
  (if (jme-org-archive-p (point))
      (message "Yes")
    (message "No")))

(defun jme-org-archive-done-tasks ()
  "Archive tasks which are done."
  (interactive)
  (defvar org-done-keywords)
  (declare-function outline-next-heading "outline" ())
  (declare-function org-archive-subtree "org-archive" (&optional FIND-DONE))
  (save-excursion
    (goto-char (point-min))
    (let ((done-regexp
           (concat "\\* \\(" (regexp-opt org-done-keywords) "\\) ")))
      (while (re-search-forward done-regexp nil t)
        (let ((end (save-excursion
                     (outline-next-heading)
                     (point)))
              begin)
          (goto-char (line-beginning-position))
          (setq begin (point))
          (if (jme-org-archive-p begin)
              (org-archive-subtree)
            (goto-char end)))))
    (save-buffer)))

(defun jme-org-archive-and-publish ()
  "Archive old tasks and publish as html."
  (interactive)
  (declare-function org-html-export-to-html
                    "ox-html"
                    (&optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST))
  (jme-org-archive-done-tasks)
  (org-html-export-to-html))

 (defun jme-org-style-org ()
   "Apply customized styling to org."
   (setq line-spacing 0.2)
   (jme-common-enable-mode variable-pitch-mode)
   (jme-common-enable-mode visual-line-mode)
   (jme-common-enable-mode org-modern-mode)
   (jme-common-enable-mode olivetti-mode))

(defun jme-org--enable ()
  "Configure org mode."
  (custom-set-variables
   ;; Adapt indentation to outline node level
   '(org-adapt-indentation t)
   ;; Don't keep tags aligned (for org-modern)
   '(org-auto-align-tags nil)
   ;; Check invisible regions before editing
   '(org-catch-invisible-edits 'show-and-error)
   ;; Fall-back file for org-capture
   '(org-default-notes-file (concat org-directory "/notes.org"))
   ;; The ellipsis to use in org
   '(org-ellipsis " ▾")
   ;; Hide markup when viewing (for org-modern)
   '(org-hide-emphasis-markers t)
   ;; Insert new headings after the current subtree
   '(org-insert-heading-respect-content t)
   ;; Record time when an item is done
   '(org-log-done 'time)
   ;; Drawer to log info into
   '(org-log-into-drawer "LOGBOOK")
   ;; Handle outline path in one step
   '(org-outline-path-complete-in-steps nil)
   ;; Show entities as UTF-8 characters
   '(org-pretty-entities t)
   ;; Allow creation of headlines when refiling
   '(org-refile-allow-creating-parent-nodes 'confirm)
   ;; limit refiling to agenda files
   '(org-refile-targets '((org-agenda-files . (:maxlevel . 5))))
   ;; Provide refile targets as paths
   '(org-refile-use-outline-path 'file)
   ;; Make ctrl-a/ctrl-e headline aware
   '(org-special-ctrl-a/e t)
   ;; Fontify code in source blocks
   '(org-src-fontify-natively t)
   ;; Use the native language tabs in code blocks
   '(org-src-tab-acts-natively t)
   ;; Don't switch to overview when loading org files
   '(org-startup-folded nil)
   ;; Column for tags (for org-modern)
   '(org-tags-column 0)
   '(org-modules '(org-annotate-file
                   ol-bibtex
                   ol-eww))
   ;; Languages to use for babel
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

  ;; Workflow steps
  (custom-set-variables '(org-todo-keywords
                          '((sequence "TODO(t)" "NEXT(n!)" "STARTED(s!)" "WAITING(w@/!)" "STALLED(x@/!)" "|" "DONE(d!)" "CANCELLED(c@)")
                            (sequence "TASK(f)" "|" "DONE(d!)")
                            (sequence "MAYBE(m)" "|" "DONE(d!)" "CANCELLED(c@)")
                            (sequence "RISK(r)" "|" "MITIGATED(i@)"))))

  (org-babel-do-load-languages
   'org-babal-load-languages
   'org-babal-load-languages)

  (jme-org--configure-latex)
  (jme-org--configure-capture)

  (custom-set-variables '(safe-local-variable-values
                          (quote ((after-save-hook archive-done-tasks)))))

  (org-clock-persistence-insinuate)  ; set clocks up to persist

  (add-hook 'org-mode-hook #'jme-org-style-org)

  ;; Org-modern customization
  (custom-set-variables
   '(org-modern-star '["◉" "○" "●" "○" "●" "○" "●"]))

  ;; Org mode
  (defvar org-mode-map)
  (declare-function org-time-stamp-inactive "org" (ARG))
  (define-key org-mode-map (kbd "C-c t") #'org-time-stamp-inactive)

  ;; Global
  (declare-function org-store-link "ol" (ARG &optional INTERACTIVE))
  (declare-function org-insert-link-global "ol" ())
  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c L") #'org-insert-link-global)
  (global-set-key (kbd "C-c c") #'org-capture))

(defun jme-org--disable ()
  "Disable org configuration."
  ;; TODO
  (custom-set-variables
   '(safe-local-variable-values nil)
   '((sequence "TODO" "DONE")))
  (defvar org-mode-map)
  (define-key org-mode-map (kbd "C-c t") nil)
  (global-unset-key (kbd "C-c l"))
  (global-unset-key (kbd "C-c L"))
  (global-unset-key (kbd "C-c c"))
  (remove-hook 'org-mode-hook #'jme-org-style-org))

(defun jme-org-unload-function ()
  "Unload org configuration."
  (jme-org--disable))

(jme-common-defconfiguration jme-org "Org-mode configuration")

(provide 'jme-org)
;;; jme-org.el ends here.
