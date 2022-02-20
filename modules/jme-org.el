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
(straight-use-package 'org-superstar)

(defun jme-org--superstar-mode-function ()
  "Hook function to enable `superstar-mode'."
  (jme-common-enable-mode org-superstar-mode))

;; (use-package org-ref
;;   :after org
;;   :defer t
;;   :init
;;   (setq org-ref-completion 'org-ref-ivy-cite))

;; Agenda
;; (use-package org-super-agenda
;;   :bind ("C-c a" . jme:org-agenda-all)
;;   :config
;;   (defun jme:org-agenda-all ()
;;     "Show the full agenda with special view."
;;     (interactive)
;;     (org-agenda nil "z"))
;;   (setq org-agenda-custom-commands
;;       '(("z" "Full agenda super view"
;;          ((agenda "" ((org-agenda-span 'day)
;;                       (org-super-agenda-groups
;;                        '((:name "Today"
;;                                 :time-grid t
;;                                 :date today
;;                                 :scheduled today
;;                                 :order 1)))))
;;           (alltodo "" ((org-agenda-overriding-header "")
;;                        (org-super-agenda-groups
;;                         '((:name "Inbox"
;;                                  :category "Inbox"
;;                                  :order 1)
;;                           (:name "Overdue"
;;                                  :deadline past
;;                                  :order 2)
;;                           (:name "Due Today"
;;                                  :deadline today
;;                                  :order 3)
;;                           (:name "Next to do"
;;                                  :todo "NEXT"
;;                                  :order 11)
;;                           (:name "Due Soon"
;;                                  :deadline future
;;                                  :order 12)
;;                           (:name "Important"
;;                                  :priority "A"
;;                                  :order 13)
;;                           (:name "Stalled"
;;                                  :todo "STALLED"
;;                                  :order 20)
;;                           (:name "Risks"
;;                                  :todo "RISK"
;;                                  :order 30)
;;                           (:name "Started"
;;                                  :todo "STARTED"
;;                                  :order 40)
;;                           (:name "Tasks"
;;                                  :todo "TASK"
;;                                  :order 50)
;;                           (:name "Maybe"
;;                                  :todo "MAYBE"
;;                                  :order 60)
;;                           (:name "Waiting"
;;                                  :todo "WAITING"
;;                                  :order 70)
;;                           (:name "Projects"
;;                                  :children todo
;;                                  :order 80)
;;                           (:discard (:tag ("meeting")))))))))))
;;   (org-super-agenda-mode))


(defalias 'archive-done-tasks 'jme-org-archive-done-tasks)

(defcustom jme-org-agenda-files '( "calendar.org" "tasks.org" "notes.org")
  "Files always added to the agenda."
  :type 'list
  :group 'jme-customizations)

(defcustom jme-org-archive-expiry-days 14
  "The number of days after which a completed task should be auto-archived.
This can be 0 for immediate, or a floating point value."
  :type 'integer
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

(defun jme-org-agenda-files ()
  "Construct list of files for org agenda."
  (mapcar (lambda (file)
            (concat org-directory "/" file)) jme-org-agenda-files))

(defun jme-org--configure-latex ()
  "Configue support for LaTeX documents."
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
          (if (jme-org-archive-p begin)
              (org-archive-subtree)
            (goto-char end)))))
    (save-buffer)))

(defun jme-org-archive-and-publish ()
  "Archive old tasks and publish as html."
  (interactive)
  (jme-org-archive-done-tasks)
  (org-html-export-to-html))

(defun jme-org-style-org ()
  "Apply customized styling to org."
;  (setq line-spacing 0.2)
  (jme-common-enable-mode variable-pitch-mode)
  (jme-common-enable-mode visual-line-mode))

(defun jme-org--enable ()
  "Configure org mode."
  (custom-set-variables
   '(org-modules '(org-annotate-file
                   ol-bibtex
                   ol-eww))
   ;; Initial set of agenda files
   '(org-agenda-files jme-org-agenda-files)
   ;; Fall-back file for org-capture
   '(org-default-notes-file (concat org-directory "/notes.org"))
   ;; Don't switch to overview when loading org files
   '(org-startup-folded nil)
   ;; The ellipsis to use in org
   '(org-ellipsis " ▾")
   ;; Hide leading stars for headlines
   '(org-hide-leading-stars t)
   ;; Hide markup when viewing
   '(org-hide-emphasis-markers t)
   ;; Check invisible regions before editing
   '(org-catch-invisible-edits 'smart)
   ;; Fontify code in source blocks
   '(org-src-fontify-natively t)
   ;; Use the native language tabs in code blocks
   '(org-src-tab-acts-natively t)
   ;; limit refiling to agenda files
   '(org-refile-targets '((org-agenda-files . (:maxlevel . 5))))
   ;; Provide refile targets as paths
   '(org-refile-use-outline-path 'file)
   ;; Handle outline path in one step
   '(org-outline-path-complete-in-steps nil)
   ;; Allow creation of headlines when refiling
   '(org-refile-allow-creating-parent-nodes 'confirm)
   ;; How much to display in overview
   '(org-agenda-span 'day)
   ;; Restore window configuration after exiting agenda
   '(org-agenda-restore-windows-after-quit t)
   ;; Use log mode by default in agenda
   '(org-agenda-start-with-log-mode t)
   ;; Record time when an item is done
   '(org-log-done 'time)
   ;; Drawer to log info into
   '(org-log-into-drawer "LOGBOOK")
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

  ;; superstars
  (custom-set-variables
   '(org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

  (add-hook 'org-mode-hook #'jme-org--superstar-mode-function)

  ;; Org mode
  (define-key org-mode-map (kbd "C-c t") #'org-time-stamp-inactive)

  ;; Global
  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c L") #'org-insert-link-global)
  (global-set-key (kbd "C-c c") #'counsel-org-capture))

(defun jme-org--disable ()
  "Disable org configuration."
  ;; TODO
  (remove-hook 'org-mode-hook #'jme-org-style-org)
  (remove-hook 'org-mode-hook #'jme-org--superstar-mode-function))

(defun jme-org-unload-function ()
  "Unload org configuration."
  (jme-org--disable))

(jme-common-defconfiguration jme-org "Orgmode configuration")

(provide 'jme-org)
;;; jme-org.el ends here.
