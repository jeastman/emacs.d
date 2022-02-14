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

(defvar jme:agenda-files (list "/calendar.org" "/tasks.org" "/notes.org"))

(defun jme:agenda-files ()
  "Construct list of files for org agenda."
  (mapcar (lambda(file) (concat org-directory file)) jme:agenda-files))

;; Original value of org-modules
;; (org-w3m org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-eww)
(custom-set-variables
 '(org-modules '(org-annotate-file
                 ol-bibtex
                 ol-eww))
 '(org-agenda-files jme:agenda-files)
 '(org-default-notes-file (concat org-directory "/notes.org"))
 '(org-startup-folded nil)
 '(org-ellipsis " ▾")
 '(org-hide-leading-stars t)
 '(org-hide-emphasis-markers t)
 '(org-catch-invisible-edits 'smart)
 '(org-src-fontify-natively t)
 '(org-src-tab-acts-natively t)
 '(org-refile-targets '((org-agenda-files . (:maxlevel . 5))))
 '(org-refile-use-outline-path 'file)
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-allow-creating-parent-nodes 'confirm)
 '(org-agenda-span 'day)
 '(org-agenda-restore-windows-after-quit t)
 '(org-agenda-start-with-log-mode t)
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

(use-package ob-mermaid
  :config
  (add-to-list 'org-babel-load-languages '(mermaid . t))
  (org-babel-do-load-languages
   'org-babal-load-languages
   'org-babal-load-languages))

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
(add-to-list 'org-latex-classes
             '("jme-org-article" "\\documentclass[11pt,a4paper]{article}
\\usepackage[T1]{fontenc}
\\usepackage{fontspec}
\\usepackage{graphicx}
\\defaultfontfeatures{Mapping=tex-text}
\\setromanfont{Noto Serif}
\\setromanfont [BoldFont={Noto Serif Bold},
                ItalicFont={Noto Serif Italic}]{Noto Serif}
\\setsansfont{Noto Sans}
\\setmonofont[Scale=0.8]{Noto Mono}
\\usepackage{geometry}
\\geometry{a4paper, textwidth=6.5in, textheight=10in,
            marginparsep=7pt, marginparwidth=.6in}
\\pagestyle{empty}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(setq  org-latex-pdf-process
       '("latexmk -shell-escape -bibtex -pdf %f"))

;; (setq org-latex-pdf-process
;;   '("xelatex -interaction nonstopmode %f"
;;      "xelatex -interaction nonstopmode %f")) ;; for multiple passes


;; TODO Keywords

(custom-set-variables '(org-todo-keywords
                        '((sequence "TODO(t)" "NEXT(n!)" "STARTED(s!)" "WAITING(w@/!)" "STALLED(x@/!)" "|" "DONE(d!)" "CANCELLED(c@)")
                          (sequence "TASK(f)" "|" "DONE(d!)")
                          (sequence "MAYBE(m)" "|" "DONE(d!)" "CANCELLED(c@)")
                          (sequence "RISK(r)" "|" "MITIGATED(i@)"))))

;; Setup for capture

;; Load code review functions needed for capture tempates
(jme:load jme:config-dir "code-review")

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
          )))

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

(use-package org-superstar
  :commands (org-superstar-mode)
  :custom
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))
  :hook (org-mode . (lambda () (org-superstar-mode 1))))

(use-package org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :custom
  (org-priority-faces (quote ((65 . "red") (66 . "orange") (67 . "orange"))))
  (org-fancy-priorities-list `(,(all-the-icons-faicon "bell")
                               ,(all-the-icons-faicon "bell-o")
                               ,(all-the-icons-faicon "bell-slash-o"))))

(use-package org-sticky-header
  :commands (org-sticky-header-mode)
  :hook (org-mode . org-sticky-header-mode)
  :custom
  (org-sticky-header-full-path 'full "Show the full outline path."))

;; toggle visibility of emphasis markers
(use-package org-appear
  :hook (org-mode . org-appear-mode))

;; Support conversion of code blocks to HTML for export.
(use-package htmlize)

(defvar jme:org-archive-expiry-days 14
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
  "Look for completed tasks in the current buffer and archive them."
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

;; Handy function to remove org links from the buffer.
;; This is particularly handy when composing email (using org-msg)
;; and need to remove links to local content from copied text.
;; Inspired by (and largely copied from) stackexchange answer by Andrew Swann
;; https://emacs.stackexchange.com/questions/10707/in-org-mode-how-to-remove-a-link
(defun jme:org-replace-links-by-description (&optional start end)
  "Look for all org links in the current buffer and replace with their description, optionally using region defined by START and END."
  (interactive
   (if (use-region-p) (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward org-link-bracket-re nil t)
        (replace-match (match-string-no-properties
                        (if (match-end 2) 2 1)))))))

;; Clocking

(org-clock-persistence-insinuate)  ; set clocks up to persist

;; TODO - Some keybindings are needed
(use-package counsel-org-clock)

(use-package verb
  :after org
  :config
  (add-to-list 'verb-content-type-handlers '("text/javascript" verb--handler-json))
  (add-to-list 'verb-content-type-handlers '("application/soap+xml" xml-mode))
  (define-key org-mode-map (kbd "C-c r") verb-command-map))

(use-package org-web-tools
  :after org
  :commands (org-web-tools-insert-link-for-url
             org-web-tools-insert-web-page-as-entry
             org-web-tools-read-url-as-org
             org-web-tools-convert-links-to-page-entries
             org-web-tools-archive-attach
             org-web-tools-archive-view))

(use-package org-ref
  :after org
  :defer t
  :init
  (setq org-ref-completion 'org-ref-ivy-cite))

(use-package org-drill
  :after org
  :commands (org-drill))

;; Agenda
(use-package org-super-agenda
  :bind ("C-c a" . jme:org-agenda-all)
  :config
  (defun jme:org-agenda-all ()
    "Show the full agenda with special view."
    (interactive)
    (org-agenda nil "z"))
  (setq org-agenda-custom-commands
      '(("z" "Full agenda super view"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                                :time-grid t
                                :date today
                                :scheduled today
                                :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "Inbox"
                                 :category "Inbox"
                                 :order 1)
                          (:name "Overdue"
                                 :deadline past
                                 :order 2)
                          (:name "Due Today"
                                 :deadline today
                                 :order 3)
                          (:name "Next to do"
                                 :todo "NEXT"
                                 :order 11)
                          (:name "Due Soon"
                                 :deadline future
                                 :order 12)
                          (:name "Important"
                                 :priority "A"
                                 :order 13)
                          (:name "Stalled"
                                 :todo "STALLED"
                                 :order 20)
                          (:name "Risks"
                                 :todo "RISK"
                                 :order 30)
                          (:name "Started"
                                 :todo "STARTED"
                                 :order 40)
                          (:name "Tasks"
                                 :todo "TASK"
                                 :order 50)
                          (:name "Maybe"
                                 :todo "MAYBE"
                                 :order 60)
                          (:name "Waiting"
                                 :todo "WAITING"
                                 :order 70)
                          (:name "Projects"
                                 :children todo
                                 :order 80)
                          (:discard (:tag ("meeting")))))))))))
  (org-super-agenda-mode))

;; Visual Tweaks

;;; Style org buffers for readability
;;; inspired by:
;;; github.com/jethrokuan
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(defun jme:style-org ()
  "Apply customized styling to org."
;  (setq line-spacing 0.2)
  (variable-pitch-mode t)
  (visual-line-mode t))

(add-hook 'org-mode-hook #'jme:style-org)
;; Key bindings

;; Org mode
(bind-key "C-c t" 'org-time-stamp-inactive org-mode-map)

;; Global
(bind-key "C-c l" 'org-store-link)
(bind-key "C-c L" 'org-insert-link-global)
(bind-key "C-c c" 'counsel-org-capture)

;;; orgmode.el ends here
