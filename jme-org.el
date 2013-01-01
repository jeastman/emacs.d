(require 'org)
;; Org-mode settings
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org.txt$" . org-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-log-done 'time)
(setq org-startup-folded 'content)
;(setq org-agenda-include-diary t)


(setq diary-file (concat org-directory "/diary"))
(setq org-mobile-inbox-for-pull (concat org-directory "/flagged.org"))
(setq org-agenda-files (list (concat org-directory "/work.org")
                             (concat org-directory "/tasks.org")
                             (concat org-directory "/home.org")
                             (concat org-directory "/knights.org")
                             (concat org-directory "/someday.org")
                             (concat org-directory "/notes.org")))
(setq org-default-notes-file (concat org-directory "/notes.org"))

; Use IDO for target completion
(setq org-completion-use-ido t)
; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))
; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))
; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)
; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Workflows

(setq org-todo-keywords
      '((sequence "PROJECT(p)" "|" "FINISHED(f)")
        (sequence "TODO(t)" "|" "NOTE(n)" "DONE(d)")
        (sequence "TODO(t)" "APPT(a)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)" "DEFERRED(r)")))

(setq org-global-properties
      '(("Effort_ALL". "0 0:15 0:30 1:00 2:00 3:00 4:00")))

;; TEMPLATES
(setq org-capture-templates
      '(("m" "Mezeo Appointment" entry (file+headline
                                  (concat org-directory "/work.org") "General")
         "* APPT %^{Description} %^g\n  %?\n  Added: %U")
        ("a" "Appointment" entry (file+headline
                                  (concat org-directory "/tasks.org") "Calendar")
         "* APPT %^{Description} %^g\n  %?\n  Added: %U")
        ("t" "Todo" entry (file+headline
                           (concat org-directory "/tasks.org") "Tasks")
         "* TODO %^{Brief Description} %^g\n  %?\n  Added: %U")
        ("n" "Notes" entry (file+datetree
                              (concat org-directory "/notes.org"))
         "* %^{Description} %^g\n  %?\n  Added: %U")))

;; Custom agenga commands
(setq org-agenda-custom-commands
      '(
        ("P" "Projects"
         ((tags "PROJECT")))
        ("H" "Office and Home Lists"
         ((agenda)
          (tags-todo "OFFICE")
          (tags-todo "HOME")
          (tags-todo "COMPUTER")
          (tags-todo "PHONE")))
        ("D" "Daily Action List"
         (
          (agenda "" ((org-agenda-ndays 1)
                      (org-agenda-sorting-strategy
                       (quote ((agenda time-up priority-down tag-up) )))
                      (org-deadline-warning-days 0)
                      ))
          ))
        ("I" "Import diary from iCal" agenda ""
         ((org-agenda-mode-hook
           (lambda ()
             (org-mac-iCal)))))
        ))

(add-hook 'org-agenda-cleanup-fancy-diary-hook
          (lambda ()
            (goto-char (point-min))
            (save-excursion
              (while (re-search-forward "^[a-z]" nil t)
                (goto-char (match-beginning 0))
                (insert "0:00-24:00")))
            (while (re-search-forward "^ [a-z]" nil t)
              (goto-char (match-beginning 0))
              (save-excursion
                (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
              (insert (match-string 0)))))

;;
;; Archiving completed tasks
;; see http://article.gmane.org/gmane.emacs.orgmode/3629
;;
;; To set an org file up for archiving you need to add the following at the top
;; of the file (replace archive.text with the archive file):
;;
;; -*- mode: org; after-save-hook: (archive-done-tasks) -*-
;; #+ARCHIVE: archive.txt::
;;

(defvar jme/org-archive-expiry-days 7
  "The number of days after which a completed task should be auto-archived.
This can be 0 for immediate, or a floating point value.")

(defun jme/org-archive-done-tasks ()
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
          (if (re-search-forward state-regexp end t)
              (let* ((time-string (match-string 2))
                     (when-closed (org-parse-time-string time-string)))
                (if (>= (time-to-number-of-days
                         (time-subtract (current-time)
                                        (apply #'encode-time when-closed)))
                        jme/org-archive-expiry-days)
                    (org-archive-subtree)))
            (goto-char end)))))
    (save-buffer)))

(setq safe-local-variable-values (quote ((after-save-hook archive-done-tasks))))
(defalias 'archive-done-tasks 'jme/org-archive-done-tasks)

;; Uncomment the following to always use flyspell in org mode
;(add-hook 'org-mode-hook 'turn-on-flyspell)

(if (eq window-system 'ns)
    (progn
      (require 'org)
      (add-to-list 'org-modules 'org-mac-iCal)
      )
    )
