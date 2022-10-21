;; jme-org-agenda.el --- OrgMode Agenda configuration. -*- lexical-binding: t; -*-

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
(eval-when-compile
  (defvar org-agenda-category-icon-alist)
  (defvar org-agenda-redo-command)
)
(straight-use-package 'org-super-agenda)

(defcustom jme-org-agenda-files '( "calendar.org" "tasks.org" "notes.org")
  "Files always added to the agenda."
  :type 'list
  :tag "Agenda Files"
  :group 'jme-customizations)

(defun jme-org-agenda-files ()
  "Construct list of files for org agenda."
  (defvar org-directory)
  (mapcar (lambda (file)
            (concat org-directory "/" file)) jme-org-agenda-files))

(defun jme-org-agenda--faces ()
  "Create a list of faces to be used in agenda."
  (let ((face-height (face-attribute 'default :height)))
    (list
     (list 'default (list :family "Noto Sans"
                          :height (ceiling (* face-height 1.5)) :weight 'thin)))))

(defun jme-org-agenda--set-title ()
  "Set a title on the agenda buffer in the header line."
  (let ((title (if (and org-agenda-redo-command
                        (stringp (cadr org-agenda-redo-command)))
                   (format "—  %s" (cadr org-agenda-redo-command))
                 "- Agenda")))
    (setq-local header-line-format
                (format "%s %s"
                        title
                        (make-string (- (window-width) (length title)) ?- t)))))

(defun jme-org-agenda--style-buffer ()
  "Apply style to agenda buffer."
  (setq-local line-spacing 3)
  )

(defun jme-org-agenda--hook-function ()
  "Function intended to be called in agenda hook."
  (jme-org-agenda--set-title))

(defun jme-org-agenda-all()
  "Show the full agenda with special view."
  (interactive)
  (org-agenda nil "z"))

(defun jme-org-agenda--enable()
  "Configure Org agenda."
  (custom-set-variables
   ;; Initial set of agenda files
   '(org-agenda-files jme-org-agenda-files)
   ;; How much to display in overview
   '(org-agenda-span 'day)
   ;; Restore window configuration after exiting agenda
   '(org-agenda-restore-windows-after-quit t)
   ;; Use log mode by default in agenda
   '(org-agenda-start-with-log-mode t)
   ;; Agenda custom commands
   '(org-agenda-custom-commands '(("z" "Full agenda super view"
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
   '(org-agenda-category-icon-alist
     `(("Admin" ,(list
                  (all-the-icons-material "assignment"))
        nil nil :ascent center)
       ("Archived" ,(list
                   (all-the-icons-material "archive"))
        nil nil :ascent center)
       ("Birthday" ,(list
                     (all-the-icons-material "cake"))
        nil nil :ascent center)
       ("Daily" ,(list
                  (all-the-icons-material "today"))
        nil nil :ascent center)
       ("Email" ,(list
                  (all-the-icons-material "mail"))
        nil nil :ascent center)
       ("Lunch" ,(list
                  (all-the-icons-material "local_dining"))
        nil nil :ascent center)
       ("Meeting" ,(list
                    (all-the-icons-material "today"))
        nil nil :ascent center)
       ("Mgmt" ,(list
                 (all-the-icons-material "person"))
        nil nil :ascent center)
       ("Review" ,(list
                   (all-the-icons-material "rate_review"))
        nil nil :ascent center)
       ("Time" ,(list
                 (all-the-icons-material "access_time"))
        nil nil :ascent center)
       ("Training" ,(list
                     (all-the-icons-material "directions_run"))
        nil nil :ascent center)
       ("NIC" ,(list
                (all-the-icons-material "cloud"))
        nil nil :ascent center)
       ("Weekly" ,(list
                   (all-the-icons-material "view_week"))
        nil nil :ascent center)
       ("tasks" ,(list
                  (all-the-icons-material "check_box"))
        nil nil :ascent center)
       ("task_archive" ,(list
                         (all-the-icons-material "archive"))
        nil nil :ascent center)
       ("Inbox" ,(list
                  (all-the-icons-material "inbox"))
        nil nil :ascent center)
       ("Customer" ,(list
                     (all-the-icons-material "account_circle"))
        nil nil :ascent center)
       ("Synacor" ,(list
                    (all-the-icons-material "business"))
        nil nil :ascent center)
       ("Research" ,(list
                     (all-the-icons-material "whatshot"))
        nil nil :ascent center)
       ("calendar" ,(list
                     (all-the-icons-material "today"))
        nil nil :ascent center))))
  (with-eval-after-load "org"
    (jme-common-enable-mode org-super-agenda-mode)
    (add-hook 'org-agenda-finalize-hook #'jme-org-agenda--hook-function)))

(defun jme-org-agenda--disable()
  "Disable agenda configuration."
  (remove-hook 'org-agenda-finalize-hook #'jme-org-agenda--hook-function)
  (jme-common-revert-symbols '(org-agend-files
                               org-agenda-span
                               org-agenda-restore-windows-after-quit
                               org-agenda-start-with-log-mode
                               org-agenda-custom-commands))
  ;; no default value for org-agenda-category-icon-alist.
  (set 'org-agenda-category-icon-alist nil)
  (jme-common-disable-mode org-super-agenda-mode))

(defun jme-org-unload-function()
  "Unload `org-agenda' configuration."
  (jme-org-agenda--disable))

(jme-common-defconfiguration jme-org-agenda "Org agenda configuration")

(provide 'jme-org-agenda)
;;; jme-org-agenda.el ends here
