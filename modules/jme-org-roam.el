;;; jme-org-roam.el --- Org-Roam Configuration  -*- lexical-binding: t -*-

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
;; Org-roam customization based on suggestions from David Wilson (systemcrafters.net).
;; See: https://youtube.com/watch?v=CUkuyW6hr18

;;; Code:
(require 'straight)
(require 'jme-common)

(straight-use-package 'org-roam)

(require 'org-roam)
(require 'org-roam-dailies)
(require 'org-roam-capture)
;;(require 'org-roam-node)
(defvar org-roam-v2-ack)
(defvar org-agenda-files)
(defvar org-note-abort)
(defvar org-state)
(defvar org-after-todo-state-change-hook)
(declare-function jme-org-agenda-files "jme-org" ())
(declare-function org-refile "org-refile" (&optional ARG DEFAULT-BUFFER RFLOC MSG))
(declare-function org-capture-get "org-capture" (PROPERTY &optional LOCAL))
(declare-function org-roam-capture "org-capture" (&optional GOTO KEYS &key FILTER-FN TEMPLATES INFO))

(defun jme-org-roam--filter-by-tag (tag-name)
  "Filter `org-roam' files based on TAG-NAME."
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun jme-org-roam--list-notes-by-tag (tag-name)
  "Produce a list of `org-roam' notes which have the specified TAG-NAME."
  (delete-dups
   (mapcar #'org-roam-node-file
           (seq-filter
            (jme-org-roam--filter-by-tag tag-name)
            (org-roam-node-list)))))

(defun jme-org-roam-refresh-agenda-list ()
  "Update `org-agenda-files' with items from `org-roam' notes."
  (interactive)
  (setq org-agenda-files (append
                          (jme-org-agenda-files)
                          (jme-org-roam--list-notes-by-tag "Project"))))

(defun jme-org-roam--project-finalize-hook ()
  "Add the captured project file to `org-agenda-files'.
Does not add if the capture was aborted."
  (remove-hook 'org-capture-after-finalize-hook
               #'jme-org-roam--project-finalize-hook)

  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun jme-org-roam-find-project ()
  "Find an `org-roam' project file, creating if necessary."
  (interactive)
  (add-hook 'org-capture-after-finalize-hook
            #'jme-org-roam--project-finalize-hook)

  (org-roam-node-find
   nil
   nil
   (jme-org-roam--filter-by-tag "Project")
   :templates
   '(("p" "project" plain "* Overview\n\n%?\n\n* Goals\n\n* Tasks\n:PROPERTIES:\n:CATEGORY: tasks\n:END:\n\n* Meetings\n:PROPERTIES:\n:CATEGORY: Meeting\n:END:\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
      :unnarrowed t))))

(defun jme-org-roam-capture-inbox ()
  "Capture to inbox note."
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?"
                                   :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

(defun jme-org-roam-capture-project-task ()
  "Capture task to a project."
  (interactive)
  (add-hook 'org-capture-after-finalize-hook #'jme-org-roam--project-finalize-hook)
  (org-roam-capture- :node (org-roam-node-read
                            nil
                            (jme-org-roam--filter-by-tag "Project"))
                     :templates '(("p" "project" plain "* Overview\n\n%?\n\n* Goals\n\n* Tasks\n:PROPERTIES:\n:CATEGORY: tasks\n:END:\n\n* Meetings\n:PROPERTIES:\n:CATEGORY: Meeting\n:END:\n\n* Dates\n\n"
                                   :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                                          "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                                                          ("Tasks"))))))

(defun jme-org-roam-copy-todo-to-today ()
  "Refiles todo items to today's daily when completed."
  (interactive)
  (let ((org-refile-keep t) ;; we do not want to delete the original
        (org-roam-dailies-capture-templates
         '(("t" "tasks" entry "%?"
            :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
        (org-after-refile-insert-hook #'save-buffer)
        today-file
        pos)
    (save-window-excursion
      (org-roam-dailies--capture (current-time) t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))

    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "Tasks" today-file nil pos)))))

(defun jme-org-roam--after-todo-state-change-function ()
  "Copy DONE tasks to today note.

Intendend to be called from `org-after-todo-state-change-hook'."
  (when (equal org-state "DONE")
    (jme-org-roam-copy-todo-to-today)))

(defun jme-org-roam--bind-keys ()
  "Key definitions for `org-roam'."
  (global-set-key (kbd "C-c n b") #'jme-org-roam-capture-inbox)
  (global-set-key (kbd "C-c n c") #'org-roam-capture)
  (global-set-key (kbd "C-c n f") #'org-roam-node-find)
  (global-set-key (kbd "C-c n i") #'org-roam-node-insert)
  (global-set-key (kbd "C-c n l") #'org-roam-buffer-toggle)
  (global-set-key (kbd "C-c n p") #'jme-org-roam-find-project)
  (global-set-key (kbd "C-c n t") #'jme-org-roam-capture-project-task)
  (define-key org-roam-dailies-map (kbd "Y") #'org-roam-dailies-capture-yesterday)
  (define-key org-roam-dailies-map (kbd "T") #'org-roam-dailies-capture-tomorrow))

(defun jme-org-roam--unbind-keys ()
  "Revert key bindings."
  (global-unset-key (kbd "C-c n b"))
  (global-unset-key (kbd "C-c n c"))
  (global-unset-key (kbd "C-c n f"))
  (global-unset-key (kbd "C-c n i"))
  (global-unset-key (kbd "C-c n l"))
  (global-unset-key (kbd "C-c n p"))
  (global-unset-key (kbd "C-c n t"))
  (define-key org-roam-dailies-map (kbd "Y") nil)
  (define-key org-roam-dailies-map (kbd "T") nil))

(defun jme-org-roam--enable ()
  "Configure org-roam."
  (setq org-roam-v2-ack t)
  (with-eval-after-load "org"
        (custom-set-variables '(org-roam-capture-templates
                                '(("d" "default" plain
                                   "%?"
                                   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
                                   :unnarrowed t)
                                  ("p" "project" plain "* Overview\n\n%?\n\n* Goals\n\n* Tasks\n:PROPERTIES:\n:CATEGORY: tasks\n:END:\n\n* Meetings\n:PROPERTIES:\n:CATEGORY: Meeting\n:END:\n\n* Dates\n\n"
                                   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
                                   :unnarrowed t))))
        (add-to-list 'display-buffer-alist
                     '("\\*org-roam\\*"
                       (display-buffer-in-direction)
                       (direction . right)
                       (window-width . 0.33)
                       (window-height . fit-window-to-buffer)))
        (require 'org-roam-protocol)
        (require 'org-roam-dailies)
        (org-roam-db-autosync-enable)
        (jme-org-roam-refresh-agenda-list)
        (add-to-list 'org-after-todo-state-change-hook
                     'jme-org-roam--after-todo-state-change-function)

        (jme-org-roam--bind-keys)))

(defun jme-org-roam--disable ()
  "Disable `org-roam' configuration."
  (jme-org-roam--unbind-keys)
  (remove-hook 'org-after-todo-state-change-hook
               'jme-org-roam--after-todo-state-change-function)
  (jme-common-disable-mode org-roam-db-autosync-mode))

(defun jme-org-roam-unload-function ()
  "Remove org-roam configuration."
  (jme-org-roam--disable)
  (jme-common-safe-unload-features '(org-roam-dailies
                                     org-roam-protocol
                                     org-roam)))

(jme-common-defconfiguration jme-org-roam "Configuration for org-roam")

(provide 'jme-org-roam)
;;; jme-org-roam.el ends here.
