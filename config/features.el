;;; features.el --- Feature configuration -*- lexical-binding: t -*-

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

(use-package all-the-icons)


(use-package all-the-icons-ivy
  :after (ivy)
  :config
  (all-the-icons-ivy-setup))


(use-package ivy
  :delight
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window)
         ("C-c v p" . ivy-push-view)
         ("C-c v s" . ivy-switch-view)
         ("C-c v d" . ivy-pop-view)
         :map ivy-minibuffer-map
         ("C-l" . ivy-immediate-done)
         :map ivy-switch-buffer-map
         ("C-l" . ivy-immediate-done))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-display-style 'fancy)
  (ivy-use-virtual-buffers t)
  :config
  (ivy-mode))

(use-package ivy-hydra
  :after ivy)

(use-package ivy-rich
  :after (ivy counsel)
  :custom
  (ivy-virtual-abbreviate 'full)
  (ivy-rich-switch-buffer-align-virtual-buffer t)
  (ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode))

(use-package all-the-icons-ivy-rich
  :after ivy-rich
  :config
  (all-the-icons-ivy-rich-mode 1))

(use-package swiper
  :after ivy
  :bind ("C-s" . swiper-isearch))

;; Sorting and filtering
(use-package prescient
  :after counsel
  :config
  (prescient-persist-mode 1))
;; ivy support for prescient
(use-package ivy-prescient
  :after prescient
  :config
  (ivy-prescient-mode 1))

;; Add limit of line length to optimize for large files (logs)
;; See https://oremacs.com/2018/03/05/grep-exclude/
(use-package counsel
  :delight
  :after swiper
  :custom
  (counsel-rg-base-command
      "rg -S -M 120 --no-heading --line-number --color never %s .")
  :config
  (counsel-mode 1))


(use-package switch-window
  :commands
  (switch-window switch-window-then-swap-buffer)
  :bind
  (("C-x o" . switch-window)
   ("C-x w" . switch-window-then-swap-buffer)))

;; Used in conjunction with `org-roam' allowing content search through notes.
(use-package deft
  :commands
  (deft)
  :bind
  (("C-c n z" . deft))
  :custom
  (deft-recursive nil) ;; Limit to just top-leve org-roam notes
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org"))

;; Org-roam customization based on suggestions from David Wilson (systemcrafters.net).
;; See: https://youtube.com/watch?v=CUkuyW6hr18

(defun jme:org-roam-filter-by-tag (tag-name)
  "Filter `org-roam' files based on TAG-NAME."
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun jme:org-roam-list-notes-by-tag (tag-name)
  "Produce a list of `org-roam' notes which have the specified TAG-NAME."
  (delete-dups
   (mapcar #'org-roam-node-file
           (seq-filter
            (jme:org-roam-filter-by-tag tag-name)
            (org-roam-node-list)))))

(defun jme:org-roam-refresh-agenda-list ()
  "Update `org-agenda-files' with items from `org-roam' notes."
  (interactive)
  (setq org-agenda-files (append
                          (jme:agenda-files)
                          (jme:org-roam-list-notes-by-tag "Project"))))

(defun jme:org-roam-project-finalize-hook ()
  "Add the captured project file to `org-agenda-files' if the capture was not aborted."
  (remove-hook 'org-capture-after-finalize-hook #'jme:org-roam-project-finalize-hook)

  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun jme:org-roam-find-project ()
  "Find an `org-roam' project file, creating if necessary."
  (interactive)
  (add-hook 'org-capture-after-finalize-hook #'jme:org-roam-project-finalize-hook)

  (org-roam-node-find
   nil
   nil
   (jme:org-roam-filter-by-tag "Project")
   :templates
   '(("p" "project" plain "* Overview\n\n%?\n\n* Goals\n\n* Tasks\n:PROPERTIES:\n:CATEGORY: tasks\n:END:\n\n* Meetings\n:PROPERTIES:\n:CATEGORY: Meeting\n:END:\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
      :unnarrowed t))))

(defun jme:org-roam-capture-inbox ()
  "Capture to inbox note."
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?"
                                   :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

(defun jme:org-roam-capture-project-task ()
  "Capture task to a project."
  (interactive)
  (add-hook 'org-capture-after-finalize-hook #'jme:org-roam-project-finalize-hook)
  (org-roam-captiure- :node (org-roam-node-read
                             nil
                             (jme:org-roam-filter-by-tag "Project"))
                      :templates '(("p" "project" plain "* Overview\n\n%?\n\n* Goals\n\n* Tasks\n:PROPERTIES:\n:CATEGORY: tasks\n:END:\n\n* Meetings\n:PROPERTIES:\n:CATEGORY: Meeting\n:END:\n\n* Dates\n\n"
                                    :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                                           "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                                                           ("Tasks"))))))

(defun jme:org-roam-copy-todo-to-today ()
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

(use-package org-roam
  :after org
  :init
  (setq org-roam-v2-ack t)
;;   :custom-face
;;   (org-roam-link ((t (:inherit org-link :foreground "#C991E1"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n b" . jme:org-roam-capture-inbox)
         ("C-c n t" . jme:org-roam-capture-project-task)
         ("C-c n p" . jme:org-roam-find-project)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :custom
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)
     ("p" "project" plain "* Overview\n\n%?\n\n* Goals\n\n* Tasks\n:PROPERTIES:\n:CATEGORY: tasks\n:END:\n\n* Meetings\n:PROPERTIES:\n:CATEGORY: Meeting\n:END:\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
      :unnarrowed t)))
  :config
  (add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))
  (require 'org-roam-protocol)
  (require 'org-roam-dailies)
  (org-roam-db-autosync-enable)
  (jme:org-roam-refresh-agenda-list)
  (add-to-list 'org-after-todo-state-change-hook
               (lambda ()
                 (when (equal org-state "DONE")
                   (jme:org-roam-copy-todo-to-today)))))

 (use-package org-roam-bibtex
   :after org-roam
   :commands (orb-note-actions)
   :hook (org-roam-mode . org-roam-bibtex-mode)
   :custom
   (org-note-actions-interface 'ivy)
   :config
   (require 'org-ref)
   :bind (:map org-mode-map
               (("C-c n a" . orb-note-actions))))

 (use-package org-noter
   :after org)

 (use-package org-pdftools
   :after org
   :hook (org-mode . org-pdftools-setup-link))

 (use-package pdf-tools
   :after org-noter
   :magic ("%PDF" . pdf-view-mode)
   :config
   (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
   (pdf-tools-install :no-query))

 (use-package org-noter-pdftools
   :after org-noter
   :defer t
   :config
   (with-eval-after-load 'pdf-annot
     (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package fontawesome)
;;; features.el ends here
