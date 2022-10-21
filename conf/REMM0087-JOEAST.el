;;; REMM0087-JOEAST.el --- Configuration for laptop -*- lexical-binding: t; -*-

;; Author: John Eastman
;; Created: 25 Aug 2022

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

;; Machine-specific configuration.

;;; Code:

(custom-set-variables
 '(org-roam-directory "~/Documents/notes")
 '(org-directory "~/Documents/org")
 '(org-refile-targets '((org-agenda-files :maxlevel . 5)
                        (("~/Documents/org/task_archive.txt") :maxlevel . 5)
                        (nil :maxlevel . 5)))
 '(bibtex-dialect 'biblatex)
 '(bibtex-completion-bibliography '("~/Documents/Bibliography/refs.bib"))
 '(reftex-default-bibliography "~/Documents/Bibliography/refs.bib")
 '(org-ref-bibliography-notes "~/Documents/org/notes.org")
 '(org-ref-default-bibliography '("~/Documents/Bibliography/refs.bib"))
 '(org-ref-pdf-directory "~/Documents/Bibliography/bibtex-pdfs/"))

(defun jme-local-machine-config ()
  "Machine specific packages and configuration."
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; General Email settings
  (setq message-kill-buffer-on-exit t)
  ;;(setq mail-user-agent 'notmuch-user-agent)
  (setq mail-user-agent 'mu4e-user-agent)

  )

(setq org-roam-v2-ack t)

(provide 'REMM0087-JOEAST)
;;; REMM0087-JOEAST.el ends here
