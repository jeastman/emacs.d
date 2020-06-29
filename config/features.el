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
         ("C-c v d" . ivy-pop-view))
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

(use-package deft
  :commands
  (deft)
  :bind
  (("C-c n d" . deft))
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org"))

(use-package org-roam
  :after org
  :commands (org-roam--build-cache)
  :straight (:host github :repo "jethrokuan/org-roam")
  :hook
  (after-init . org-roam-mode)
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-show-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert)))
  :custom-face
  (org-roam-link ((t (:inherit org-link :foreground "#C991E1")))))

(use-package company-org-roam
  :straight (:host github :repo "jethrokuan/company-org-roam")
  :config
  (push 'company-org-roam company-backends))

(use-package org-roam-bibtex
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (:map org-mode-map
              (("C-c n a" . orb-note-actions))))

(use-package org-noter
  :after org)

(use-package org-pdftools
  :after org
  :hook (org-load . org-pdftools-setup-link))

(use-package pdf-tools
  :after org-noter
  :config
  (pdf-loader-install))

(use-package org-noter-pdftools
  :after pdf-tools
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package fontawesome)
;;; features.el ends here
