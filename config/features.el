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

;; NOTE: Currently using a forked branch of all-the-icons
;; to account for an issue with font-lock.
;; SEE: https://github.com/domtronn/all-the-icons.el/issues/99
;; SEE: https://github.com/domtronn/all-the-icons.el/issues/121#issuecomment-423759625
;; TODO: Revert to main repository once issue is fixed.
(use-package all-the-icons
   :straight (all-the-icons :files (:defaults "data")
                            :host github
                            :repo "domtronn/all-the-icons.el"
                            :fork (:host github
                                         :remote "ubolonton"
                                         :repo "ubolonton/all-the-icons.el"
                                         :branch "font-lock-fix")))
;; (use-package all-the-icons)

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

(use-package swiper
  :after ivy
  :bind ("C-s" . swiper))

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

;;; features.el ends here
