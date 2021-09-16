;;; ui.el --- UI Configuration -*- lexical-binding: t -*-

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
;; General UI configuration.
;;
;; Note the special handling of the menu bar on MacOS

;;; Code:

;; Basic items
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))              ; disable the toolbar
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))            ; disable the scrollbar
(if (fboundp 'blink-cursor-mode)
    (blink-cursor-mode 0))           ; don't blink the cursor
;(if (fboundp 'global-hl-line-mode)
;    (global-hl-line-mode t))         ; highlight current line
(if (fboundp 'show-paren-mode)
    (show-paren-mode t))             ; highlight matching parens
(if (fboundp 'fringe-mode)
    (fringe-mode 16 ))               ; Make the fringe slightly wider

;; The menu is kind of a waste of space for me
;; Disable it when not on MacOS (darwin)
(if (eq window-system 'nil)
    (if (fboundp 'menu-bar-mode)
        (menu-bar-mode -1))
  (if (fboundp 'menu-bar-mode)
      (if (string-equal system-type "darwin")
          (menu-bar-mode 1)
        (menu-bar-mode -1))))

(custom-set-variables '(indicate-empty-lines t))

;; Scroll settings (optimized for trackpad)
(custom-set-variables '(scroll-step 1)
                      '(mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
                      '(mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling
                      '(mouse-wheel-follow-mouse 't))                 ; scroll window under mouse

;; line numbers
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override modes which may be derived from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Center buffer
(defun jme:visual-fill ()
  "Visually center a buffer."
  (interactive)
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :config (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
  :hook ((org-mode . jme:visual-fill)
         (eww-mode . jme:visual-fill)
         (elfeed-show-mode . jme:visual-fill)))

(use-package rainbow-delimiters
  :defer t
  :commands (rainbow-delimiters-mode)
  :hook (prog-mode . rainbow-delimiters-mode))

;;; ui.el ends here
