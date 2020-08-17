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

;; Font configuration
(defvar jme:default-font-scale 120 "Default font scale.")
(defvar jme:default-font-family "Hack" "Default font to use.")
(defvar jme:variable-font-family "Noto Sans" "Font for variable pitch use.")

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

;; Seetings for inital frame
(add-to-list 'initial-frame-alist '(height . 48))
(add-to-list 'initial-frame-alist '(width . 160))
;; Settings for new frames
(add-to-list 'default-frame-alist '(height . 48))
(add-to-list 'default-frame-alist '(width . 80))


(custom-set-variables '(indicate-empty-lines t))

;; Scroll settings (optimized for trackpad)
(custom-set-variables '(scroll-step 1)
                      '(mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
                      '(mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling
                      '(mouse-wheel-follow-mouse 't))                 ; scroll window under mouse

(use-package rainbow-delimiters
  :defer t
  :commands (rainbow-delimiters-mode)
  :hook (prog-mode . rainbow-delimiters-mode))

;; Theme

;; Theme customization
;; Inspired by http://www.howardism.org/Technical/Emacs/orgmode-wordprocessor.html
;; and thus
;; https://github.com/jonnay/org-beautify-theme
(defun jme:customize-theme ()
  "Customize theme to my liking."
  (interactive)
  (let* ((variable-tuple
          (cond ((find-font (font-spec :family jme:variable-font-family)) `(:font ,jme:variable-font-family))
                ((find-font (font-spec :family "Source Sans Pro")) '(:font "Source Sans Pro"))
                ((find-font (font-spec :family "Lucida Grande")) '(:font "Lucida Grande"))
                ((find-font (font-spec :family "Verdana")) '(:font "Verdana" ))
                ((find-font (font-spec :family "Sans Serif")) '(:font "Sans Serif"))
                (nil (warn "Cannot find an appropriate variable font.")))))
    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (:inherit 'outline-8 ,@variable-tuple))))
     `(org-level-7 ((t (:inherit 'outline-7 ,@variable-tuple))))
     `(org-level-6 ((t (:inherit 'outline-6 ,@variable-tuple))))
     `(org-level-5 ((t (:inherit 'outline-5 ,@variable-tuple))))
     `(org-level-4 ((t (:inherit 'outline-4 ,@variable-tuple :height 1.0))))
     `(org-level-3 ((t (:inherit 'outline-3 ,@variable-tuple :height 1.1))))
     `(org-level-2 ((t (:inherit 'outline-2 ,@variable-tuple :height 1.25))))
     `(org-level-1 ((t (:inherit 'outline-1 ,@variable-tuple :height 1.3))))
     `(org-document-title ((t (:inherit org-level-1 :weight normal :height 1.75 :underline nil))))
     `(org-done ((t (:inherit 'org-headline-done :bold 'inherit :strike-through t))))))
  (mapc
   (lambda (face)
     (set-face-attribute face nil :inherit 'fixed-pitch))
   (list 'org-code
         'org-block
         'org-table
         'org-tag
         'org-verbatim
         'org-block-begin-line
         'org-block-end-line
         'org-meta-line
         'org-property-value
         'org-special-keyword
         'org-document-info-keyword)))

;; (use-package material-theme)
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-org-config)
  (jme:customize-theme))

(when (display-graphic-p)
  (set-mouse-color "#51afef"))

(defun jme:set-font-scale (size)
  "Adjust the font scale used to SIZE."
  (interactive "nWhat font scale do you want? ")
  (set-face-attribute 'mode-line nil :inherit 'default :height size)
  (cond
   ((find-font (font-spec :family jme:default-font-family))
    (progn
      (set-face-attribute 'default nil :family jme:default-font-family :height size)
      (set-face-attribute 'fixed-pitch nil :family jme:default-font-family :inherit 'default)))
   ((find-font (font-spec :family jme:variable-font-family))
    (progn
      (set-face-attribute 'default nil :family jme:variable-font-family :weight 'light :height size)
      (set-face-attribute 'variable-pitch nil :family jme:variable-font-family :inherit 'default)))))

(defun jme:reset-font-scale ()
  "Reset the font scale to the desired value."
  (interactive)
  (if window-system
      (jme:set-font-scale jme:default-font-scale)))

(add-hook 'window-setup-hook 'jme:reset-font-scale)

;;; ui.el ends here
