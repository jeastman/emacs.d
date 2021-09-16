;;; uitheme.el --- Theme Configuration -*- lexical-binding: t -*-

;; Author: John Eastman
;; Created: 27 Mar 2021

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
;; Isolate theme loading such that it can be loaded
;; early in the init process.
;;
;; This is to ensure that there is minor impact on the
;; visual loading of Emacs.
;;

;;; Code:

;; Fonts

;; Font configuration
(defvar jme:default-font-scale 170 "Default font scale.")
(defconst jme:default-font-family "FiraCode Nerd Font" "Default font to use.")
(defconst jme:backup-font-family "Hack" "Secondary font to use.")
(defconst jme:variable-font-family "Noto Sans" "Font for variable pitch use.")

(defun jme:set-font-scale (size)
  "Adjust the font scale used to SIZE."
  (interactive "nWhat font scale do you want? ")
  (set-face-attribute 'mode-line nil :inherit 'default :height size)
  (cond
   ((find-font (font-spec :family jme:default-font-family))
    (progn
      (set-face-attribute 'default nil :family jme:default-font-family :weight 'light :height size)
      (set-face-attribute 'fixed-pitch nil :family jme:default-font-family :inherit 'default)))
   ((find-font (font-spec :family jme:backup-font-family))
    (progn
      (set-face-attribute 'default nil :family jme:backup-font-family :weight 'light :height size)
      (set-face-attribute 'fixed-pitch nil :family jme:backup-font-family :inherit 'default))))
   (cond
    ((find-font (font-spec :family jme:variable-font-family))
    (progn
      (set-face-attribute 'variable-pitch nil :family jme:variable-font-family :inherit 'default)))))

(defun jme:reset-font-scale ()
  "Reset the font scale to the desired value."
  (interactive)
  (if window-system
      (jme:set-font-scale jme:default-font-scale)))

(add-hook 'window-setup-hook 'jme:reset-font-scale)
;; Theme

;; Theme customization
;; Inspired by http://www.howardism.org/Technical/Emacs/orgmode-wordprocessor.html
;; and thus
;; https://github.com/jonnay/org-beautify-theme
(defun jme:customize-theme ()
  "Customize theme to my liking."
  (interactive)
  (mapc
   (lambda (face)
     (set-face-attribute face nil :inherit 'fixed-pitch))
   (list 'org-block
         'org-block-begin-line
         'org-block-end-line
         'org-checkbox
         'org-document-info-keyword
         'org-property-value
         'line-number
         'line-number-current-line
         'org-tag))
  (mapc
   (lambda (face)
     (set-face-attribute face nil :inherit '(shadow fixed-pitch)))
   (list 'org-code
         'org-table
         'org-verbatim))
  (mapc
   (lambda (face)
     (set-face-attribute face nil :inherit '(font-lock-comment-face fixed-pitch)))
   (list 'org-meta-line
         'org-special-keyword)))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)

  (defun jme:toggle-ui-theme ()
    "Toggle color theme between light/dark theme.
Utilizes `state' property of the function to track state."
    (interactive)
    (if (get 'jme:toggle-ui-theme 'state)
        (progn
          (disable-theme 'override-light)
          (disable-theme 'doom-tomorrow-day)
          (enable-theme 'doom-palenight)
          (enable-theme 'override-dark)
          (jme:customize-theme)
          (when (display-graphic-p)
            (set-mouse-color "#69bdd2"))
          (put 'jme:toggle-ui-theme 'state nil))
      (progn
        (disable-theme 'override-dark)
        (disable-theme 'doom-palenight)
        (enable-theme 'doom-tomorrow-day)
        (enable-theme 'override-light)
        (jme:customize-theme)
        (when (display-graphic-p)
          (set-mouse-color "#042f66"))
        (put 'jme:toggle-ui-theme 'state t))))

  (add-hook 'window-setup-hook (lambda ()
                                 (progn
                                   ;; load but do not enable themes
                                   (load-theme 'doom-palenight t t)
                                   (load-theme 'doom-tomorrow-day t t)
                                   (load-theme 'override-dark t t)
                                   (load-theme 'override-light t t)
                                   (doom-themes-org-config)
                                   (jme:toggle-ui-theme)))))

;;; uitheme.el ends here
