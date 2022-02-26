;;; jme-themes.el --- Theme Configuration  -*- lexical-binding: t -*-

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

(straight-use-package 'doom-themes)

(defun jme-themes--window-setup-function ()
  "Load theme and toggle theme on."
  (progn
    (defvar doom-themes-enable-bold)
    (defvar doom-themes-enable-italic)
    (require 'doom-themes)
    (declare-function doom-themes-org-config "doom-themes-ext-org" ())
    (declare-function doom-themes-visual-bell-config "doom-themes-ext-visual-bell" ())
    (setq doom-themes-enable-bold t
          doom-themes-enable-italic t)
    ;; load but do not enable themes
    (load-theme 'doom-palenight t t)
    (load-theme 'doom-tomorrow-day t t)
    (load-theme 'override-dark t t)
    (load-theme 'override-light t t)
    (doom-themes-org-config)
    (doom-themes-visual-bell-config)
    (jme-themes-toggle-theme)))


(defun jme-themes--enable ()
  "Configure theme settings."
  ;; Doom theme configuration
  (add-hook 'window-setup-hook #'jme-themes--window-setup-function))

(defun jme-themes--disable ()
  "Disable the theme configuration."
  (jme-themes-disable-themes))

(defun jme-themes--customize-theme ()
  "Customize theme with personal preferences."
  ;; ensure specific faces are set as fixed-pitch face
  (mapc
   (lambda (face)
     (if (facep face)
         (set-face-attribute face nil :inherit 'fixed-pitch)))
   (list 'org-block
         'org-block-begin-line
         'org-block-end-line
         'org-checkbox
         'org-document-info-keyword
         'org-property-value
         'line-number
         'line-number-current-line
         'org-tag))
  ;; ensure specific faces iherit from fixed-pitch shadow face
  (mapc
   (lambda (face)
     (if (facep face)
         (set-face-attribute face nil :inherit '(shadow fixed-pitch))))
   (list 'org-code
         'org-table
         'org-verbatim))
  ;; ensure specific faces inherit from fixed-pitch font-lock-comment face
  (mapc
   (lambda (face)
     (if (facep face)
         (set-face-attribute face nil :inherit '(font-lock-comment-face fixed-pitch))))
   (list 'org-meta-line
         'org-special-keyword)))

(defun jme-themes-toggle-theme ()
    "Toggle color theme between light/dark theme.
Utilizes `state' property of the function to track state."
    (interactive)
    (if (get 'jme-themes-toggle-theme 'state)
        (progn
          (disable-theme 'override-light)
          (disable-theme 'doom-tomorrow-day)
          (enable-theme 'doom-palenight)
          (enable-theme 'override-dark)
          (jme-themes--customize-theme)
          (when (display-graphic-p)
            (set-mouse-color "#69bdd2"))
          (put 'jme-themes-toggle-theme 'state nil))
      (progn
        (disable-theme 'override-dark)
        (disable-theme 'doom-palenight)
        (enable-theme 'doom-tomorrow-day)
        (enable-theme 'override-light)
        (jme-themes--customize-theme)
        (when (display-graphic-p)
          (set-mouse-color "#042f66"))
        (put 'jme-themes-toggle-theme 'state t))))

(defun jme-themes-disable-themes ()
  "Disable theme configuration."
  (interactive)
  (disable-theme 'override-dark)
  (disable-theme 'override-light)
  (disable-theme 'doom-tomorrow-day)
  (disable-theme 'doom-palenight))

(defun jme-themes-unload-function ()
  "Unload `jme-themes' feature."
  (jme-themes--disable))

(jme-common-defconfiguration jme-themes "Themes configuration")

(provide 'jme-themes)
;;; jme-themes.el ends here.
