;; jme-fonts.el --- Font configuration -*- lexical-binding: t; -*-

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
;; Configure fonts
;; This work was originally inspired by and adapted from
;; Protesilaos Stavrou's prot-fonts.el
;; https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/prot-lisp/prot-fonts.el

;;; Code:

;;; Customization options
(require 'jme-common)

(defcustom jme-fonts-typeface-config-alist
  '((default . ( :fixed-pitch-family "FiraCode Nerd Font"
                 :fixed-pitch-height 170
                 :fixed-pitch-weight normal
                 :variable-pitch-family "Noto Sans"
                 :variable-pitch-height 170
                 :variable-pitch-weight regular))
    (alternate . ( :fixed-pitch-family "Hack"
                   :fixed-pitch-height 170
                   :fixed-pitch-weight normal
                   :variable-pitch-family "Noto Sans"
                   :variable-pitch-height 170
                   :variable-pitch-weight regular))
    (presentation . ( :fixed-pitch-family "FiraCode Nerd Font"
                      :fixed-pitch-height 170
                      :fixed-pitch-weight normal
                      :variable-pitch-family "Noto Sans"
                      :variable-pitch-height 170
                      :variable-pitch-weight regular)))
  "Alist of typefaces which can be applied.

The car of each cons cell is the intended configuration for
the typeface properties.

The cdr is a plist which specifies the properties to be applied for that
configuration."
  :group 'jme-customizations
  :type 'alist)

(defun jme-fonts--apply-face-attribute (face family &optional weight height)
  "Set FACE font attributes for FAMILY, with optional WEIGHT and HEIGHT.

Returns t if successful.

If the system cannot find the appropriate font, an error message is emitted
and nil is returned."
  (let ((h (or height 100))
        (w (or weight 'normal)))
    (if (find-font (font-spec :family family))
        (progn
          (set-face-attribute face nil :family family :weight w :height h)
          t)
      (progn
        (message "Cannot apply face attributes [%s] %s %s %s" face family weight height)
        nil))))

(defvar jme-fonts--font-config-hist '()
  "History of inputs for typeface configurations.")

(defun jme-fonts--set-fonts-prompt ()
  "Prompt for the font configuration (used by jme-fonts-set-font-config)."
  (let ((def (or (and jme-fonts--current-config
                      (symbol-name jme-fonts--current-config))
                 (car jme-fonts--font-config-hist)
                 "default")))
    (completing-read
     (format "Select font configuration [%s]: " def)
     (mapcar (lambda (entry) (symbol-name (car entry)))
             jme-fonts-typeface-config-alist)
     nil t nil 'jme-fonts--font-config-hist def)))

(defvar jme-fonts--current-config nil
  "Current font typeface attribute collection name.")

(defconst jme-fonts--managed-faces '(default fixed-pitch variable-pitch)
  "Faces managed by `jme-fonts-set-font-config'.")

(defvar jme-fonts--saved-face-attributes nil
  "Saved face attributes to restore when font configuration is disabled.")

(defun jme-fonts--snapshot-managed-faces ()
  "Capture baseline face attributes for `jme-fonts--managed-faces'."
  (unless jme-fonts--saved-face-attributes
    (setq jme-fonts--saved-face-attributes
          (mapcar (lambda (face)
                    (cons face
                          (list :family (face-attribute face :family nil t)
                                :weight (face-attribute face :weight nil t)
                                :height (face-attribute face :height nil t))))
                  jme-fonts--managed-faces))))

(defun jme-fonts--restore-managed-faces ()
  "Restore face attributes captured by `jme-fonts--snapshot-managed-faces'."
  (dolist (entry jme-fonts--saved-face-attributes)
    (let ((face (car entry))
          (attributes (cdr entry)))
      (apply #'set-face-attribute face nil attributes))))

(defun jme-fonts-set-font-config (config)
  "Set fonts based on CONFIG.

Stores the CONFIG in history if successful.

CONFIG is a symbol that represents the configuration specified
in the car of a cons cell in `jme-fonts-typeface-config-alist'."
  (interactive (list (jme-fonts--set-fonts-prompt)))
  (when window-system
    (let* ((fonts (if (stringp config) (intern config) config))
           (properties (alist-get fonts jme-fonts-typeface-config-alist)))
      (unless properties
        (user-error "Unknown font config: %s" config))
      (let* ((fixed-pitch-family (plist-get properties :fixed-pitch-family))
             (fixed-pitch-height (plist-get properties :fixed-pitch-height))
             (fixed-pitch-weight (plist-get properties :fixed-pitch-weight))
             (variable-pitch-family (plist-get properties :variable-pitch-family))
             (variable-pitch-height (plist-get properties :variable-pitch-height))
             (variable-pitch-weight (plist-get properties :variable-pitch-weight)))
        (jme-fonts--snapshot-managed-faces)
        (if (jme-fonts--apply-face-attribute
             'default fixed-pitch-family fixed-pitch-weight fixed-pitch-height)
            (if (jme-fonts--apply-face-attribute
                 'fixed-pitch fixed-pitch-family fixed-pitch-weight fixed-pitch-height)
                (if (jme-fonts--apply-face-attribute
                     'variable-pitch variable-pitch-family variable-pitch-weight variable-pitch-height)
                    (progn
                      (add-to-history 'jme-fonts--font-config-hist (symbol-name fonts))
                      (setq jme-fonts--current-config fonts)))))))))

(defun jme-fonts--enable ()
  "Enable font configuration.

Expects `default' to be a value in `jme-fonts-typeface-config-alist'."
  (jme-fonts-set-font-config 'default))

(defun jme-fonts--disable ()
  "Disable font configuration."
  (when jme-fonts--saved-face-attributes
    (jme-fonts--restore-managed-faces)
    (setq jme-fonts--saved-face-attributes nil))
  (setq jme-fonts--current-config nil))

(defun jme-fonts-unload-function ()
  "Revert preferred font configuration."
  (jme-fonts--disable))

(jme-common-defconfiguration jme-fonts "Font configuration")

(provide 'jme-fonts)
;;; jme-fonts.el ends here.
