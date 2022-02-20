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
                 :variable-pitch-weight light))
    (alternate . ( :fixed-pitch-family "Hack"
                   :fixed-pitch-height 170
                   :fixed-pitch-weight normal
                   :variable-pitch-family "Noto Sans"
                   :variable-pitch-height 170
                   :variable-pitch-weight light))
    (presentation . ( :fixed-pitch-family "FiraCode Nerd Font"
                      :fixed-pitch-height 170
                      :fixed-pitch-weight normal
                      :variable-pitch-family "Noto Sans"
                      :variable-pitch-height 170
                      :variable-pitch-weight light)))
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
  (let ((def (nth 1 jme-fonts--font-config-hist)))
    (completing-read
     (format "Select font configuration [%s]: " def)
     (mapcar #'car jme-fonts-typeface-config-alist)
     nil t nil 'jme-fonts--font-config-hist def)))

(defvar jme-fonts--current-config nil
  "Current font typeface attribute collection name.")

(defun jme-fonts-set-font-config (config)
  "Set fonts based on CONFIG.

Stores the CONFIG in history if successful.

CONFIG is a symbol that represents the configuration specified
in the car of a cons cell in `jme-fonts-typeface-config-alist'."
  (interactive (list (jme-fonts--set-fonts-prompt)))
  (when window-system
    (let* ((fonts (if (stringp config) (intern config) config))
           (properties (alist-get fonts jme-fonts-typeface-config-alist))
           (fixed-pitch-family (plist-get properties :fixed-pitch-family))
           (fixed-pitch-height (plist-get properties :fixed-pitch-height))
           (fixed-pitch-weight (plist-get properties :fixed-pitch-weight))
           (variable-pitch-family (plist-get properties :variable-pitch-family))
           (variable-pitch-height (plist-get properties :variable-pitch-height))
           (variable-pitch-weight (plist-get properties :variable-pitch-weight)))
      (if (jme-fonts--apply-face-attribute
           'default fixed-pitch-family fixed-pitch-weight fixed-pitch-height)
          (if (jme-fonts--apply-face-attribute
               'fixed-pitch fixed-pitch-family fixed-pitch-weight fixed-pitch-height)
              (if (jme-fonts--apply-face-attribute
                   'variable-pitch variable-pitch-family variable-pitch-weight variable-pitch-height)
                  (progn
                    (add-to-history 'jme-fonts--font-config-hist (format "%s" config))
                    (setq jme-fonts--current-config (format "%s" config)))))))))

(defun jme-fonts--enable ()
  "Enable font configuration.

Expects `default' to be a value in `jme-fonts-typeface-config-alist'."
  (jme-fonts-set-font-config 'default))

(defun jme-fonts--disable ()
  "Disable font configuration."
  ;; TODO disable fonts
  )

(defun jme-fonts-unload-funtion ()
  "Revert preferred font configuration."
  (jme-fonts--disable))

(jme-common-defconfiguration jme-fonts "Font configuration")

(provide 'jme-fonts)
;;; jme-fonts.el ends here.
