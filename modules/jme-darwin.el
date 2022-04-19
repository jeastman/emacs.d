;;; jme-darwin.el --- Darwin (MacOS) specific Configuration  -*- lexical-binding: t -*-

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
;; (load (concat "jme-" (symbol-name system-type)))
;;; Code:
(require 'jme-common)

(defun jme-darwin--enable ()
  "Enable Darwin configuration."
  ;; Turn on composition mode (support for font ligatures)
  (jme-common-enable-mode mac-auto-operator-composition-mode))

(defun jme-darwin--disable ()
  "Disable Darwin configuration."
  (jme-common-disable-mode mac-auto-operator-composition-mode))

(defun jme-darwin-unload-function ()
  "Unload `jme-darwin' feature."
  (when (fboundp 'jme-darwin-disable)
      (jme-darwin-disable)))

(jme-common-defconfiguration jme-darwin "Darwin (MacOS) configuration")

(provide 'jme-darwin)
;;; jme-darwin.el ends here
