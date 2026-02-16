;; jme-icons.el --- Icon package configuration -*- lexical-binding: t; -*-

;; Author: John Eastman
;; Created: 15 Feb 2026

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
;; Configure icon packages shared by multiple modules.

;;; Code:

(require 'straight)
(require 'jme-common)

(defun jme-icons--enable ()
  "Install and load icon packages."
  (straight-use-package 'nerd-icons)
  (require 'nerd-icons)
  (when (display-graphic-p)
    (straight-use-package 'all-the-icons)
    (require 'all-the-icons)))

(defun jme-icons--disable ()
  "Unload icon packages."
  (jme-common-safe-unload-features '(all-the-icons
                                     nerd-icons)))

(defun jme-icons-unload-function ()
  "Unload icon configuration."
  (jme-icons--disable))

(jme-common-defconfiguration jme-icons "Icon package configuration")

(provide 'jme-icons)
;;; jme-icons.el ends here.
