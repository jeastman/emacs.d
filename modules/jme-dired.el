;;; jme-dired.el --- Dired Configuration  -*- lexical-binding: t -*-

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
(straight-use-package 'all-the-icons-dired)

(defun jme-dired-mode-hook-function ()
  "Setup for DIRED mode."
  (jme-common-enable-mode dired-hide-details-mode))

(defun jme-dired--enable ()
  "Configuration for DIRED mode."
  (add-hook 'dired-mode-hook #'jme-dired-mode-hook-function)
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

(defun jme-dired--disable ()
  "Revert DIRED configuration."
  ;; TODO for each dired buffer,
  ;; (dired-hide-details-mode -1)
  (remove-hook 'dired-mode-hook #'jme-dired-mode-hook-function)
  (remove-hook 'dired-mode-hook #'all-the-icons-dired-mode))

(defun jme-dired-unload-function ()
  "Unload Dired customizations."
  (jme-dired--disable))

(jme-common-defconfiguration jme-dired "Dired configuration")

(provide 'jme-dired)
;;; jme-dired.el ends here
