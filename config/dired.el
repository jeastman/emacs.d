;;; dired.el --- Dired configuration -*- lexical-binding: t -*-

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

(defun jme:dired-mode-setup ()
  "Setup for dired mode."
  (dired-hide-details-mode t))

(add-hook 'dired-mode-hook 'jme:dired-mode-setup)

(use-package all-the-icons-dired
  :after (all-the-icons)
  :commands (all-the-icons-dired-mode)
  :hook (dired-mode . all-the-icons-dired-mode))

;;; dired.el ends here
