;;; jme-defaults.el --- Configuration defaults  -*- lexical-binding: t; -*-

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
;; Sets opinionated defaults

;;; Code:
(require 'jme-common)

(defun dont-kill-emacs ()
  "Prevent accidental Emacs exit."
  (interactive)
  (error "To exit Emacs, use M-x kill-emacs"))

(global-set-key (kbd "C-x C-c") 'dont-kill-emacs)


(defun jme-defaults--enable ()
  "Apply defaults configuration."
  ;; Allow all commands to work normally
  (setq disabled-command-function nil)

  ;; Use "y" and "n" instead of "yes" and "no"
  (setq use-short-answers t)

  ;; Surpress beeps
  (setq ring-bell-function 'ignore)

  ;; Security defaults
  ;; Use gnutls for checks
  (defvar gnutls-verify-error)
  (setq gnutls-verify-error t))

(defun jme-defaults--revert-to-system-defaults ()
  "Revert opinionated default settings."
  (jme-common-revert-symbols '(disabled-command-function
                               use-short-answers
                               ring-bell-function
                               gnutls-verify-error)))

(defun jme-defaults--disable ()
  "Revert default configuration."
  (jme-defaults--revert-to-system-defaults))

(defun jme-defaults-unload-function ()
  "Uload deafults feature."
  (jme-defaults--disable))

(jme-common-defconfiguration jme-defaults
                             "Default configuration.")

(provide 'jme-defaults)
;;; jme-defaults.el ends here
