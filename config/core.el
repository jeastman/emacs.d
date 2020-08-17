;;; core.el --- Core configuration items  -*- lexical-binding: t -*-

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

(setq disabled-command-function nil) ; enable all commands

(custom-set-variables
 '(ring-bell-function 'ignore)       ; turn off noise
 '(inhibit-startup-message t)
 '(inhibit-startup-echo-area-message (user-login-name))
 '(global-auto-revert-non-file-buffers t)
 '(auto-revert-verbose nil)
 '(uniquify-buffer-name-style 'post-forward)
 '(uniquify-separator ":")
 '(uniquify-after-kill-buffer-p t)
 '(uniquify-ignore-buffers-re "^\\*"))

(global-auto-revert-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 Support (everywhere)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (custom-set-variables '(x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))))

;; Stay secure
(setq gnutls-verify-error t
      tls-checktrust t)

;;; core.el ends here
