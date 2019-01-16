;;; editor.el --- Editor configuration -*- lexical-binding: t -*-

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

;; Editor settings
(custom-set-variables
 '(fill-column 75)
 '(tab-width 4)
 '(indent-tabs-mode nil)
 '(next-line-add-newlines t)
 '(select-enable-clipboard t)
 '(require-final-newline t))

(use-package smartparens
  :delight
  :hook (after-init . smartparens-global-mode)
  :config
  (require 'smartparens-config))

(use-package expand-region
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package whitespace
  :delight
  :custom
  (whitespace-style '(face trailing space-before-tab
                           indentation space-after-tab))
  (whitespace-line-column 80)
  :config
  (global-whitespace-mode 1))

;; Helpful package for visualizing color identifiers.
(use-package rainbow-mode)

;; Whitespace cleanup can be dangerous if it changes the content of the
;; file. Some changes are guaranteed to be safe, which this function sticks
;; to, allowing it to be safe for a before-save-hook.
;; SEE: http://whattheemacsd.com/buffer-defuns.el-01.html
(defun jme:cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a `before-save-hook', and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

;; less safe version
(defun jme:cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (jme:cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

(add-hook 'before-save-hook 'jme:cleanup-buffer-safe)

;;; editor.el ends here
