;;; jme-modeline.el --- Modeline Configuration  -*- lexical-binding: t -*-

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
(straight-use-package 'doom-modeline)
(straight-use-package 'minions)

(defun jme-modeline--after-init-function ()
  "Hook handler for after init."
  (jme-common-enable-mode doom-modeline-mode)
  (jme-common-enable-mode minions-mode))

(defun jme-modeline--enable ()
  "Configure modeline."
  (jme-common-enable-mode column-number-mode)               ; show column numbers
  (jme-common-enable-mode line-number-mode)                 ; show line numbers
  (jme-common-enable-mode size-indication-mode)             ; show buffer size

  (custom-set-variables
    '(doom-modeline-height 15)
    '(doom-modeline-bar-width 6)
    '(doom-modeline-lsp t)
    '(doom-modeline-github t)
    '(doom-modeline-major-mode-color-icon t)
    '(doom-modeline-minor-modes t))

  (require 'doom-modeline)
  (require 'minions)
  (add-hook 'after-init-hook #'jme-modeline--after-init-function))

(defun jme-modeline--disable ()
  "Revert modeline configuration."
  (jme-common-disable-mode minions-mode)
  (jme-common-disable-mode doom-modeline-mode))

(defun jme-modeline-unload-function ()
  "Unload the modeline feature."
  (jme-modeline--disable)
  (jme-common-safe-unload-features '(minions
                                     doom-modeline)))

(jme-common-defconfiguration jme-modeline "Modeline configuration")

(provide 'jme-modeline)
;;; jme-modeline.el ends here.
