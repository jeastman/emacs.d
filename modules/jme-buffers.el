;; jme-buffers.el --- Buffer configuration -*- lexical-binding: t; -*-

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
;; This file contains buffer-specific configuration

;;; Code:
;; Auto-Revert.
;; For additional information see Info node `(emacs)Auto Revert'.
(require 'jme-common)

(defun jme-buffers--enable-auto-revert ()
  "Enable auto-revert mode."
  ;; Both files and buffers should be reverted by
  ;; Global Auto-Revert Mode.
  (defvar global-auto-revert-non-file-buffers)
  (setq global-auto-revert-non-file-buffers t)
  ;; Silence auto-revert notifications
  (defvar auto-revert-verbose)
  (setq auto-revert-verbose nil)

  ;; Turn on global auto-revert mode
  (global-auto-revert-mode 1))

(defun jme-buffers--disable-auto-revert ()
  "Disable auto-revert, setting back to defaults."
  (jme-common-revert-symbols '(global-auto-revert-non-file-buffers
                        auto-revert-verbose))
  (global-auto-revert-mode -1))

(defun jme-buffers--config-uniquify ()
  "Configure buffer uniqueness."
  (setq
   ;; style name|bar/mumble
   uniquify-buffer-name-style 'post-forward
   ;; Separator for buffer name components
   uniquify-separator ":"
   ;; Regular expression for buffers to ignore
   uniquify-ignore-buffers-re "^\\*"))

(defun jme-buffers--revert-uniquify ()
  "Revert uniquify configuration."
  (jme-common-revert-symbols '(uniquify-buffer-name-style
                               uniquify-separator
                               uniquify-ignore-buffers-re)))

(defun jme-buffers--enable ()
  "Apply buffers configuration."
  ;; Replace suspend frame if in graphical mode
  (when (display-graphic-p)
    (global-set-key (kbd "C-z") #'bury-buffer))
  (jme-buffers--enable-auto-revert)
  (jme-buffers--config-uniquify))

(defun jme-buffers--disable ()
  "Un-apply buffers configuration."
  (jme-buffers--disable-auto-revert)
  (jme-buffers--revert-uniquify))

(defun jme-buffers-unload-function ()
  "Unload buffers feature."
  (jme-buffers--disable))

(jme-common-defconfiguration jme-buffers "Buffer configuration")

(provide 'jme-buffers)
;;; jme-buffers.el ends here
