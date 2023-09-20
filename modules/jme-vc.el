;; jme-vc.el --- Version control configuration -*- lexical-binding: t; -*-

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
(require 'straight)
(require 'jme-common)

(straight-use-package 'magit)
(straight-use-package 'magit-gitflow)
(straight-use-package 'diff-hl)

;;; Code:
(defun jme-vc--enable ()
  "Configure version control."
  ;; Magit
  (declare-function magit-status "magit-status" (&optional DIRECTORY CACHE))
  (global-set-key (kbd "C-x d") #'magit-status)
  ;; magit-gitflow
  (declare-function turn-on-magit-gitflow "magit-gitflow" ())
  (add-hook 'magit-mode-hook #'turn-on-magit-gitflow)
  ;; diff-hl
  (declare-function diff-hl-dired-mode "diff-hl-dired" (&optional ARG))
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  (declare-function diff-hl-magit-post-refresh "diff-hl" ())
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)

  (jme-common-enable-mode global-diff-hl-mode))

(defun jme-vc--disable ()
  "Revert configuration change."
  (remove-hook 'magit-mode-hook #'turn-on-magit-gitflow)
  (remove-hook 'dired-mode-hook #'diff-hl-dired-mode)
  (remove-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (global-set-key (kbd "C-x d") #'dired)
  (jme-common-disable-mode global-diff-hl-mode))

(defun jme-vc-unload-function ()
  "Uload version control configuration."
  (jme-vc--disable)
  (jme-common-safe-unload-features
   '(diff-hl magit-gitflow ghub magit)))

(jme-common-defconfiguration jme-vc "Version Control Configuration")

(provide 'jme-vc)
;;; jme-vc.el ends here.
