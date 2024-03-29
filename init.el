;;; init.el --- Emacs configuration file -*- lexical-binding: t -*-

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
;; This configuration was developed using Emacs 28. Some measures have been
;; taken to make it compatible with previous versions, but I have not tested
;; this.  Expect issues to be present if running Emacs version < 28;

;;; Code:

;; Some configuration moved to early-init, which is
;; only supported with emacs 27+.
(when (version< emacs-version "27")
  (load (concat user-emacs-directory "early-init.el")))

;; Monitor startup time.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Set the default value for various coding systems to UTF-8
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; Add local modules directory to the load path
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

;; straight bootstrap code (bootstrap-version 5)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'straight)

;; Garbage Collector Magic Hack
;; Set to start in emacs-startup-hook once gc is reset.
;; See early-init for additional details.
;; With Emacs 28.1, this seems to have a negative effect
;; TODO: investigate GC issues.
;;(straight-use-package 'gcmh)
;;(require 'gcmh nil 'noerror)

;; Handle setting proper environment on Mac
;; (use-package exec-path-from-shell
;;   :if (memq window-system '(mac ns x))
;;   :config
;;   (progn
;;     (when (string-match-p "/zsh$" (getenv "SHELL"))
;;       ;; Use a non-interactive login shell to ensure
;;       ;; zsh path is loaded properly from .zprofile
;;       ;; which only loads with a login shell.
;;       (setq exec-path-from-shell-arguments '("-l")))
;;     (exec-path-from-shell-initialize)))

;; Control the creation of files in Emacs directory.
(straight-use-package 'no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
(require 'no-littering nil 'noerror)

;; Load machine dependent configuration
(load
 (expand-file-name
  (concat
   user-emacs-directory
   "conf/"
   (car (split-string (system-name)  "\\."))))
 'noerror)

(require 'jme-defaults)
(require 'jme-window)
(require 'jme-themes)
(require 'jme-modeline)
(require 'jme-history)
(require 'jme-fonts)
(require 'jme-buffers)
(require 'jme-editor)
(require 'jme-python)
(require 'jme-completion)
(require 'jme-org)
(require 'jme-org-agenda)
(require 'jme-dired)
(require 'jme-vc)
(require 'jme-shell)
(require 'jme-org-roam)

;; Specify a custom file
(setq custom-file
      (concat (file-name-directory user-init-file) "custom.el"))

;; Load OS dependent configuration
(load
 (expand-file-name
  (concat
   user-emacs-directory
   "modules/jme-"
   (symbol-name system-type)
   ".el"))
 'noerror)

;; Call the module
(when (fboundp
       (intern (concat "jme-" (symbol-name system-type))))
  (funcall (intern (concat "jme-" (symbol-name system-type))) 1))

(jme-defaults 1)
(jme-window 1)
(jme-themes 1)
(jme-modeline 1)
(jme-history 1)
(jme-fonts 1)
(jme-buffers 1)
(jme-editor 1)
(jme-python 1)
(jme-completion 1)
(jme-org 1)
(jme-org-agenda 1)
(jme-dired 1)
(jme-vc 1)
(jme-shell 1)
(jme-org-roam 1)

(when (fboundp 'jme-local-machine-config)
  (jme-local-machine-config))

;; load customization file
;; (when (file-exists-p custom-file)
;;   (load custom-file))
