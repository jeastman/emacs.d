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
;; Ensure org is loaded as early as possible
(straight-use-package 'org)

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

(defmacro jme-require (feature)
  "Require FEATURE, but don't hard error if it's missing.
Logs a message so missing modules are visible at startup."
  `(unless (require ,feature nil 'noerror)
     (message "jme: missing module %s" ,feature)))

(jme-require 'jme-defaults)
(jme-require 'jme-window)
(jme-require 'jme-themes)
(jme-require 'jme-modeline)
(jme-require 'jme-history)
(jme-require 'jme-fonts)
(jme-require 'jme-icons)
(jme-require 'jme-buffers)
(jme-require 'jme-editor)
(jme-require 'jme-python)
(jme-require 'jme-completion)
(jme-require 'jme-org)
(jme-require 'jme-org-agenda)
(jme-require 'jme-dired)
(jme-require 'jme-vc)
(jme-require 'jme-shell)
(jme-require 'jme-org-roam)
(jme-require 'jme-llm)
(jme-require 'jme-markdown)

(defmacro jme-call (fn &rest args)
  "Call FN with ARGS if it's defined; log a message otherwise."
  `(if (fboundp ,fn)
       (funcall ,fn ,@args)
     (message "jme: missing function %s" ,fn)))

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

(jme-call 'jme-defaults 1)
(jme-call 'jme-window 1)
(jme-call 'jme-themes 1)
(jme-call 'jme-modeline 1)
(jme-call 'jme-history 1)
(jme-call 'jme-fonts 1)
(jme-call 'jme-icons 1)
(jme-call 'jme-buffers 1)
(jme-call 'jme-editor 1)
(jme-call 'jme-python 1)
(jme-call 'jme-completion 1)
(jme-call 'jme-org 1)
(jme-call 'jme-org-agenda 1)
(jme-call 'jme-dired 1)
(jme-call 'jme-vc 1)
(jme-call 'jme-shell 1)
(jme-call 'jme-org-roam 1)
(jme-call 'jme-llm 1)
(jme-call 'jme-markdown 1)

(when (fboundp 'jme-local-machine-config)
  (jme-local-machine-config))

;; load customization file
;; (when (file-exists-p custom-file)
;;   (load custom-file))
