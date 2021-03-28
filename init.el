;;; init.el --- Emacs configuration file -*- lexical-binding: t -*-

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
;; I moved my Emacs configuration to a literate format in 2013 and
;; have been using that since.  While literate code, expecially for a
;; configuration file, seems like a great idea and would lead to a
;; wonderful experience, it has not.  I find that as I hack on my
;; configuration, the added literate verbiage seems contrived.  Most of
;; the time, the code speaks for itself and the addition of literate
;; text is just redundant.  While this may not be true for some of the
;; larger components (org for example), most of the time it is
;; sufficient.
;;
;; My new plan is to revert back to plain elisp code and add in
;; documentation comments as necessary.  That is the point of code
;; comments anyway.
;;
;; I've decided to do this by re-creating my configuration from the
;; ground up.  It's always good to do on occasion.  There are plenty of
;; new packages that I was either not taking advantage of or not using
;; properly.  This iteration of refresh is intended to make use of
;; those things, as well as weed out the cruft.
;;
;; Since I am starting all over, I have decided to give straight
;; (https://github.com/raxod502/straight.el) a try.

;;; Code:

;; Some configuration moved to early-init, which is
;; only supported with emacs 27+.
(when (version< emacs-version "27")
  (load (concat user-emacs-directory "early-init.el")))

(defvar jme:config-dir
  (concat (expand-file-name user-emacs-directory) "config")
  "Custom configuration directory.")

;; Monitor startup time.
(defun jme:display-startup-time ()
  "Disply the configuration startup time."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'jme:display-startup-time)

;; straight bootstrap code (bootstrap-version 5)
(setq straight-use-package-by-default t)
(defvar boostrap-version)
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

;; utilize use-package
(straight-use-package 'use-package)

;; Ensure org
(straight-use-package '(org-plus-contrib :includes org))

;; Handle setting proper environment on Mac
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (progn
    (when (string-match-p "/zsh$" (getenv "SHELL"))
      ;; Use a non-interactive login shell to ensure
      ;; zsh path is loaded properly from .zprofile
      ;; which only loads with a login shell.
      (setq exec-path-from-shell-arguments '("-l")))
    (exec-path-from-shell-initialize)))

(use-package auto-compile
  :demand t
  :config
  (auto-compile-on-load 1)
  (auto-compile-on-save-mode 1))

(defun jme:load (directory base)
  "Look for files located in DIRECTORY whose base name is BASE.

Check the base name against several extensions.  If a file with
that name exists, then load it."
  (let ((literate      (expand-file-name (concat base ".org") directory))
        (encrypted-org (expand-file-name (concat base ".org.gpg") directory))
        (plain         (expand-file-name (concat base ".el") directory))
        (encrypted-el  (expand-file-name (concat base ".el.gpg") directory)))
    (cond
     ((file-exists-p encrypted-org) (org-babel-load-file encrypted-org))
     ((file-exists-p encrypted-el)  (load encrypted-el))
     ((file-exists-p literate)      (org-babel-load-file literate))
     ((file-exists-p plain)         (load plain)))))

;; Load theme configuration early to avoid ui flashing
(jme:load jme:config-dir "uitheme")

;; load paths first to ensure emacs-user-directory stays clean
(jme:load jme:config-dir "paths")

;; load os-specific settings - should be limitied to path setup
(let* ((system-type-name (symbol-name system-type))
       (base-name (replace-regexp-in-string "/" "-" system-type-name)))
  (jme:load jme:config-dir base-name))

;; load system specific file - should be limited to path setup
(let ((host-name-base (car (split-string (system-name) "\\."))))
  (jme:load jme:config-dir host-name-base))

;; load configuration files
(mapc (lambda (f) (jme:load jme:config-dir f))
      '("backup"
        "core"
        "features"
        "modeline"
        "ui"
        "dired"
        "completion"
        "editor"
        "help"
        "history"
        "lint"
        "orgmode"
        "prog-modes"
        "lsp"
        "projects"
        "shell"
        "snippets"
        "version-control"))

;; (jme:load jme:config-dir "backup")
;; (jme:load jme:config-dir "core")
;; (jme:load jme:config-dir "features")
;; (jme:load jme:config-dir "modeline")
;; (jme:load jme:config-dir "ui")
;; (jme:load jme:config-dir "dired")
;; (jme:load jme:config-dir "completion")
;; (jme:load jme:config-dir "editor")
;; (jme:load jme:config-dir "help")
;; (jme:load jme:config-dir "history")
;; (jme:load jme:config-dir "lint")
;; (jme:load jme:config-dir "orgmode")
;; (jme:load jme:config-dir "prog-modes")
;; (jme:load jme:config-dir "lsp")
;; (jme:load jme:config-dir "projects")
;; (jme:load jme:config-dir "shell")
;; (jme:load jme:config-dir "snippets")
;; (jme:load jme:config-dir "version-control")

;; load host specific packages
(let ((host-name-base (car (split-string (system-name) "\\."))))
  (jme:load jme:config-dir (concat host-name-base "-pkg")))

;; load customization file
(setq custom-file
      (concat (file-name-directory user-init-file) "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; User specific settings
(jme:load jme:config-dir user-login-name)

;; private settings
(jme:load user-emacs-directory ".private")

;;; init.el ends here
