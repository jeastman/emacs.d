;;; init.el --- Emacs configuration file
;;
;; Author: John Eastman
;; Date: 2013-10-24
;; Converted to literate format
;; Date: 2018-04-10
;; Streamlined for minimal config
;;
;;; Commentary:
;;
;; Just enough configuration to initialize the packaging system and
;; ensure org is loaded.
;;
;;; Code:

(let ((gc-cons-threshold most-positive-fixnum))

  (when (version< emacs-version "25")
    (warn "This configuration expects Emacs version 25 or greater -- You are running %s!"
      emacs-version))

  ;; require the package manager
  (require 'package)

  ;; In an effort to reduce the risk of loading outdated byte code files,
  ;; set load-prefer-newer to t. Starting with Emacs version 24.5,
  ;; load-prefer-newer can prevent outdated byte code files from being
  ;; loaded. However, there is still a possibility that a source file would
  ;; not be re-compiled. auto-compile works to fill this gap. (loaded later)
  (setq-default
   load-prefer-newer t            ; load prefers the newest version of a file
   package-enable-at-startup nil) ; Prevent packages from being activated after reading init

  ;; initialize the package repositories
  (setq package-archives '(("org"   . "https://orgmode.org/elpa/")
                           ("melpa-stable" . "http://stable.melpa.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")
                           ("gnu"   . "https://elpa.gnu.org/packages/")))

  (package-initialize)

  ;; Install use-package
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (eval-when-compile
    (require 'use-package))
  (unless (package-installed-p 'diminish)
    (package-refresh-contents)
    (package-install 'diminish)
    (require 'diminish))
  (require 'bind-key)
  (custom-set-variables
   '(use-package-verbose t))

  (use-package org
    :pin org)

  (org-babel-load-file (expand-file-name "init-ext.org" user-emacs-directory))

  (garbage-collect))

(provide 'init)
;;; init.el ends here
