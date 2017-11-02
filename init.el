;;; init.el --- Emacs configuration file
;;
;; Converted to literate format
;; Author: John Eastman
;; Date: 2013-10-24
;;
;;; Commentary:
;;
;; Bootstrap initialization file.  Just enough configuration
;; to initialize the packaging system and ensure org is loaded
;; so that the rest of the configuration can be loaded.
;;
;;; Code:
;;

;; Bling on Software tip about Garbage Collection
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;; Original source appears to be:
;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(let ((gc-cons-threshold most-positive-fixnum))

  ;; Warn about using older versions of Emacs
  ;; Nothing specific here, but this configuration is developed and maintained on
  ;; version specified here.
  (when (version< emacs-version "25")
    (warn "This configuration expects Emacs version 25 or greater - you are running %s!"
          emacs-version))

  ;; require the package manager
  (require 'package)

  ;; Set some startup configuration values
  (setq-default
   load-prefer-newer t            ; load prefers the newest version of a file
   package-enable-at-startup nil) ; Prevent packages from being activated after reading init

  ;; initialize the ELPA repositories where packages are fetched
  (setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                           ("melpa" . "https://melpa.org/packages/")
                           ("gnu"   . "https://elpa.gnu.org/packages/")))

  (package-initialize)

  ;; Utilize use-package
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (require 'use-package)
  (custom-set-variables
   '(use-package-verbose t))

  ;; load org and make sure we have the right version
  (use-package org
    :ensure t
    :pin org
    :ensure org-plus-contrib)

  ;; load the literate init file
  (org-babel-load-file (expand-file-name "init-ext.org" user-emacs-directory))

  ;; trigger garbage collection to clean up our deferred mess
  (garbage-collect))
(provide 'init)
;;; init.el ends here
