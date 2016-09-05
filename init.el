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

;; Use cask for package management
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Utilize pallet to keep cask in sync
(unless (package-installed-p 'pallet)
  (package-refresh-contents)
  (package-install 'pallet))
(pallet-mode t)

;; Utilize use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(custom-set-variables
 '(use-package-verbose t))

;; Ensure we have loaded an org version
(when (not (cl-remove-if-not
            (lambda (p) (equal 'org (car p)))
            package-alist))
  (message "No org-mode package found; installing now...")
  (package-refresh-contents)
  (package-install 'org))

;; load org and make sure we have the right version
(require 'org)
(when (string-match "^[1234567]" (org-version))
  (warn "Org-Mode is out of date. We expect org 8 or higher, but instead we have %s" (org-version)))

;; load the literate init file
(org-babel-load-file (expand-file-name "init-ext.org" "~/.emacs.d"))
(provide 'init)
;;; init.el ends here
