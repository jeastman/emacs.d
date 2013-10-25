;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs configuration file
;; Converted to literate format
;; Author: John Eastman
;; Date: 2013-10-24
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use cask for package management
(require 'cask "~/.cask/cask.el")
(cask-initialize)

(when (not (cl-remove-if-not
            (lambda (p) (equal 'org (car p)))
            package-alist))
  (message "No org-mode package found; installing now...")
  (package-install 'org))

;; Invoke and configure package management.
;(require 'package)
;(setq elpa-dir (concat dotfiles-dir "elpa"))
;(setq elpa-archives-dir (concat elpa-dir "/archives"))
;(add-to-list 'package-archives
;             '("marmalade" .
;               "http://marmalade-repo.org/packages/") t)
;(add-to-list 'package-archives
;             '("melpa" .
;               "http://melpa.milkbox.net/packages/") t)
;
;(package-initialize)

(require 'org)
(when (string-match "^[1234567]" (org-version))
  (warn "Org-Mode is out of date. We expect org 8 or higher, but instead we have %s" (org-version)))

(require 'pallet)

(org-babel-load-file (expand-file-name "init-ext.org" "~/.emacs.d"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tomorrow-night)))
 '(custom-safe-themes (quote ("d7caa3eedf8032f9193be8917d685ba6cc8181c924b941a31e07449b0aafbc9a" "af308b38a4e64c038e6b7069b431f0a7b9d08fbdfe032631f1b436e8b4700aac" default)))
 '(safe-local-variable-values (quote ((erlang-indent-level . 4) (after-save-hook archive-done-tasks)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
