;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs configuration file
;; Converted to literate format
;; Author: John Eastman
;; Date: 2013-04-13
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This section has been moved from init-ext as I am loading org from
;; from the package system and need it to be used from the elpa directory.
;;
;; This is currently broken for first time use, since 
;; Define dotfile-dir and metafiles-dir values
(setq dotfiles-dir (file-name-directory
    (or (buffer-file-name) load-file-name)))
(setq metafiles-dir "~/.emacs-meta")

;; As this may be the first time this is run, we may need to create
;; the metafiles directory if it does not exist.
(unless (file-exists-p metafiles-dir)
  (make-directory metafiles-dir))

;; Invoke and configure package management.
(require 'package)
(setq elpa-dir (concat dotfiles-dir "elpa"))
(setq elpa-archives-dir (concat elpa-dir "/archives"))
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" .
               "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(require 'org)
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
