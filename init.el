;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs configuration file
;; Converted to literate format
;; Author: John Eastman
;; Date: 2013-04-13
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(add-hook 'after-init-hook
; `(lambda ()
;    ;; remember this directory
;    (require 'org)
;    (org-babel-load-file (expand-file-name "init-ext.org" ".emacs.d"))))

(require 'org)
(org-babel-load-file (expand-file-name "init-ext.org" ".emacs.d"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tomorrow-night)))
 '(custom-safe-themes (quote ("af308b38a4e64c038e6b7069b431f0a7b9d08fbdfe032631f1b436e8b4700aac" default)))
 '(safe-local-variable-values (quote ((erlang-indent-level . 4) (after-save-hook archive-done-tasks)))))
