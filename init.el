;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs configuration file
;; Converted to literate format
;; Author: John Eastman
;; Date: 2013-04-13
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook
 `(lambda ()
    ;; remember this directory
    (require 'org)
    (org-babel-load-file (expand-file-name "init-ext.org" ".emacs.d"))))
