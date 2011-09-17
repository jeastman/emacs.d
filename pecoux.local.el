;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Path specific configuration for pecoux
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Use aspell instead of ispell
(setq-default ispell-program-name "/usr/local/bin/aspell")
; Use multi-markdown instead of markdown
(setq markdown-command "/usr/local/bin/mmd")

; Erlang paths
(add-to-list 'load-path "/usr/local/Cellar/erlang/R14B03/lib/erlang/lib/tools-2.6.6.4/emacs")
(setq erlang-root-dir "/usr/local/Cellar/erlang/R14B03")

; lintnode location
(setq lintnode-location "/Users/jeastman/Projects/l/lintnode")
; Node path
(setenv "NODE_PATH" "/usr/local/lib/node_modules")

; CoffeeScript path
(setq coffee-command "/usr/local/bin/coffee")

; Python flymake script path
(setq jme-python-flymake-script "/Users/jeastman/.emacs.d/vendor/pycheckers.sh")
; (setq jme-python-flymake-script "/usr/local/bin/pep8")
; (setq jme-python-flymake-script "/usr/local/bin/pyflakes")

; Org mode
(setq org-directory "~/Dropbox/org")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq my-notes "~/Dropbox/notes")
