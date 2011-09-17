;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Path specific configuration for jeastman-mp.mezeo.local
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Use aspell instead of ispell
(setq-default ispell-program-name "/usr/bin/aspell")
; Use multi-markdown instead of markdown
(setq markdown-command "/usr/bin/mmd")

; Erlang paths
(add-to-list 'load-path "/usr/local/lib/erlang/lib/tools-2.6.6.1/emacs")
(setq erlang-root-dir "/usr/local/lib/erlang")

; lintnode location
(setq lintnode-location "/home/jeastman/Projects/l/lintnode")
; Node path
(setenv "NODE_PATH" "/usr/local/lib/node_modules")

; CoffeeScript path
(setq coffee-command "/usr/local/bin/coffee")

; Python flymake script path
(setq jme-python-flymake-script "/home/jeastman/.emacs.d/vendor/pycheckers.sh")
; (setq jme-python-flymake-script "/usr/local/bin/pep8")
; (setq jme-python-flymake-script "/usr/local/bin/pyflakes")

; Org mode
(setq org-directory "~/Documents/Dropbox/org")
(setq org-mobile-directory "~/Documents/Dropbox/MobileOrg")
(setq my-notes "~/Documents/Dropbox/notes")

; Set to use chromium as browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")
