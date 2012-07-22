;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Path specific configuration for pecoux
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Prevent finder from open new frame
(setq ns-pop-up-frames 'nil)

; Use aspell instead of ispell
(setq-default ispell-program-name "/usr/local/bin/aspell")
; Use multi-markdown instead of markdown
(setq markdown-command "/usr/local/bin/mmd")

; Erlang paths
(add-to-list 'load-path "/usr/local/Cellar/erlang/R15B01/lib/erlang/lib/tools-2.6.7/emacs")
(setq erlang-root-dir "/usr/local/Cellar/erlang/R15B01")
(setq erlang-man-dirs
  '(("Man - Commands" "share/man/man1" t)
    ("Man - Modules" "share/man/man3" t)
    ("Man - Files" "share/man/man4" t)
    ("Man - Applications" "share/man/man6" t)))

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

; Set preferred sources for helm
(if (package-installed-p 'helm)
    (progn (require 'helm-misc)
     (setq helm-for-files-preferred-list
           (quote
            (helm-c-source-ffap-line
             helm-c-source-ffap-guesser
             helm-c-source-buffers-list
             helm-c-source-recentf
             helm-c-source-bookmarks
             helm-c-source-file-cache
             helm-c-source-files-in-current-dir
             helm-c-source-mac-spotlight)))))


