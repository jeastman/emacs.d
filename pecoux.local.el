;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Path specific configuration for pecoux
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Choose a nice mac font
;(set-face-attribute 'default nil :font "Consolas-14")

; Prevent finder from open new frame
(setq ns-pop-up-frames 'nil)

; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

; Use aspell instead of ispell
(setq-default ispell-program-name "/usr/local/bin/aspell")
; Use multi-markdown instead of markdown
(setq markdown-command "/usr/local/bin/mmd")

; Erlang paths
(add-to-list 'load-path "/usr/local/Cellar/erlang/R15B02/lib/erlang/lib/tools-2.6.8/emacs")
(setq erlang-root-dir "/usr/local/Cellar/erlang/R15B01")
(setq erlang-man-dirs
  '(("Man - Commands" "share/man/man1" t)
    ("Man - Modules" "share/man/man3" t)
    ("Man - Files" "share/man/man4" t)
    ("Man - Applications" "share/man/man6" t)))

; lintnode location
;;(setq lintnode-location "/Users/jeastman/Projects/l/lintnode")
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

; Fix middle-click when using flyspell on mac
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [moude-3] #'undefined)))

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


