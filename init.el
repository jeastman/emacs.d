;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs configuration file
;; Updated for Emacs 24
;; Author: John Eastman
;; Date: 08-02-2011
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fix up path - make sure usr/local is at top
(setenv "PATH" (concat "/usr/local/bin:/usr/local/sbin:/usr/texbin:" (getenv "PATH")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set up package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives 
             '("marmalade" .
               "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" .
               "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar my-packages
  '(ace-jump-mode autopair auto-complete coffee-mode deft dired+ expand-region flymake-coffee helm js2-mode magit magithub markdown-mode markdown-mode+ multi-term nose paredit pretty-mode rainbow-delimiters rainbow-mode slime slime-repl slime-js smex w3m)
  "List of packages to ensure are installed at startup.")

(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package))))
 my-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library Paths
;; Everything is situated underneath dotfiles-dir (~/.emacs.d)
;; This should make everything self-contained and easy to migrate
;; to other environments
;;
;; This section contains all of the path configurations.
;; Meta-information files (backups, etc.) are stored in ~/.emacs-meta
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Establish the configuration directory
(setq dotfiles-dir (file-name-directory
    (or (buffer-file-name) load-file-name)))
(setq metafiles-dir "~/.emacs-meta")

;; Update load path
(add-to-list 'load-path dotfiles-dir)

; Add all top-level subdirectories of .emacs.d to the load path
(let ((default-directory dotfiles-dir))
      (normal-top-level-add-subdirs-to-load-path))

; Paths for meta-information
(setq meta-places (concat metafiles-dir "/places"))
;(setq meta-backup (concat metafiles-dir "/backups/")) ;; still needs work
(setq meta-bookmarks (concat metafiles-dir "/bookmarks"))
(setq meta-saveplace (concat metafiles-dir "/saveplace"))
(setq meta-savehist (concat metafiles-dir "/savehist"))
(setq meta-recent (concat metafiles-dir "/recentf"))
(setq meta-saves (concat metafiles-dir "/auto-save-list/.saves-"))
(setq meta-ido (concat metafiles-dir "/ido.last"))
(setq meta-tramp (concat metafiles-dir "/tramp"))

; Keep 3rd-party libraries in vendor directory
(add-to-list 'load-path (concat dotfiles-dir "vendor"))
(let ((default-directory (concat dotfiles-dir "vendor")))
     (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'exec-path "/usr/local/bin")

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for system and user specific stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq system-specific-config (concat dotfiles-dir system-name ".el")
    user-specific-config (concat dotfiles-dir user-login-name ".el")
    user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-config) (load user-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes (quote ("bf8c80820d13f0cd019c797c0b053222f87245a5" default)))
 '(safe-local-variable-values (quote ((erlang-indent-level . 4) (after-save-hook archive-done-tasks)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'bind-key)
(set-terminal-coding-system 'utf-8) ; set terminal output to utf-8
(set-keyboard-coding-system 'utf-8) ; set terminal input to utf-8
(prefer-coding-system 'utf-8)       ; set preferred coding to utf-8

(setq visible-bell t                ; set visible bell instead of beeps
      echo-keystrokes 0.1           ; make sure we can see what we type
      shift-select-mode nil         ; Don't use shift+arrows for mark
      mouse-yank-at-point t         ; Use point instead of mouse click
      require-final-newline t       ; Add newline to end of file on save
      truncate-partial-width-windows nil ; respect 'truncate-lines'
      uniquify-buffer-name-style 'forward ; sane paths in buffer names
      whitespace-style '(faces trailing lines-tail space-before-tab
			       indentation space-after-tab) ; whitespace handline
      whitespace-line-column 80     ; lines longer than this are highlighted
      ediff-window-setup-function 'ediff-setup-windows-plain 
      save-place-file meta-places)  ; Where to store the places file

(global-whitespace-mode 1)          ; Always enable whitespace minor mode

(setq-default tab-width 4)          ; set the default tab width

; Auto revert mode
(global-auto-revert-mode 1)         ; Reverts buffers when they change on disk

; uniquify: unique buffer names
(require 'uniquify) ;; make buffer names more unique
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":"
  uniquify-after-kill-buffer-p t
  uniquify-ignore-buffers-re "^\\*")

; Place all backup copies of files in a common location
(defconst use-backup-dir t)
(setq backup-directory-alist (quote ((".*" . "~/.emacs-meta/backups/")))
      version-control t                ; Use version numbers for backups
      kept-new-versions 2              ; Number of newest versions to keep
      kept-old-versions 5              ; Number of oldest versions to keep
      delete-old-versions t            ; Ask to delete excess backup versions?
      backup-by-copying-when-linked t) ; Copy linked files, don't rename.

; Never put tabs in files, use spaces instead
; Note: Use C-q C-i to put a real tab should the need ever arise.
(setq-default indent-tabs-mode nil)

; make emacs use the clipboard
(setq x-select-enable-clipboard t)

(random t) ; Seed the random-number generator

; Allow narrowing
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

; bookmarks
(setq bookmark-default-file meta-bookmarks
    bookmark-save-flag 1)

; saveplace: save location in file when saving files
(setq save-place-file meta-saveplace)
(setq-default save-place t)            ; activate it for all buffers
(require 'saveplace)                   ; get the package

; savehist: save some history
(setq savehist-additional-variables    ; also save...
  '(search ring regexp-search-ring)    ; ... my search entries
  savehist-autosave-interval 60        ; save every minute (default: 5 min)
  savehist-file meta-savehist)         ; keep my home clean
(savehist-mode t)                      ; do customization before activation

(require 'recentf)                     ; save recently used files
(setq
  recentf-save-file meta-recent        ; where to store the file
  recentf-max-saved-items 100          ; max save 100
  recentf-max-menu-items 15)           ; max 15 in menu
(recentf-mode t)                       ; turn it on

(setq auto-save-list-file-prefix       ; where to save auto-save-list
      meta-saves)

(setq-default flymake-no-changes-timeout '3) ; timeout for flymake

(when (require 'autopair nil 'noerror)
  (autopair-global-mode))              ; enable autopair in all buffers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; deft
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require 'deft nil 'noerror)
  (when (file-exists-p my-notes)
    (setq
     deft-extension "org"
     deft-directory my-notes
     deft-text-mode 'org-mode)
    (global-set-key (kbd "<f9>") 'deft)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual Tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Remove menu when not using window-system
(if (eq window-system 'nil)
    (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (if (fboundp 'menu-bar-mode) (menu-bar-mode 1)))

; Get rid of the toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

; Get rid of the scroll bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

; Set the font to use
(if (eq window-system 'ns)                   ; only setting on MacOS for now
    (set-default-font "Consolas-14"))

; Set up smooth scrolling
(setq scroll-step 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
(setq mouse-wheel-progressive-speed nil) ; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ; scroll window under mouse

; Set fill
(setq-default fill-column 75)      ; column that triggers fill

; Show paren matching
(show-paren-mode 1)

(setq inhibit-startup-message t          ; don't show ...
  inhibit-startup-echo-area-message t)   ; ... startup messages

; Set line/column number on modeline
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode t)

; Show newlines at end of file
;(define-fringe-bitmap 'empty-line [0 0 #x3c #x3c #x3c #x3c 0 0])
(set-default 'indicate-empty-lines t) ; show fringe bitmap in left edge

; cause Emacs to fully redraw the display before it processes queued
; input events - slight performance tweak for newer machines
(setq redisplay-dont-pause t)

(require 'pretty-mode nil 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window sizing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when-compile
  (defvar emacs-min-top)
  (defvar emacs-min-left)
  (defvar emacs-min-height)
  (defvar emacs-min-width))

(if window-system
    (unless noninteractive
      (defvar emacs-min-top 22)
      (defvar emacs-min-left 5)
      (defvar emacs-min-height (if (= 1050 (x-display-pixel-height)) 55 64))
      (defvar emacs-min-width 100)))

(defun emacs-min ()
  (interactive)
  (set-frame-parameter (selected-frame) 'fullscreen nil)
  (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
  (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil)
  (set-frame-parameter (selected-frame) 'top emacs-min-top)
  (set-frame-parameter (selected-frame) 'left emacs-min-left)
  (set-frame-parameter (selected-frame) 'height emacs-min-height)
  (set-frame-parameter (selected-frame) 'width emacs-min-width))

(if window-system
    (add-hook 'after-init-hook 'emacs-min))

(defun emacs-max ()
  (interactive)
  (if t
      (progn
        (set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
        (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
        (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil))
    (set-frame-parameter (selected-frame) 'top 26)
    (set-frame-parameter (selected-frame) 'left 2)
    (set-frame-parameter (selected-frame) 'width
                         (floor (/ (float (x-display-pixel-width)) 9.15)))
    (if (= 1050 (x-display-pixel-height))
        (set-frame-parameter (selected-frame) 'height
                             (if (>= emacs-major-version 24)
                                 66
                               55))
      (set-frame-parameter (selected-frame) 'height
                           (if (>= emacs-major-version 24)
                               75
                             64)))))

(defun emacs-toggle-size ()
  (interactive)
  (if (> (cdr (assq 'width (frame-parameters))) 100)
      (emacs-min)
    (emacs-max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hide-or-expand ()
  (interactive)
  (if (> (length (window-list)) 1)
      (delete-other-windows)
    (bury-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mark and pop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar push-mark-before-goto-char nil)

(defadvice goto-char (before push-mark-first activate)
  (when push-mark-before-goto-char
    (push mark)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific key-bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Don't need C-z to minimize, put it to better use
;(if window-system (progn
;    (global-set-key "\C-z" 'multi-term-next)
;    (global-set-key "\C-x\C-z" 'multi-term-next)))
(bind-key "C-z" 'hide-or-expand)

(bind-key "M-`" 'other-frame)
(bind-key "C-`" 'pop-to-mark-command)

(when (package-installed-p 'expand-region)
    (bind-key "C-=" 'er/expand-region))

;; C-c map

(bind-key "C-c m" 'emacs-toggle-size)

;; C-. map

(when (package-installed-p 'ace-jump-mode)
    (bind-key "C-. C-s" 'ace-jump-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spell check
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice flyspell-mode (after advice-flyspell-check-buffer-on-start activate)
  (flyspell-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'comint)
(setq comint-prompt-read-only t)
(define-key comint-mode-map [(meta p)]
   'comint-previous-matching-input-from-input)
(define-key comint-mode-map [(meta n)]
   'comint-next-matching-input-from-input)
(define-key comint-mode-map [(control meta n)]
    'comint-next-input)
(define-key comint-mode-map [(control meta p)]
    'comint-previous-input)

;;(setq comint-completion-autolist t	;list possibilities on partial completion
;;       comint-completion-recexact nil	;use shortest compl. if characters cannot be added
       ;; how many history items are stored in comint-buffers (e.g. py- shell)
       ;; use the HISTSIZE environment variable that shells use (if avail.)
       ;; (default is 32)
;;       comint-input-ring-size (string-to-number (or (getenv  
;;"HISTSIZE") "100")))

(add-hook 'term-mode-hook
          #'(lambda () 
              (setq autopair-dont-activate t) ;; for emacsen < 24
              (autopair-mode -1))             ;; for emacsen >= 24
          )

(add-hook 'term-exec-hook
          (function
           (lambda ()
             (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))

(autoload 'multi-term "multi-term" nil t)
(autoload 'multi-term-next "multi-term" nil t)
;;(setq multi-term-program "/bin/bash")   ;; use bash
;; only needed if you use autopair
(add-hook 'term-mode-hook
      #'(lambda () (setq autopair-dont-activate t)))
(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term) ;; create a new one

(require 'tramp)
(setq shell-prompt-pattern "^[^a-zA-Z].*[#$%>] *")
(setq tramp-default-method "ssh")
(setq tramp-persistency-file-name meta-tramp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; prefer dired over dumping directory to buffer
(require 'dired+)
(put 'dired-find-alternate-file 'disabled nil)  ;enable `a' command
(global-set-key "\C-x\C-d" 'dired)

(when (require 'dired-single nil 'noerror)

  ;Make sure each dired buffer doesn't spawn new dired buffers
  (defun my-dired-init ()
    "Bunch of stuff to run for dired, either immediately or when it's
  loaded."
    ;; <add other stuff here>
    (define-key dired-mode-map [return] 'dired-single-buffer)
    (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
    (define-key dired-mode-map "^"
      (function
       (lambda nil (interactive) (dired-single-buffer "..")))))
  ;; if dired's already loaded, then the keymap will be bound
  (if (boundp 'dired-mode-map)
      ;; we're good to go; just add our bindings
      (my-dired-init)
    ;; it's not loaded yet, so add our bindings to the load-hook
    (add-hook 'dired-load-hook 'my-dired-init)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text mode hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'magit nil 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require 'markdown-mode nil 'noerror)
  (add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "jme-org")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flymake-get-tex-args (file-name)
    (list "/usr/texbin/chktex" (list "-q" "-I" "-H" "-v0" file-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp mode hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Make return automatically indent
(add-hook 'lisp-mode-hook (lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'emacs-lisp-mode-hook (lambda () (local-set-key (kbd "RET") 'newline-and-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Erlang
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'erlang-start nil 'noerror)
(require 'erlang-flymake nil 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JavaScript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require 'js-comint nil 'noerror)
  (setq inferior-js-program-command "node"))
(when (require 'flymake-jslint nil 'noerror)
(add-hook 'js-mode-hook (lambda () (flymake-mode t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coffee Script
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the node path
(when (require 'coffee-mode nil 'noerror)

  (defun coffee-custom ()
    "coffee-mode-hook"

    ;; CoffeeScript uses two spaces.
    (set (make-local-variable 'tab-width) 2)

    ;; If you don't have js2-mode
    (setq coffee-js-mode 'javascript-mode)

    ;; If you don't want your compiled files to be wrapped
    (setq coffee-args-compile '("-c" "--bare"))

    ;; *Messages* spam
    (setq coffee-debug-mode t)

    ;; Emacs key binding
    (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)

    ;; Compile '.coffee' files on every save
    (and (file-exists-p (buffer-file-name))
         (file-exists-p (coffee-compiled-file-name))
         (coffee-cos-mode t)))

  (add-hook 'coffee-mode-hook 'coffee-custom)
  (add-hook 'coffee-mode-hook '(lambda () (flymake-coffee-load))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pyhton
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(setq python-remove-cwd-from-path nil)

; Bring back indent after newline
(add-hook 'python-mode-hook '(lambda ()
           (define-key python-mode-map "\C-m" 'newline-and-indent)))

(when (load "flymake" t)
         (defun flymake-pyflakes-init ()
           (let* ((temp-file (flymake-init-create-temp-buffer-copy
                              'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
             (list jme-python-flymake-script  (list temp-file))))

         (add-to-list 'flymake-allowed-file-name-masks
                  '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

;; To avoid having to mouse hover for the error message, these functions
;; make flymake error messages appear in the minibuffer
(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the message in the minibuffer"
  (require 'cl)
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
          (let ((err (car (second elem))))
            (message "%s" (flymake-ler-text err)))))))

(add-hook 'post-command-hook 'show-fly-err-at-point)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun ido-goto-symbol ()
  "Will update the imenu index and then use ido to select a
   symbol to navigate to"
  (interactive)
  (imenu-make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

;; ido-mode is like magic pixie dust!
(when (> emacs-major-version 21)
    (ido-mode t)
    (setq
        ido-save-directory-list-file meta-ido
;        ido-ignore-buffers ;; ignore these guys
;          '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
;             "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
        ido-case-fold  t                 ; be case-insensitive
        ido-enable-last-directory-history t ; remember last used dirs
        ido-max-work-directory-list 30   ; should be enough
        ido-max-work-file-list      50   ; remember many
        ido-use-filename-at-point nil    ; don't use filename at point (annoying)
        ido-use-url-at-point nil         ; don't use url at point (annoying)
        ido-enable-flex-matching nil     ; don't try to be too smart
        ido-max-prospects 10
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-confirm-unique-completion t  ; wait for RET, even with unique completion
))

;; when using ido, the confirmation is rather annoying...
(setq confirm-nonexistent-file-or-buffer nil)

;; increase minibuffer size when ido completion is active
(add-hook 'ido-minibuffer-setup-hook
  (function
    (lambda ()
      (make-local-variable 'resize-minibuffer-window-max-height)
      (setq resize-minibuffer-window-max-height 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(add-to-list 'load-path (concat ...))
(when (require 'auto-complete-config nil 'noerror)
  (ac-config-default)
  (setq ac-comphist-file (concat metafiles-dir "/ac-comphist.dat"))
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (package-installed-p 'helm)
  (require 'helm-misc)
  (bind-key "C-c M-x" 'helm-M-x)
  (bind-key "C-h a" 'helm-c-apropos)
  (bind-key "M-s a" 'helm-do-grep)
  (bind-key "M-s b" 'helm-occur)
  (bind-key "M-s F" 'helm-for-files))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; swank-js support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (package-installed-p 'slime-js)
  (add-hook 'js2-mode-hook
            (lambda ()
              (slime-js-minor-mode 1)))
  (add-hook 'css-mode-hook
            (lambda ()
              (define-key css-mode-map "\M-\C-x" 'slime-js-refresh-css)
              (define-key css-mode-map "\C-c\C-r" 'slime-js-embed-css))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; w3m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq w3m-use-cookies t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for color code color  display in files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require 'rainbow-mode nil 'noerror)
  (add-hook 'css-mode-hook 'rainbow-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Newsticker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq newsticker-cache-filename (concat metafiles-dir "/.newsticker-cache"))
(setq newsticker-dir (concat metafiles-dir "/newsticker/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pianobar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'pianobar "pianobar" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start the Server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Don't start the server unless we can verify that it isn't running.
(require 'server)
(when (and (functionp 'server-running-p) (not (server-running-p)))
(server-start))

;; Support for Chrome 'edit with emacs' extension
(when (require 'edit-server nil 'noerror)
  (edit-server-start))
