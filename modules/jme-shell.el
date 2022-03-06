;; jme-shell.el --- Shell configuration -*- lexical-binding: t; -*-

;; Author: John Eastman
;; Created: 12 Feb 2022

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this file; see the file COPYING.  If not, see see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'straight)
(require 'jme-common)
(straight-use-package 'eshell-syntax-highlighting)
(declare-function eshell-life-is-too-much "esh-mode" ())
(declare-function eshell-syntax-highlighting-global-mode
                  "eshell-syntax-highlighting" (&optional ARG))
(declare-function eshell-save-some-history "em-hist" ())
(declare-function eshell-truncate-buffer "esh-mode" ())
(declare-function eshell/alias "em-alias" (&optional ALIAS &rest DEFINITION))
(declare-function eshell/pwd "em-dirs" (&rest ARGS))
(declare-function vc-git--run-command-string "vc-git" (FILE &rest ARGS))
(eval-when-compile
  (defvar eshell-mode-map)
  (defvar eshell-prompt-function)
  (defvar eshell-prompt-regexp)
  (defvar eshell-visual-commands)
  (defvar eshell-visual-subcommands)
  (defvar eshell-history-size)
  (defvar eshell-buffer-maximum-lines)
  (defvar eshell-scroll-to-bottom-on-input)
  (defvar eshell-output-filter-functions)
  (defvar eshell-last-command-status))

(defface jme-shell-prompt-exit-success
  '((((class color) (background light)) :foreground "forest green")
    (((class color) (background   dark)) :foreground "green"))
  "Face for successful exit of last command."
  :group 'jme-customizations)

(defface jme-shell-prompt-exit-fail
  '((((class color) (background light)) :foreground "red")
    (((class color) (background  dark)) :foreground "red"))
  "Face for failure exit of last command."
  :group 'jme-customizations)

(defface jme-shell-prompt-git-branch
  '((((class color) (background light)) :foreground "purple")
    (((class color) (background  dark)) :foreground "thistle"))
  "Face for git branch in prompt."
  :group 'jme-customizations)

(defface jme-shell-prompt-git-status
  '((((class color) (background light)) :foreground "OrangeRed4")
    (((class color) (background  dark)) :foreground "OrangeRed2"))
  "Face for git branch in prompt."
  :group 'jme-customizations)

(defface jme-shell-prompt-timer
  '((((class color) (background light)) :foreground "gold3")
    (((class color) (background  dark)) :foreground "gold1"))
  "Face for timer in prompt."
  :group 'jme-customizations)

(defvar-local jme-shell--last-command-start nil
  "Holds the start time of the last command initiated.
This value is used to determine the duration of the command.")

(defvar jme-shell--minimum-diff-seconds 3
  "Threshold of seconds to display command duration.
If the duration is less than this value, no duration is displayed.")

(defun jme-shell--pre-command-function ()
  "Register start time before each command is invoked.
Intended to be used by `eshell-pre-command-hook'"
  (setq jme-shell--last-command-start (current-time)))

(defun jme-shell--time-diff (start-time end-time)
  "Format time difference between START-TIME and END-TIME.
Produces a string suitable for duration of a command.
If the difference is less than `jme-shell--minimum-diff-seconds',
or if start-time is nil, an empty string is returned."
  (if start-time
      (let* ((delta (time-subtract end-time start-time))
             (exceeds-threshold (> (cadr delta) jme-shell--minimum-diff-seconds))
             (has-mins (> (cadr delta) 60)))
        (if exceeds-threshold
            (concat " took "
                    (jme-common-with-face
                        (if has-mins
                            (format-time-string "%Mm %Ss" delta)
                          (format-time-string "%Ss" delta))
                      'jme-shell-prompt-timer))
          ""))
    ""))

(defun jme-shell--project-git-status ()
  "Git status for the current project."
  (split-string
   (with-output-to-string
     (with-current-buffer standard-output
       (process-file "git" nil '(t nil) nil
                     "-C" (expand-file-name (cdr (project-current)))
                     "status"
                     "--branch"
                     "--ignore-submodules=dirty"
                     "--porcelain=v2")))
   "\n" t))

(defun jme-shell--parse-git-status (status)
  "Parse a git STATUS, providing icons.
Expects a 1 or 2 status line from git status."
  (let ((stat (list (substring status 2 3) (substring status 3 4))))
    (flatten-tree
     (mapcar (lambda (s)
               (pcase s
                 ("D" "✘")
                 ("M" "✎")
                 ("A" "✎")))
             stat))))

(defun jme-shell--parse-git-branch-name (state)
  "Produce the git branch name based on STATE.
STATE is expected to be a list generated from the
output of git status."
  (let (branch)
   (dolist (status state branch)
     (when (string-prefix-p "# branch.head" status)
       (setq branch (substring status 14))))
   branch))

(defun jme-shell--vc-icons (state)
  "Construct a string of VC icons for STATE.
STATE is expected to be a list generated from the
output of git status."
  (delete-dups
   (flatten-tree
    (mapcar
     (lambda (s)
       (pcase (substring s 0 1)
         ("#" (when (string-prefix-p "# branch.ab" s)
                (let* ((plus (string-match "\+[0-9]+" s))
                       (minus (string-match "\-[0-9]+" s))
                       (ahead (substring s (1+ plus) (1- minus)))
                       (behind (substring s (1+ minus))))
                  (flatten-tree (list (unless (string= "0" ahead) "⇡")
                                      (unless (string= "0" behind) "⇣"))))))
         ("1" (jme-shell--parse-git-status s))
         ("2" (list "»" (jme-shell--parse-git-status s)))
         ("u" "=")
         ("?" "?")
         ("!" nil)
         (_ nil)))
     state))))

(defun jme-shell--current-vc-status ()
  "Constructs a string to use for current vc status."
  (let* ((project (project-current nil))
         (is-vc (eq 'vc (car project))))
    (when is-vc
      (let* ((git-status (jme-shell--project-git-status))
             (git-icons (apply #'concat (jme-shell--vc-icons git-status)))
             (git-icons-str (if (> (length git-icons) 0)
                                (jme-common-with-face
                                    (concat " [" git-icons "]")
                                  'jme-shell-prompt-git-status)
                              ""))
             (git-branch (jme-shell--parse-git-branch-name git-status)))
        (concat "on "
                (jme-common-with-face (concat " " git-branch) 'jme-shell-prompt-git-branch)
                git-icons-str)))))

(defun jme-shell--prompt-function ()
  "Format the eshell prompt."
  ;; git "clean" vs. "unclean"
  ;; git branch name
  ;; TODO: abbreviate filename further
  (let ((dir (abbreviate-file-name (eshell/pwd)))
        (duration (jme-shell--time-diff jme-shell--last-command-start (current-time)))
        (status (jme-shell--current-vc-status))
        (prompt (if (= (user-uid) 0) "# " "> "))
        (last-status (if (> eshell-last-command-status 0)
                         (jme-common-with-face "✘ " 'jme-shell-prompt-exit-fail)
                       (jme-common-with-face "✔ " 'jme-shell-prompt-exit-success))))
    (concat dir " " status duration "\n" last-status prompt)))

(defconst jme-shell--prompt-regexp "^[^$\n]*[✘✔]?> ")

;; Adapted from
;; http://www.howardism.org/Technical/Emacs/eshell-fun.html
;;
;; TODO: Check for existing buffer and switch to it
(defun jme-shell-window-popup ()
      "Invokes eshell in directory for the current buffer."
      (interactive)
      (let* ((parent (if (buffer-file-name)
                         (file-name-directory (buffer-file-name))
                       default-directory))
             (height (/ (window-total-height) 3))
             (name   (car (last (split-string parent "/" t)))))
        (split-window-vertically (- height))
        (other-window 1)
        (eshell "new")
        (rename-buffer (concat "*eshell: " name "*"))))

;; Adapted from
;; https://github.com/wasamasa/dotemacs/blob/master/init.org#eshell
;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org
(defun jme-shell-quit-or-delete-char (arg)
  "Support dual-mode \\`delete-forward-char' with ARG in eshell.
If characters exsit in front of point, detele them,
if not terminate the shell."
  (interactive "p")
  ;; TODO: possible speed up by passing non-nill to looking-back
  (if (and (eolp) (looking-back eshell-prompt-regexp nil))
      (progn
        (eshell-life-is-too-much)
        (ignore-errors
          (delete-window)))
    (delete-char arg)))

(defun jme-shell--configure-eshell ()
  "Initial configuration of eshell."
  (setq eshell-history-size         10000  ; Size of input history ring
        eshell-buffer-maximum-lines 10000  ; Size in lines for buffers
        eshell-scroll-to-bottom-on-input t ; input causses scroll
        eshell-prompt-function #'jme-shell--prompt-function
        eshell-prompt-regexp jme-shell--prompt-regexp) ;"^[^#$\n]* [#$] ")

  (setq eshell-visual-commands
        '("less" "tmux" "tail" "top" "bash" "zsh" "bat")
        eshell-visual-subcommands
        '(("git"
           "diff" "df" "dc"
           "show"
           "log" "lg" "tree" "lol" "lola" "lala" "ltla" "ldla"))))

(defun jme-shell--enable ()
  "Configure eshell and related."
  ;; save comman history
  (add-hook 'eshell-first-time-mode-hook #'jme-shell--configure-eshell)
  ;; Save the history for shell before commands
  (add-hook 'eshell-pre-command-hook #'eshell-save-some-history)
  ;; update the command time.
  (add-hook 'eshell-pre-command-hook #'jme-shell--pre-command-function)
  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions #'eshell-truncate-buffer)

  ;; Keybindings
  (global-set-key (kbd "C-!") 'jme-shell-window-popup)

  (define-key eshell-mode-map (kbd "C-d") #'jme-shell-quit-or-delete-char)

  ;; Aliases
  (eshell/alias "ff" "find-file $1")
  (eshell/alias "fo" "find-file-other-window $1")
  (eshell/alias "d" "dired $1")
  (eshell/alias "ll" "lsd $1")

  (eshell-syntax-highlighting-global-mode 1)

  (custom-set-variables
   '(tramp-default-method "ssh")))

(defun jme-shell--disable ()
  "Revert configuration."
  (eshell-syntax-highlighting-global-mode -1)
  (remove-hook 'eshell-pre-command-hook #'eshell-save-some-history)
  (remove-hook 'eshell-pre-command-hook #'jme-shell--pre-command-function)
  (remove-hook 'eshell-first-time-mode-hook #'jme-shell--configure-eshell)
  (jme-common-remove-from-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  ;; (setq eshell-output-filter-functions
  ;;       (delete 'eshell-truncate-buffer
  ;;               eshell-output-filter-functions))
  (global-unset-key (kbd "C-!"))
  ;; Just clear "C-d" from eshell map, since it exists in global map
  (define-key eshell-mode-map (kbd "C-d") nil))

(jme-common-defconfiguration jme-shell "Shell configuration")

(provide 'jme-shell)
;;; jme-shell.el ends here.
