;;; shell.el --- Shell configuration -*- lexical-binding: t -*-

;; Author: John Eastman
;; Created: 06 Jan 2019

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
;;

;;; Code:

(defun jme:configure-eshell ()
  "Configure eshell."
  ;; save comman history
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignore-dups t
        eshell-scroll-to-bottom-on-input t))

;; Adapted from
;; http://www.howardism.org/Technical/Emacs/eshell-fun.html
;;
;; TODO: Check for existing buffer and switch to it
(defun jme:eshell-window-popup ()
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
(defun jme:eshell-quit-or-delete-char (arg)
    (interactive "p")
    (if (and (eolp) (looking-back eshell-prompt-regexp))
        (progn
          (eshell-life-is-too-much)
          (ignore-errors
            (delete-window)))
      (delete-forward-char arg)))

(use-package eshell-git-prompt
  :after eshell
  :config
  (eshell-git-prompt-use-theme 'powerline))

;; can't access eshell-mode-map before eshell is loaded, so cannot use it in :bind.
(use-package eshell
  :hook ((eshell-first-time-mode . jme:configure-eshell)
         (eshell-mode . (lambda ()
                          (bind-key "C-d" 'jme:eshell-quit-or-delete-char eshell-mode-map)
                          (eshell/alias "ff" "find-file $1")
                          (eshell/alias "fo" "find-file-other-window $1")
                          (eshell/alias "d" "dired $1")
                          (eshell/alias "ll" "lsd $1"))))
  :bind (("C-!" . jme:eshell-window-popup))
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-visual-commands
        '("less" "tmux" "tail" "top" "bash" "zsh" "bat"))
    (setq eshell-visual-subcommands
          '(("git"
             "diff" "df" "dc"
             "show"
             "log" "lg" "tree" "lol" "lola" "lala" "ltla" "ldla")))))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package tramp
  :custom
  (tramp-default-method "ssh")
  (tramp-persistency-file-name meta-tramp))

;;; shell.el ends here
