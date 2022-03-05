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
(eval-when-compile
  (defvar eshell-mode-map)
  (defvar eshell-prompt-regexp)
  (defvar eshell-visual-commands)
  (defvar eshell-visual-subcommands)
  (defvar eshell-history-size)
  (defvar eshell-buffer-maximum-lines)
  (defvar eshell-scroll-to-bottom-on-input)
  (defvar eshell-output-filter-functions))


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
        eshell-scroll-to-bottom-on-input t) ; input causses scroll

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
  (remove-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (remove-hook 'eshell-first-time-mode-hook 'jme-shell--configure-eshell)
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
