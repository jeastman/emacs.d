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

(use-package eshell-git-prompt
  :after eshell
  :config
  (eshell-git-prompt-use-theme 'powerline))

(use-package eshell
  :hook (eshell-first-time-mode . jme:configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-visual-commands
        '("less" "tmux" "top" "bash" "zsh"))
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
