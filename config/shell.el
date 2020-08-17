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

(setq eshell-visual-commands
      '("less" "tmux" "top" "bash" "zsh"))

(setq eshell-visual-subcommands
      '(("git"
         "diff" "df" "dc"
         "show"
         "log" "lg" "tree" "lol" "lola" "lala" "ltla" "ldla")))

(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

(use-package tramp
  :custom
  (tramp-default-method "ssh")
  (tramp-persistency-file-name meta-tramp))

;;; shell.el ends here
