;; jme-typescript.el --- Configuration for Typescript programmng -*- lexical-binding: t; -*-

;; Copyright (C) 2024  John Eastman

;; Author: John Eastman <john.eastman@gmail.com>
;; Keywords: convenience, languages

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides configuration of Typescript for programming.

;;; Code:
(require 'straight)
(require 'jme-common)
(require 'treesit)
(straight-use-package 'combobulate)

(defun jme-typescript-grammars ()
  "Install tree-sitter grammars if they are absent."
  (dolist (grammar
           '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.23.1"))
             (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.23.2"))
             (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1" "src"))
             (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.24.8"))
             (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src"))
             (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src"))
             (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
    (add-to-list 'treesit-language-source-alist grammar)
    ;; only install `grammer' if we don't already have it installed
    (unless (treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar)))))


(defun jme-typescript-enable ()
  "Enable configuration."
  (custom-set-variables '(js-indent-level 2))
  (dolist (mapping
    '((css-mode . css-ts-mode)
      (typescript-mode . typescript-ts-mode)
      (js-mode . typescript-ts-mode)
      (js2-mode . typescript-ts-mode)
      (json-mode . json-ts-mode)
      (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  (jme-typescript-grammars))

(defun jme-typescript-disable ()
  "Disable the configuration."
  )

(jme-common-defconfiguration jme-typescript "Configuration for Typescript programming")

(provide 'jme-typescript)
;;; jme-typescript.el ends here
