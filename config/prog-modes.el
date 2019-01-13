;;; prog-modes.el --- Configuration for programming specific modes -*- lexical-binding: t -*-

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

;; Asciidoc
(use-package adoc-mode
  :commands (adoc-mode)
  :mode ("\\.adoc\\'" . adoc-mode)
  :hook (adoc-mode-hook . (lambda() (buffer-face-mode t))))

;; Javascript
(use-package js2-mode
  :commands (js2-mode)
  :mode ("\\.js$" . js2-mode)
  :bind (:map js2-mode-map
              ("C-x C-e" . js-send-last-sexp)
              ("C-M-x" . js-send-last-sexp-and-go)
              ("C-c b" . js-send-buffer)
              ("C-c l" . js-load-file-and-go))
  :init
  (setq js-indent-level 2)
  (setq js-strict-missing-semi-warning nil)
  (add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode)))

;; JSON
(use-package json-mode
  :commands (json-mode)
  :mode ("\\.json$" . json-mode))

;; JSX support
(use-package rjsx-mode
  :commands (rjsx-mode)
  :mode ("components\\/.*\\.js\\'" . rjsx-mode)
  :after js2-mode)

(load (concat jme:config-dir "/python.el"))

;;; prog-modes.el ends here
