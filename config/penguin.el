;; penguin.el --- Machine Specific Configuration -*- lexical-binding: t -*-

;; Author: John Eastman
;; Created: 18 Oct 2019

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
; TODO Fix problem with flatpak
(load (expand-file-name "~/.roswell/helper.el"))

(custom-set-variables
 '(bibtex-files '("/home/johneastman/Bibliography/refs.bib"))
 '(org-directory "~/org")
 '(org-roam-directory "~/Projects/n/notes")
 '(deft-directory "~/Projects/n/notes")
 '(reftex-default-bibliography '("~/Bibliography/refs.bib"))
 '(org-ref-bibliography-notes "~/org/notes.org")
 '(org-ref-default-bibliography '("~/Bibliography/refs.bib"))
 '(org-ref-pdf-directory "~/Bibliography/bibtex-pdfs/"))

(setq org-directory "~/org")

(use-package emoji-fontset
  :config
  (emoji-fontset-enable "Noto Emoji"))

;;; penguin.el ends here
