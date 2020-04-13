;;; 6542-13mbpr.el --- Machine Specific Configuration -*- lexical-binding: t -*-

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

(custom-set-variables
 '(plantuml-jar-path (car (file-expand-wildcards
                           "/usr/local/Cellar/plantuml/*/libexec/plantuml.jar")))
 '(org-plantuml-jar-path (car (file-expand-wildcards
                               "/usr/local/Cellar/plantuml/*/libexec/plantuml.jar")))
 '(org-ditaa-jar-path (car (file-expand-wildcards
                            "/usr/local/Cellar/ditaa/*/libexec/ditaa-*-standalone.jar")))
 '(ob-mermaid-cli-path "/usr/local/bin/mmdc")
 '(org-directory "~/Documents/org")
 '(org-roam-directory "~/Documents/notes")
 '(deft-directory "~/Documents/notes")
 '(org-refile-targets '((org-agenda-files :maxlevel . 5)
                        (("~/Documents/org/task_archive.txt") :maxlevel . 5)
                        (nil :maxlevel . 5)))
 '(reftex-default-bibliography "~/Documents/Bibliography/refs.bib")
 '(org-ref-bibliography-notes "~/Documents/org/notes.org")
 '(org-ref-default-bibliography "~/Documents/Bibliography/refs.bib")
 '(org-ref-pdf-directory "~/Documents/Bibliography/bibtex-pdfs/")
 )

(setq org-directory "~/Documents/org")

;; CLASSPATH needs to include eclipse.jdt.ls jar in order for lsp to work.
(let ((lsp-jar
       (car
        (file-expand-wildcards
         (concat
          jme:projects-dir
          "e/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/plugins/org.eclipse.equinox.launcher_*.jar")))))
  (setenv "CLASSPATH"
          (concat (getenv "CLASSPATH") ":" lsp-jar)))


(load (expand-file-name "~/.roswell/helper.el"))

;; Export org to confluence
(require 'ox-confluence)
;;; 6542-13mbpr.el ends here
