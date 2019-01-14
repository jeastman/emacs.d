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
 '(plantuml-jar-path "/usr/local/Cellar/plantuml/1.2018.14/libexec/plantuml.jar")
 '(org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2018.14/libexec/plantuml.jar")
 '(org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.11.0/libexec/ditaa-0.11.0-standalone.jar")
 '(org-directory "~/Documents/org")
 '(org-refile-targets '((org-agenda-files :maxlevel . 5)
                        (("~/Documents/org/task_archive.txt") :maxlevel . 5)
                        (nil :maxlevel . 5)))
 )

;;; 6542-13mbpr.el ends here
