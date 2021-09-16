;;; override-dark-theme.el --- Theme overrides for dark themes -*- lexical-binding: t -*-

;; Author: John Eastman
;; Created: 12 Sep 2021

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
;; Provides a simple theme intended to overlay a "dark" theme
;; with personal customizations.
;;
;; General idea is to use it in addition to another theme.
;;
;; To use this theme pu the following in your startup file:
;;
;; (load-theme 'override-dark-theme t)
;;
;; This should be done after the "base" theme is loaded.
;;

;;; Code:
(deftheme override-dark
  "Override dark theme")

(custom-theme-set-variables
 'override-dark
 '(org-todo-keyword-faces
   '(("TODO" :foreground "DarkOrange" :weight bold)
     ("NEXT" :foreground "yellow" :weight bold)
     ("STARTED" :foreground "DarkOrange" :weight bold)
     ("WAITING" :foreground "gold" :weight bold)
     ("MAYBE" :foreground "SpringGreen")
     ("DONE" :foreground "DarkSeaGreen")
     ("STALLED" :foreground "SlateGrey")
     ("CANCELLED" :foreground "SlateGrey" :strike-through t)
     ("TASK" :foreground "DeepSkyBlue")
     ("RISK" :foreground "white" :background "OrangeRed")
     ("MITIGATED" :foreground "white" :background "DarkGreen"))))

(let* ((default-variable-font-family
         (if (boundp 'jme:variable-font-family)
             jme:variable-font-family
           "Noto Sans"))
       (variable-tuple
        (cond ((find-font (font-spec :family default-variable-font-family)) `(:font ,default-variable-font-family))
              ((find-font (font-spec :family "Source Sans Pro")) '(:font "Source Sans Pro"))
              ((find-font (font-spec :family "Lucida Grande")) '(:font "Lucida Grande"))
              ((find-font (font-spec :family "Verdana")) '(:font "Verdana" ))
              ((find-font (font-spec :family "Sans Serif")) '(:font "Sans Serif"))
              (nil (warn "Cannot find an appropriate variable font.")))))
  (custom-theme-set-faces
   'override-dark
   `(org-level-8 ((t (:inherit 'outline-8 ,@variable-tuple))))
   `(org-level-7 ((t (:inherit 'outline-7 ,@variable-tuple))))
   `(org-level-6 ((t (:inherit 'outline-6 ,@variable-tuple))))
   `(org-level-5 ((t (:inherit 'outline-5 ,@variable-tuple))))
   `(org-level-4 ((t (:inherit 'outline-4 ,@variable-tuple :height 1.0))))
   `(org-level-3 ((t (:inherit 'outline-3 ,@variable-tuple :height 1.05))))
   `(org-level-2 ((t (:inherit 'outline-2 ,@variable-tuple :height 1.1))))
   `(org-level-1 ((t (:inherit 'outline-1 ,@variable-tuple :height 1.2))))
   `(org-document-title ((t (:inherit org-level-1 :weight normal :height 1.75 :underline nil))))))

(provide-theme 'override-dark)

;;; override-dark-theme.el ends here
