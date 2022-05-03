;;; jme-news.el --- Configuration for news reading and sources  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  John Eastman

;; Author: John Eastman <jeastman@miggle.lan>
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

;; Provides configuration of elfeed for reading RSS content.

;;; Code:
(require 'straight)
(require 'jme-common)
(straight-use-package 'elfeed)
(straight-use-package 'elfeed-org)
(straight-use-package 'elfeed-goodies)
(defvar org-directory)
(defvar rmh-elfeed-org-files)
(declare-function elfeed-org "elfeed-org" ())
(declare-function elfeed-goodies/setup "elfeed-goodies" ())


(defun jme-news--enable()
  "Enable news configuration."
  (setq-default elfeed-search-filter "+unread @1-week-ago")
  (elfeed-org)
  (setq rmh-elfeed-org-files (list (concat org-directory "/elfeed.org")))
  (elfeed-goodies/setup))

(defun jme-news--disable()
  "Disable news configuration.")

(jme-common-defconfiguration jme-news "Configuration for news reading")

(provide 'jme-news)
;;; jme-news.el ends here
