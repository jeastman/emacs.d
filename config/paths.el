;;; paths.el --- Path configuration -*- lexical-binding: t -*-

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

(add-to-list 'load-path (concat user-emacs-directory "/lisp"))

;; Configure Metadata Directory
;; Use ~/.emacs-meta for all =Meta= information (backups, temp files, etc.)
(defvar metafiles-dir
  "~/.emacs-meta" "Custom location for metadata.")

(unless (file-exists-p metafiles-dir)
  (make-directory metafiles-dir))

(setq temporary-file-directory (concat metafiles-dir "/tmp/"))
(unless (file-exists-p temporary-file-directory)
  (make-directory temporary-file-directory))

(defvar meta-config (concat metafiles-dir "/etc/")
  "Location of etc directory for no-littering.")

(defvar meta-data (concat metafiles-dir "/var/")
  "Location of var directory for no-littering.")

(defvar meta-saveplace (concat metafiles-dir "/places")
  "Name of the file that records save-place-alist.")

(defvar meta-desktop (concat metafiles-dir "/desktop/")
  "Name of the desktop save directory.")

(defvar meta-bookmarks (concat metafiles-dir "/bookmarks")
  "Location for bookmarks file.")

(defvar meta-savehist (concat metafiles-dir "/savehist")
  "File used by savehist where minibuffer history is saved to and loaded.")

(defvar meta-recent (concat metafiles-dir "/recentf")
  "File to save the recent list into.")

(defvar meta-saves (concat metafiles-dir "/auto-save-list/.saves-")
  "Prefix to use for auto-save files.")

(defvar meta-tramp (concat metafiles-dir "/tramp")
  "File used for tramp persistence.")

(defvar meta-url (concat metafiles-dir "/url")
  "Directory for url files.")

(use-package no-littering
  :demand t
  :init
  (setq no-littering-etc-directory meta-config
        no-littering-var-directory meta-data))

(custom-set-variables '(bookmark-default-file meta-bookmarks)
                      '(eshell-directory-name (concat metafiles-dir "/eshell/"))
                      '(url-cookie-file (concat meta-url "/cookies"))
                      '(url-cache-directory (concat temporary-file-directory "url/cache")))

;; Projects directory
;; All of my machines utilize a similar project structure
;; rooted in a "Projects" directory
(defvar jme:projects-dir (expand-file-name "~/Projects/")
  "Project home directory.")

;;; paths.el ends here
