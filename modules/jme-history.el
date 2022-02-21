;; jme-history.el --- Remembering things and backing up. -*- lexical-binding: t; -*-

;; Author: John Eastman
;; Created: 12 Feb 2022

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
;; This file contains configuration for remembering things
;; and backing up.

;;; Code:
(require 'straight)
(require 'jme-common)
(straight-use-package 'recentf)
(straight-use-package 'savehist)

(defun jme-history--enable ()
  "Configure history settings."

  ;; File backup settings
  (setq version-control t     ; Control use of version numbers for backup files
        kept-new-versions 2   ; Number of newest versions to keep
        kept-old-versions 2   ; Number of oldest versions to keep
        delete-old-versions t ; Delete excess backup versions silently
        backup-by-copying-when-linked t)

  (custom-set-variables
   '(recentf-max-saved-items 100 "Save 100 recent items.")
   '(recentf-max-menu-items 15 "Maximum number of items in the recentf menu."))

  (require 'recentf)
  (jme-common-enable-mode recentf-mode)

  (custom-set-variables
   '(savehist-additional-variables
     '(kill-ring search-ring regexp-search-ring))
   '(savehist-autosave-interval 300)
   '(history-delete-duplicates t))

  (require 'savehist)
  (jme-common-enable-mode savehist-mode)

  (jme-common-enable-mode desktop-save-mode)                ; maintain sessions across invocations
  (jme-common-enable-mode save-place-mode))                 ; remember where you were in files

(defun jme-history--disable ()
  "Revert settings for this module."
  (jme-common-disable-mode recentf-mode)
  (jme-common-disable-mode savehist-mode)
  (jme-common-disable-mode desktop-save-mode)
  (jme-common-disable-mode save-place-mode))

(defun jme-history-unload-function ()
  "Unload feature."
  (jme-history--disable)
  (jme-common-safe-unload-features '(savehist
                                     recentf)))

(jme-common-defconfiguration jme-history "History configuration")

(provide 'jme-history)
;;; jme-history.el ends here.
