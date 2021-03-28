;;; history.el --- History configuration -*- lexical-binding: t -*-

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

(use-package recentf
  :custom
  (recentf-save-file meta-recent "Set the save file to be in meta area.")
  (recentf-max-saved-items 100 "Save 100 recent items.")
  (recentf-max-menu-items 15 "Maximum number of items in the recentf menu.")
  :config
    (recentf-mode t))

(use-package savehist
  :custom
  (savehist-additional-variables
   '(kill-ring search-ring regexp-search-ring))
  (savehist-autosave-interval 300)
  (savehist-file meta-savehist)
  (history-delete-duplicates t)
  :config
  (savehist-mode t))

(custom-set-variables '(save-place-file meta-saveplace))

(desktop-save-mode t)                ; maintain sessions across invocations
(save-place-mode t)                  ; remember where you were in files

;;; history.el ends here
