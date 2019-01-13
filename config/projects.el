;;; projects.el --- Project support configuration -*- lexical-binding: t -*-

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

(use-package projectile
  :delight
  :custom
  (projectile-cache-file (concat metafiles-dir "/projectile.cache"))
  (projectile-known-projects-file (concat metafiles-dir "/projectile-bookmarks.eld"))
  (projectile-keymap-prefix (kbd "C-c p"))
  (projectile-enable-caching t)
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode))

(use-package counsel-projectile
  :after (ivy projectile)
  :config
  (counsel-projectile-mode 1))

;;; projects.el ends here
