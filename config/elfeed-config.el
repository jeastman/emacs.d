;;; elfeed-config.el --- elfeed configuration -*- lexical-binding: t -*-

;; Author: John Eastman
;; Created: 02 April 2021

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
(use-package elfeed
  :commands (elfeed)
  :config
  (setq-default elfeed-search-filter "+unread @1-week-ago "))

(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list (concat org-directory "/elfeed.org"))))

(use-package elfeed-goodies
  :after (elfeed)
  :config
  (elfeed-goodies/setup))

(provide 'elfeed-config)
;;; elfeed-config.el ends here
