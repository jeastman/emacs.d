;;; lint.el --- Support for linters  -*- lexical-binding: t -*-
;; Copyright (C) 2019 John Eastman

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

(use-package flycheck
  :delight
  :custom
  (flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
  :config
  (global-flycheck-mode))

(use-package flycheck-pos-tip)

;;; lint.el ends here
