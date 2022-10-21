;; jme-python.el --- Configuration for Python programmng -*- lexical-binding: t; -*-

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

;; Provides configuration of python for programming.

;;; Code:
(require 'straight)
(require 'jme-common)
(straight-use-package 'blacken)
(straight-use-package 'py-isort)
(straight-use-package 'pyvenv)
(straight-use-package 'poetry)

(defun jme-python--enable ()
  "Enable configuration."
  (add-hook 'python-mode-hook 'blacken-mode)
  (add-hook 'before-save-hook 'py-isort-before-save))

(defun jme-python--disable ()
  "Disable configuration."
  (remove-hook 'before-save-hook 'py-isort-before-save)
  (remove-hook 'python-mode-hook 'blacken-mode))

(jme-common-defconfiguration jme-python "Configuration for Python programming")

(provide 'jme-python)
;;; jme-python.el ends here
