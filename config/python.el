;;; python.el --- Python Configuration -*- lexical-binding: t -*-

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
(setq python-indent-guess-indent-offset-verbose nil
      python-shell-interpreter "python3")


(use-package pyenv-mode
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode)
  (let ((global-pyenv
         (replace-regexp-in-string "\n" ""
                                   (shell-command-to-string "pyenv global"))))
    (pyenv-mode-set global-pyenv)
    (setq pyenv-current-version global-pyenv)))

(use-package pipenv
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

(use-package python-pytest
  :custom
  (python-pytest-executable "poetry run pytest"))

;; See: https://github.com/ambv/black
;; black will need to be installed separately
;; See also: https://github.com/rupert/pyls-black
;; for LSP integration
(use-package blacken
  :commands (blacken-mode))

;;; python.el ends here
