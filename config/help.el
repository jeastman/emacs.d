;;; help.el --- Help configuration -*- lexical-binding: t -*-

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

(use-package which-key
  :delight
  :config
  (setq which-key-key-replacement-alist
        '(("left" . "◀")
          ("right" . "▶")
          ("up" . "▲")
          ("down" . "▼")))
  (which-key-mode 1))

(use-package helpful
  :commands (helpful-at-point helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-variable-function #'helpful-variable)
  (counsel-describe-function-function #'helpful-callable)
  :bind (("C-h f" . helpful-callable)
         ([remap describe-variable] . counsel-describe-variable)
         ([remap describe-key] . helpful-key)
         ([remap describe-function] . counsel-describe-function)
         ([remap describe-command] . helpful-command)
         ("C-c C-d" . helpful-at-point)))

;;; help.el ends here
