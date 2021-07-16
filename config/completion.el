;;; completion.el --- Configuration of completion systems -*- lexical-binding: t -*-

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

;; Utilities for enhancing company completion
;; See: https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/
(defun company-yasnippet-or-completion ()
  "Expand snippet or complete."
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  "Check for expansion."
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  "Expand snippet."
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  "Insert a tab or do completion.

Take special care to call 'org-cycle' in 'org-mode'."
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (if (eq major-mode 'org-mode)
              (org-cycle)
            (indent-for-tab-command))))))

(use-package company
  :delight
  :custom
  (company-idle-delay 0.5)
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations 't)
  :commands (company-mode global-company-mode company-complete
                          company-complete-common company-manual-begin
                          company-grab-line)
  :bind (:map company-active-map
          ("C-n". company-select-next)
          ("C-p". company-select-previous)
          ("M-<". company-select-first)
          ("M->". company-select-last)
          :map company-mode-map
          ("<tab>". tab-indent-or-complete)
          ("TAB". tab-indent-or-complete)
          )
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package pos-tip
  :commands (pos-tip-show))

(use-package company-quickhelp
  :after (company pos-tip)
  :commands (company-quickhelp-mode)
  :config
  (company-quickhelp-mode 1))

(use-package company-statistics
             :after company
             :config
             (company-statistics-mode))

(use-package company-flx
  :after company
  :config
  (company-flx-mode +1))

;;; completion.el ends here
