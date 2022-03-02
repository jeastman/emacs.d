;; jme-editor.el --- Editor configuration -*- lexical-binding: t; -*-

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

;;; Code:
(require 'straight)
(require 'jme-common)

;; linting and error support
(straight-use-package 'flycheck)
(straight-use-package 'flycheck-pos-tip)
;; LSP client
(straight-use-package 'eglot)
;; Alternative to built-in help which provides more information
(straight-use-package 'helpful)
;; Color delimiters, like parens, in paris
(straight-use-package 'rainbow-delimiters)
;; Increase selection by region
(straight-use-package 'expand-region)
;; Helpful package for visualizing color identifiers.
(straight-use-package 'rainbow-mode)
;; Match parens and more
(straight-use-package 'smartparens)
;; simple environment for writing
(straight-use-package 'olivetti)

;;; Customization options

(defcustom jme-editor-line-number-enabled-modes
  '(text-mode prog-mode conf-mode)
  "Modes which should have line numbers enabled."
  :type 'list
  :group 'jme-customizations)

(defcustom jme-editor-line-number-disabled-modes
  '(org-mode)
  "Modes which should not have line numbers enabled.
Modes which should not have line numbers and may be
derived from modes which are included in
`jme-editor-line-number-enabled-modes' should be listed here."
  :type 'list
  :group 'jme-customizations)

(defun jme-editor--enable ()
  "Configure editor settings."

  (custom-set-variables
   '(fill-column 75) ; Column beyond which automatic line-wrapping should happen.
   '(tab-width 4)    ; Distance between tab stops
   '(indent-tabs-mode nil)      ; Indentation cannot insert tabs
   '(next-line-add-newlines t)  ; `next-line' inserts newline
   '(select-enable-clipboard t) ; Cutting and pasting uses clipboard
   '(require-final-newline t)   ; Add newline automatically at end of file
   '(indicate-empty-lines t))   ; Visually indicate unused screen lines at end of buffer.

  ;; highlight matching parens
  (jme-common-enable-mode show-paren-mode)
  ;; configure smartparens
  (require 'smartparens-config)

  (jme-editor--add-hooks)

  ;; flycheck
  (custom-set-variables
   '(flycheck-emacs-lisp-load-path 'inherit)
   '(flycheck-display-errors-function
     #'flycheck-pos-tip-error-messages))
  (require 'flycheck)
  (require 'flycheck-pos-tip)
  (jme-common-enable-mode global-flycheck-mode)

  ;; Help bindings
  (declare-function helpful-at-point "helpful" ())
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (declare-function helpful-function "helpful" (SYMBOL))
  (global-set-key (kbd "C-h F") #'helpful-function)
  (declare-function helpful-command "helpful" (SYMBOL))
  (global-set-key (kbd "C-h x") #'helpful-command)
  (declare-function helpful-callable "helpful" (SYMBOL))
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (declare-function helpful-key "helpful" (KEY-SEQUENCE))
  (global-set-key (kbd "C-h k") #'helpful-key)
  (declare-function helpful-symbol "helpful" (SYMBOL))
  (global-set-key (kbd "C-h o") #'helpful-symbol)
  (declare-function helpful-variable "helpful" (SYMBOL))
  (global-set-key (kbd "C-h v") #'helpful-variable)

  ;; expand region
  (declare-function er/expand-region "expand-region" (ARG))
  (global-set-key (kbd "C-=") #'er/expand-region)

  ;; (use-package visual-fill-column
  ;;   :config (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
  ;;   :hook ((org-mode . jme-editor-visual-fill)
  ;;          (eww-mode . jme-editor-visual-fill)
  ;;          (elfeed-show-mode . jme-editor-visual-fill)))

  (custom-set-variables
   '(whitespace-style '(face trailing space-before-tab
                             indentation space-after-tab))
   '(whitespace-line-column 80))
  (jme-common-enable-mode global-whitespace-mode))

(defun jme-editor--add-hooks ()
  "Add mode hooks to the desired modes.

Add hooks for the various features.  Line number modes
are specified by `jme-editor-line-number-enabled-modes'
and `jme-editor-line-number-disabled-modes'."
  ;; add line numbers to desired modes
  (dolist (mode jme-editor-line-number-enabled-modes)
    (add-hook (intern (format "%s-hook" mode)) #'jme-editor--enable-line-numbers-mode))
  ;; disable line number in desired modes
  (dolist (mode jme-editor-line-number-disabled-modes)
    (add-hook (intern (format "%s-hook" mode)) #'jme-editor--disable-line-numbers-mode))
  ;; Whitespace cleanup
  (add-hook 'before-save-hook
            #'jme-editor-cleanup-buffer-safe)
  (declare-function rainbow-delimiters-mode "rainbow-delimiters" (&optional ARG))
  (add-hook 'prog-mode-hook
            #'rainbow-delimiters-mode)
  (declare-function smartparens-global-mode "smartparens" (&optional ARG))
  (add-hook 'after-init-hook
            #'smartparens-global-mode))

(defun jme-editor--remove-hooks ()
  "Remove mode hooks to the desired modes.

Remove hooks for the various festures.  Line number modes
are specified by `jme-editor-line-number-enabled-modes'
and `jme-editor-line-number-disabled-modes'."
  (remove-hook 'after-init-hook #'smartparens-global-mode)
  (remove-hook 'prog-mode-hook
               #'rainbow-delimiters-mode)
  ;; Whitespace cleanup
  (remove-hook 'before-save-hook
               #'jme-editor-cleanup-buffer-safe)
  ;; remove line numbers to desired modes
  (dolist (mode jme-editor-line-number-enabled-modes)
    (remove-hook (intern (format "%s-hook" mode)) #'jme-editor--enable-line-numbers-mode))
  ;; remove the disabling of line number in desired modes
  (dolist (mode jme-editor-line-number-disabled-modes)
    (remove-hook (intern (format "%s-hook" mode)) #'jme-editor--disable-line-numbers-mode)))

(defun jme-editor--enable-line-numbers-mode ()
  "Turn on line numbers mode.

Used as hook to modes that should display line numbers."
  (display-line-numbers-mode 1))

(defun jme-editor--disable-line-numbers-mode ()
  "Turn off line numbers mode.

Used as hook to modes that should not display line numbers."
  (display-line-numbers-mode -1))

;; ;; Center buffer
;; (defun jme-editor-visual-fill ()
;;   "Visually center a buffer."
;;   (interactive)
;;   (setq visual-fill-column-width 100
;;         visual-fill-column-center-text t)
;;   (visual-fill-column-mode 1))


(defun jme-editor--disable ()
  "Revert editor settings."
  ;; remove hooks
  (jme-editor--remove-hooks)
  (remove-hook 'before-save-hook #'jme-editor-cleanup-buffer-safe)

  (jme-common-disable-mode show-paren-mode)
  (jme-common-disable-mode global-whitespace-mode)
  (jme-common-disable-mode global-flycheck-mode)
  (jme-common-disable-mode global-whitespace-mode)

  ;; deregister key-bindings
  (global-unset-key (kbd "C-="))
  (global-unset-key (kbd "C-c C-d"))
  (global-set-key (kbd "C-h x") #'describe-command)
  (global-set-key (kbd "C-h f") #'describe-function)
  (global-set-key (kbd "C-h k") #'describe-key)
  (global-set-key (kbd "C-h o") #'describe-symbol)
  (global-set-key (kbd "C-h v") #'describe-variable)
  (global-set-key (kbd "C-h F") #'Info-goto-emacs-command-node))


;; Whitespace cleanup can be dangerous if it changes the content of the
;; file. Some changes are guaranteed to be safe, which this function sticks
;; to, allowing it to be safe for a before-save-hook.
;; SEE: http://whattheemacsd.com/buffer-defuns.el-01.html
(defun jme-editor-cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a `before-save-hook', and that
might be bad."
  (interactive)
  (unless (eq major-mode 'makefile-bsdmake-mode)
    (untabify (point-min) (point-max)))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

;; less safe version
(defun jme-editor-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (jme-editor-cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

(defun jme-editor-unload-function ()
  "Unload editor feature."
  (jme-editor--disable)
  (jme-common-safe-unload-features '(flycheck-pos-tip
                                     flycheck
                                     smartparens-config)))

(jme-common-defconfiguration jme-editor
                             "Editor configuration.")
(provide 'jme-editor)
;;; jme-editor.el ends here.
