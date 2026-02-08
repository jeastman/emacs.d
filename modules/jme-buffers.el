;; jme-buffers.el --- Buffer configuration -*- lexical-binding: t; -*-

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
;; This file contains buffer-specific configuration

;;; Code:
;; Auto-Revert.
;; For additional information see Info node `(emacs)Auto Revert'.
(require 'jme-common)
(require 'straight)
(straight-use-package 'all-the-icons-ibuffer)
(require 'all-the-icons-ibuffer)

(defun jme-buffers--configure-ibuffer ()
  "Configure iBuffer."
  (jme-common-customize-set-variables
   '((ibuffer-expert t)
     (ibuffer-display-summary nil)
     (ibuffer-use-other-window nil)
     (ibuffer-default-sorting-mode 'filename/process)
     (ibuffer-title-face 'font-lock-doc-face)
     (ibuffer-use-header-line t)
     (ibuffer-default-shrink-to-minimum-size nil)
     (ibuffer-formats
      ((mark modified read-only locked " "
             (name 30 30 :left :elide)
             " "
             (size 9 -1 :right)
             " "
             (mode 16 16 :left :elide)
             " " filename-and-process)
       (mark " "
             (name 16 -1)
             " " filename)))
     (ibuffer-saved-filter-groups
      (("Main"
         ("Directories" (mode . dired-mode))
         ("Markup" (or
                    (mode . markdown-mode)
                    (mode . adoc-mode)))
         ("Python" (or
                    (mode . python-ts-mode)
                    (mode . c-mode)
                    (mode . python-mode)))
         ("Config" (or
                    (mode . conf-mode)
                    (mode . conf-toml-mode)
                    (mode . toml-ts-mode)
                    (mode . conf-windows-mode)
                    (name . "^\\.clangd$")
                    (name . "^\\.gitignore$")
                    (name . "^Doxyfile$")
                    (name . "^config\\.toml$")
                    (mode . yaml-mode)))
         ("Web" (or
                 (mode . mhtml-mode)
                 (mode . html-mode)
                 (mode . web-mode)
                 (mode . nxml-mode)))
         ("CSS" (or
                 (mode . css-mode)
                 (mode . sass-mode)))
         ("JS" (or
                (mode . js-mode)
                (mode . rjsx-mode)))
         ("Org" (mode . org-mode))
         ("LaTeX" (name . "\\.tex$"))
         ("Magit" (or
                   (mode . magit-blame-mode)
                   (mode . magit-cherry-mode)
                   (mode . magit-diff-mode)
                   (mode . magit-log-mode)
                   (mode . magit-process-mode)
                   (mode . magit-status-mode)))
         ("Build" (or
                   (mode . make-mode)
                   (mode . makefile-gmake-mode)
                   (name . "^Makefile$")
                   (mode . change-log-mode)))
         ("Scripts" (or
                     (mode . shell-script-mode)
                     (mode . shell-mode)
                     (mode . sh-mode)
                     (mode . lua-mode)
                     (mode . bat-mode)))
         ("Fundamental" (or
                         (mode . fundamental-mode)
                         (mode . text-mode)))
         ("Emacs" (or
                   (mode . emacs-lisp-mode)
                   (name . "^\\*Help\\*$")
                   (name . "^\\*Custom.*")
                   (name . "^\\*Org Agenda\\*$")
                   (name . "^\\*info\\*$")
                   (name . "^\\*scratch\\*$")
                   (name . "^\\*Backtrace\\*$")
                   (name . "^\\*straight-process\\*$")
                  (name . "^\\*Messages\\*$"))))))))
  )

(defun jme-buffers--ibuffer-apply-groups ()
  "Apply the default ibuffer filter groups."
  (ibuffer-switch-to-saved-filter-groups "Main"))

(defun jme-buffers--enable-auto-revert ()
  "Enable auto-revert mode."
  ;; Both files and buffers should be reverted by
  ;; Global Auto-Revert Mode.
  (defvar global-auto-revert-non-file-buffers)
  (setq global-auto-revert-non-file-buffers t)
  ;; Silence auto-revert notifications
  (defvar auto-revert-verbose)
  (setq auto-revert-verbose nil)

  ;; Turn on global auto-revert mode
  (global-auto-revert-mode 1))

(defun jme-buffers--disable-auto-revert ()
  "Disable auto-revert, setting back to defaults."
  (jme-common-revert-symbols '(global-auto-revert-non-file-buffers
                        auto-revert-verbose))
  (global-auto-revert-mode -1))

(defun jme-buffers--config-uniquify ()
  "Configure buffer uniqueness."
  (setq
   ;; style name|bar/mumble
   uniquify-buffer-name-style 'post-forward
   ;; Separator for buffer name components
   uniquify-separator ":"
   ;; Regular expression for buffers to ignore
   uniquify-ignore-buffers-re "^\\*"))

(defun jme-buffers--revert-uniquify ()
  "Revert uniquify configuration."
  (jme-common-revert-symbols '(uniquify-buffer-name-style
                               uniquify-separator
                               uniquify-ignore-buffers-re)))

(defvar jme-buffers--saved-c-z-binding nil
  "Saved global binding for \\`suspend-frame\\' before \\`jme-buffers--enable\\'.")

(defvar jme-buffers--did-bind-c-z nil
  "Whether \\`jme-buffers--enable\\' updated the \\`suspend-frame\\' binding.")

(defun jme-buffers--enable ()
  "Apply buffers configuration."
  ;; Replace suspend frame if in graphical mode
  (when (display-graphic-p)
    (setq jme-buffers--saved-c-z-binding (lookup-key (current-global-map) (kbd "C-z")))
    (setq jme-buffers--did-bind-c-z t)
    (global-set-key (kbd "C-z") #'bury-buffer))
  (jme-buffers--enable-auto-revert)
  (jme-buffers--config-uniquify)
  (jme-buffers--configure-ibuffer)
  (add-hook 'ibuffer-mode-hook 'all-the-icons-ibuffer-mode)
  (add-hook 'ibuffer-mode-hook #'jme-buffers--ibuffer-apply-groups))

(defun jme-buffers--disable ()
  "Un-apply buffers configuration."
  (jme-buffers--disable-auto-revert)
  (jme-buffers--revert-uniquify)
  (remove-hook 'ibuffer-mode-hook 'all-the-icons-ibuffer-mode)
  (remove-hook 'ibuffer-mode-hook #'jme-buffers--ibuffer-apply-groups)
  (when jme-buffers--did-bind-c-z
    (define-key (current-global-map) (kbd "C-z") jme-buffers--saved-c-z-binding)
    (setq jme-buffers--did-bind-c-z nil)
    (setq jme-buffers--saved-c-z-binding nil)))

(defun jme-buffers-unload-function ()
  "Unload buffers feature."
  (jme-buffers--disable))

(jme-common-defconfiguration jme-buffers "Buffer configuration")

(provide 'jme-buffers)
;;; jme-buffers.el ends here
