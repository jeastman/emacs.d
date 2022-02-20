;; jme-completion.el --- Completion configuration -*- lexical-binding: t; -*-

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
;; This file contains configuration for completion systems.
;; An attempt is made to build upon the built-in capabilities
;; of Emacs.

;;; Code:
(require 'straight)
(require 'jme-common)

(straight-use-package 'orderless)
(straight-use-package '( vertico :files (:defaults "extensions/*")
                         :includes (vertico-directory)))
(straight-use-package 'marginalia)
(straight-use-package 'embark)
(straight-use-package 'consult)
(straight-use-package 'embark-consult)
(straight-use-package 'corfu)
(straight-use-package 'kind-icon)
(straight-use-package 'pcmpl-args)
(straight-use-package 'cape)

;; From corfu documentaton, see:
;; https://github.com/minad/corfu
;; This enables completion in minibuffer for things like
;; `M-:' (`eval-expression') and `M-!' (`shell-command').
(defun jme-completion--corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    (setq-local corfu-auto t) ; Enable auto-completion
    (jme-common-enable-mode 1)))

(defun jme-completion--corfu-enable-in-eshell ()
  "Enable Corfu in eshell."
  (setq-local corfu-auto nil) ; Do not enable auto-completion
  (jme-common-enable corfu-mode))

(defun jme-completion--corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico is not active."
  (unless (bound-and-true-p vertico--input)
    ;; (setq-local corfu-auto nil) Enable/disable auto completion
    (jme-common-enable-mode corfu-mode)))

(defun jme-completion--enable ()
  "Configure completion system."

  ;; Emacs 28 introduced a predicate to determine
  ;; which commands to include when completing
  (when (symbolp read-extended-command-predicate)
    (setq read-extended-command-predicate
          #'command-completion-default-include-p))

  ;; threshold of completions to enable cycling
  (setq completion-cycle-threshold 3)
  ;; Attempt complete if indention is not available
  (setq tab-always-indent 'complete)

  ;; The `orderless' completion style which supports space-separated
  ;; comoonents.
  (require 'orderless)
  (setq completion-styles '(orderless)
          completion-category-defaults nil
          completion-category-overrides '((file (styles partial-completion))))

  ;; Use Vertico package for minimalistic vertical completion UI.
  (require 'vertico)
  (jme-common-enable-mode vertico-mode)

  ;; Configure vertico directory extension.
  (require 'vertico-directory)
  (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
  (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
  (define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay #'vertico-directory-tidy)


  ;; Add data to minibuffer completions
  (require 'marginalia)
  (jme-common-enable-mode marginalia-mode)
  (define-key minibuffer-local-map (kbd "M-A") #'marginalia-cycle)

  ;; Support actions
  (require 'embark)
  ;; use embark to show bindings in key-prefix with C-h
  (setq prefix-help-command #'embark-bindings)

  (global-set-key (kbd "C-.") #'embark-act)
  (global-set-key (kbd "M-.") #'embark-dwim)
  ;; (global-set-key (kbd "C-h B") #'embark-bindings)
  (global-set-key [remap describe-bindings] #'embark-bindings)

  ;; TODO Embark-consult

  ;; Completing-read commands
  (require 'consult)
  (add-hook 'completion-list-mode-hook 'consult-preview-at-point-mode)
  ;; C-c bindings (mode-specific-map)
  (global-set-key (kbd "C-c m") #'consult-mode-command)
  ;; C-s bindings
  (global-set-key (kbd "C-s") #'consult-line)
  ;; C-x bindings (ctl-x-map)
  (global-set-key (kbd "C-x b") #'consult-buffer)
  (global-set-key (kbd "C-x B") #'consult-buffer-other-window)
  (global-set-key (kbd "C-x 4 b") #'consult-buffer-other-window)
  (global-set-key (kbd "C-x 5 b") #'consult-buffer-other-frame)
  ;; M-g bindings (goto-map)
  (global-set-key (kbd "M-g M-g") #'consult-goto-line)
  (global-set-key (kbd "M-g o") #'consult-outline)
  (global-set-key (kbd "M-g m") #'consult-mark)
  (global-set-key (kbd "M-g k") #'consult-global-mark)
  ;; M-s bindings (search-map)
  (global-set-key (kbd "M-s d") #'consult-find)
  (global-set-key (kbd "M-s D") #'consult-locate)
  (global-set-key (kbd "M-s g") #'consult-grep)
  (global-set-key (kbd "M-s G") #'consult-git-grep)
  (global-set-key (kbd "M-s r") #'consult-ripgrep)
  (global-set-key (kbd "M-s l") #'consult-line)
  (global-set-key (kbd "M-s L") #'consult-line-multi)
  (global-set-key (kbd "M-s m") #'consult-multi-occur)
  (global-set-key (kbd "M-s k") #'consult-keep-lines)
  (global-set-key (kbd "M-s u") #'consult-focus-lines)
  ;; Minibuffer
  (define-key minibuffer-local-map (kbd "C-r") #'consult-history)

  (require 'embark-consult)
  (add-hook 'embark-collect-mode #'consult-preview-at-point-mode)

  (require 'corfu)
  (jme-common-enable-mode corfu-global-mode)
  (add-hook 'minibuffer-setup-hook #'jme-completion--corfu-enable-always-in-minibuffer 1)
  (add-hook 'minibuffer-setup-hook #'jme-completion--corfu-enable-in-minibuffer)
  (add-hook 'eshell-mode-hook #'jme-completion--corfu-enable-in)

  ;; Icons for completion kinds
  (custom-set-variables '(kind-icon-default-face 'corfu-default))
  (require 'kind-icon)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

  ;; pcomplete extension for eshell
  (require 'pcmpl-args)
  ;; The following avices are recommended by Corfu docs.
  ;; Silence the pcomplete capf, no errors or messages!
  ;;(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  ;;(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

  ;; Cape
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-keyword)

  )

(defun jme-completion--disable ()
  "Revert completion system configuration."
  (jme-common-remove-from-list 'completion-at-point-functions #'cape-file)
  (jme-common-remove-from-list 'completion-at-point-functions #'cape-tex)
  (jme-common-remove-from-list 'completion-at-point-functions #'cape-symbol)
  (jme-common-remove-from-list 'completion-at-point-functions #'cape-keyword)
  ;; clean up eshell hook
  (remove-hook 'eshell-mode-hook #'jme-completion--corfu-enable-in)
  ;; clean up minibuffer hook
  (remove-hook 'minibuffer-setup-hook #'jme-completion--corfu-enable-in-minibuffer)
  (remove-hook 'minibuffer-setup-hook #'jme-completion--corfu-enable-always-in-minibuffer)
  ;; clean up directory hook
  (remove-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  ;; clean up embark-consult hooks
  (remove-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)
  (remove-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)
  ;; turn off corfu
  (jme-common-disable-mode corfu-global-mode)
  ;; turn off vertico-mode to clean it up
  (jme-common-disable-mode vertico-mode)
  ;; turn off marginalia
  (jme-common-disable-mode marginalia-mode)

  ;; keybindings
  ;; consult minimap history
  (define-key minibuffer-local-map (kbd "C-r") nil)
  ;; marginalia cycle in minimap
  (define-key minibuffer-local-map (kbd "M-A") nil)
  ;; Embark
  (global-uset-key (kbd "C-."))
  (global-set-key (kbd "M-.") #'xref-find-definitions)
  (global-uset-key (kbd "C-h B"))
  ;; C-c bindings (mode-specific-map)
  (global-unset-key (kbd "C-c m"))
  ;; C-s bindings
  (global-set-key (kbd "C-s") #'isearch-forward)
  ;; C-x bindings (ctl-x-map)
  (global-set-key (kbd "C-x b") #'switch-to-buffer)
  (global-uset-key (kbd "C-x B"))
  (global-set-key (kbd "C-x 4 b") #'switch-to-buffer-other-window)
  (global-set-key (kbd "C-x 5 b") #'switch-to-buffer-other-frame)
  ;; M-g bindings (goto-map)
  (global-set-key (kbd "M-g M-g") #'goto-line)
  (global-uset-key (kbd "M-g o"))
  (global-uset-key (kbd "M-g m"))
  (global-uset-key (kbd "M-g k"))
  ;; M-s bindings (search-map)
  (global-uset-key (kbd "M-s d"))
  (global-uset-key (kbd "M-s D"))
  (global-uset-key (kbd "M-s g"))
  (global-uset-key (kbd "M-s G"))
  (global-uset-key (kbd "M-s r"))
  (global-uset-key (kbd "M-s l"))
  (global-uset-key (kbd "M-s L"))
  (global-uset-key (kbd "M-s m"))
  (global-uset-key (kbd "M-s k"))
  (global-uset-key (kbd "M-s u"))

  (jme-common-revert-symbols '(read-extended-command-predicate
                               completion-styles
                               completion-category-defaults
                               completion-category-overrides)))

(defun jme-completion-unload-function ()
  "Unload completion system feature."
  (jme-completion--disable)
  (jme-common-safe-unload-features '(pcmpl-args
                                     kind-icon
                                     corfu
                                     embark-consult
                                     consult
                                     embark
                                     marginalia
                                     vertico-directory
                                     vertico
                                     orderless)))

(jme-common-defconfiguration jme-completion "Completion configuration")

(provide 'jme-completion)
;;; jme-completion.el ends here
