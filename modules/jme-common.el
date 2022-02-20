;; jme-common.el --- Common functions for configuration -*- lexical-binding: t; -*-

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
;;
;; Common functions for configuration.

;;; Code:
(defgroup jme-customizations ()
  "Personal customizations."
  :group 'emacs)

(defun jme-common--config-var-docstring (pretty-name)
  "Generate docstring for configuration variable.
PRETTY-NAME is the formatted name."
  (format "Non-nil if %s is enabled.

See the `%s' command for a description of the configuration.
Setting this varibale directly does not take effect;
either customize it (see the info mode 'Easy Cusomization')
or call the function `%s'." pretty-name pretty-name pretty-name))

(defun jme-common--config-fn-docstring (pretty-name getter)
  "Generate docstring for configuration.
PRETTY-NAME is the fomatted name.
GETTER is the symbol to evaluate to retrieve the state of the
configuration."
  (format "Toggle configuration %s on or off.

This is a configuration.  If called interactively, toggle the %s
configuration.  If the prefix argument is positive, enable the
configuration, and if it is zero or negative, disable the configuration.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
configuration if ARG is nil, omitted, or is a positive number.  Disable
the configuration if ARG is a negative number.

To check whether the configuration is enabled, evaluate `%s'."
          pretty-name pretty-name getter))

(defmacro jme-common-defconfiguration (config doc &optional body)
  "Define a new configuration CONFIG.
This defines the toggle command CONFIG and a control variable
CONFIG.
DOC is the documentation for the configuration toggle command.

The defined configuration command takes one optional (prefix) agrument.
Interactively with no prefix argument, it toggles the configuration.
A prefix argument enables the configuration if the argument is positive,
and disables it otherwise.

When called from Lisp, the configuration command toggles the configuration
if the argument is `toggle', disables the configuration if the argument
is a non-positive integer, and enables the configuration otherwise (including
if the argument is omitted or nil or a positive integer).

If DOC is nil, give the configuration command a basic doc-string
documenting what its argument does.  If the word \"ARG\" does not
appear in DOC, a paragraph is added to DOC explaining
usage of the configuration argument.

BODY may be defined specifying the actions to take when
enabling and disabling the configuration.

:enable - function to enable the configuration.
:diable - function to disable the configuration.

If the enable keyword is not specified, a function
named CONFIG--enable is checked for and executed.

If the disable keyword is not specified, a function
named CONFIG--disable is checked for and executed."
  (declare (debug (symbolp string &optional form))
           (indent fun))
  (let ((pretty-name (symbol-name config))
        (init-value nil)
        (enable nil)
        (disable nil)
        (keyw nil)
        (configfun config))
    (while (keywordp (setq keyw (car body)))
      (setq body (cdr body))
      (pcase keyw
        (:enable (setq enable (pop body)))
        (:disable (setq disable (pop body)))
        (_ (pop body))))
    (unless enable
      (if (functionp (intern (format "%s--enable" config)))
          (setq enable (intern (format "%s--enable" config)))
        (setq enable #'jme-common--default-enable)))
    (unless disable
      (if (functionp (intern (format "%s--disable" config)))
          (setq disable (intern (format "%s--disable" config)))
        (setq disable #'jme-common--default-disable)))
    `(progn
       (defcustom ,config ,init-value
         ,(concat doc "\n\n" (jme-common--config-var-docstring pretty-name))
         :initialize #'custom-initialize-default
         :type 'boolean
         :group 'jme-customizations)
       (defun ,configfun (&optional arg)
         ,(jme-common--config-fn-docstring config config)
         (interactive (list (if current-prefix-arg
                                (prefix-numeric-value current-prefix-arg)
                              'toggle)))
         (let ((last-message (current-message)))
           (if (setq-default ,config
                             (cond ((eq arg 'toggle)
                                    (not (default-value ',config)))
                                   ((and (numberp arg)
                                         (< arg 1))
                                    nil)
                                   (t t)))
               (,enable)
             (,disable))
           (if (called-interactively-p 'any)
               (progn
                 (customize-mark-as-set ',config)
                 (unless (and (current-message)
                              (not (equal last-message
                                          (current-message))))
                   (message ,(format "%s %%sabled" pretty-name)
                            (if (default-value ',config) "en" "dis"))))
             (default-value ',config)))))))

(defun jme-common--default-enable ()
  "Fallback configuration enable."
  (message "Configuration enabled."))

(defun jme-common--default-disable ()
  "Fallback configuration disable."
  (message "Configuration disabled."))

(defun jme-common-default-value-for-symbol (symbol)
  "Look for the default value for SYMBOL.
Checks the system for a value to use as the default for SYMBOL.  If SYMBOL
is a custom variable (defined with `defcustom'), the `standard-value' is used.
If no value can be found for SYMBOL, nil is returned."
  (let ((std-value (get symbol 'standard-value)))
    (if std-value
        (eval (car std-value))
      nil)))

(defun jme-common-revert-symbols (symbol-list)
  "Set the default value for each symbol in SYMBOL-LIST.
If a default value cannot be found for a symbol, it is skipped."
  (dolist (symbol symbol-list)
    (let ((value (jme-common-default-value-for-symbol symbol)))
      (if value
          (set symbol value)))))

(defmacro jme-common-autoload (fun package)
  "Autoload FUN for PACKAGE.

Ensures function FUN is not alreay bound."
  (declare (debug (symbolp symbolp))
           (indent 1))
  `(unless (fboundp ',fun)
     (autoload #',fun ,package nil t)))

(defmacro jme-common-enable-mode (mode)
  "Enable the specied MODE."
  (declare (debug (symbolp))
           (indent 1))
  `(if (fboundp ',mode)
       (,mode 1)))

(defmacro jme-common-disable-mode (mode)
  "Disable the specified MODE."
  (declare (debug (symbolp))
           (indent 1))
  `(if (fboundp ',mode)
       (,mode -1)))

(defun jme-common-remove-from-list (list element)
  "Remove from LIST the specified ELEMENT (destructive)."
  (setq list (delete element list)))

(defun jme-common-safe-unload-feature (feature)
  "Attempts to unload FEATURE.
Features which are loaded during init cannot be unloaded
without forcing the unload.  This function attempts to
check if FEATURE is in use only by init.el in which case
the FEATURE is forcefully unloaded."
  ;; TODO always forcefully unloads.
  ;; Need logic to detet if it is only used by init.el.
  (if (featurep feature)
      (unload-feature feature t)))

(defun jme-common-safe-unload-features (feature-list)
  "Attempt to unload a list of features speacified by FEATURE-LIST."
  (dolist (feature feature-list)
    (jme-common-safe-unload-feature feature)))

(provide 'jme-common)
;;; jme-common.el ends here.
