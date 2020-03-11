;;; darwin.el --- MacOS specific configuration -*- lexical-binding: t -*-

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
;; Specific customization when running on Mac.

;;; Code:

(setq-default ispell-program-name "/usr/local/bin/aspell")

(setq markdown-command "/usr/local/bin/mmd")

(setenv "NODE_PATH" "/usr/local/lib/node_modules")

;; Make use of the Mac system trash
(if (boundp 'mac-system-move-file-to-trash-use-finder)
    (setq mac-system-move-file-to-trash-use-finder t)
  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash/emacs"))

;; Support for homebrew installed packages
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Use the OPTION key as SUPER, while preserving it as ALT in other cases
(custom-set-variables '(mac-option-modifier '(:ordinary super :function alt :mouse alt)))

;; Mac emoji font
(when (fboundp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    '(#x1F600 . #x1F64F)
                    (font-spec :name "Apple Color Emoji") nil 'prepend))

;; Send notifications to the Mac Notification Center.
(cl-defun jme:message-notify (title text &key speak-it (max-text-len 64))
  "Post a message with the given TITLE and TEXT to the notifications center.
If SPEAK-IT is not NIL, will also say the message. Any text over MAX-TEXT-LEN
will be truncated.

If the ~deferred~ package is available, the notification is processed
asynchronously.

This is a Mac OS X specific function.  Note that double-quotes that occur in
the TITLE and the TEXT are replaced with single quotes."
  (let ((title (replace-regexp-in-string "\"" "'" title))
        (text (replace-regexp-in-string "\"" "'" (if (> (length text) max-text-len)
                                                     (substring text 0 max-text-len)
                                                   text)))
        (fn (lambda (title text speak-it) (do-applescript (concat
                                                           "display notification \""
                                                           text
                                                           "\" with title \""
                                                           title
                                                           (if speak-it "\"" "\" sound name \"Pop\"")))
              (when speak-it
                (do-applescript (concat
                                 "say \""
                                 text
                                 "\" using \""
                                 "Tessa"
                                 "\" without waiting until completion"))))))
    (if (require 'deferred nil 'noerror)
        (deferred:call fn title text speak-it)
      (funcall fn title text speak-it))))


;; Ensure we activate frames when created

(defun jme:raise-emacs ()
  "Raise Emacs window."
  (when (memq window-system '(mac ns))
    (mac-do-applescript "tell application \"Emacs\" to activate")))

(defun jme:raise-emacs-with-frame (frame)
  "Raise Emacs and select the provided FRAME."
  (with-selected-frame frame
    (when (display-graphic-p)
      (jme:raise-emacs))))

(add-hook 'after-make-frame-functions 'jme:raise-emacs-with-frame)

;; Start the emacs server
(when (and (functionp 'server-running-p) (not (server-running-p)))
  (progn
    (server-start)
    (require 'org-protocol)))
;;; darwin.el ends here
