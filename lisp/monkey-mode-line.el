;;; monkey-mode-line.el --- Monkey Mode line

;; Copyright (C) 2013 John Eastman <john.eastman@gmail.com>

;; Author: John Eastman <john.eastman@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: faces frames
;; prefix: monkeyml
;; Separator: /

;;; Commentary:
;;
;; Monkey Mode Line is an alternative mode-line for Emacs.
;;
;; Installation
;; ===
;; Make sure "monkey-mode-line.el" is in your load path, then place the
;; following code in your .emacs file:
;;
;;     (require 'monkey-mode-line)
;;     (monkeyml/monkey-mode-line)

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;;; Code:

(require 'custom)
(require 'cus-face)

(defconst monkeyml/version "1.0" "Version of the monkey-mode-line.el package.")
(defconst monkeyml/version-int 1 "Version of the monkey-mode-line.el package, as an integer.")
(defun monkeyml/customize ()
  "Open the customization group for the `monkey-mode-line' package."
  (interactive)
  (customize-group 'monkey-mode-line t))
(defun monkeyml/customize-faces ()
  "Open the customization group for faces used by the `monkey-mode-line' package."
  (interactive)
  (customize-group 'smart-mode-line-faces t))

(defgroup monkey-mode-line '()
  "Customization group for the `monkey-mode-line` package."
  :group 'convenience
  :prefix 'monkeyml)
(defgroup monkey-mode-line-faces '()
  "Font (face) colors for the `monkey-mode-line.el' package.

You can fully customize any of the fonts to match the color you
want. You can also set properties like bold with ':weight bold'."
  :prefix 'monkeyml
  :group 'monkey-mode-line
  :group 'faces)

;; Extra mode line faces
(defface monkeyml/read-write-face
  '((t (:inherit mode-line-face :foreground "gray60")))
  "Read write but unmodified state face"
  :group 'monkey-mode-line-faces)

(defface monkeyml/read-only-face
  '((t (:inherit mode-line-face :background "#4271ae" :foreground "gray60" :box '(:line-width 2 :color "#4271ae"))))
  "Read only state face"
  :group 'monkey-mode-line-faces)

(defface monkeyml/modified-face
  '((t (:inherit mode-line-face :foreground "#8b0000" :background "#8b0000" :box '(:line-width 2 :color "#8b0000"))))
  "Modified state face"
  :group 'monkey-mode-line-faces)

(defface monkeyml/folder-face
  '((t (:inherit mode-line-face :foreground "gray60")))
  "Directory/folder display"
  :group 'monkey-mode-line-faces)

(defface monkeyml/buffer-name-face
  '((t (:inherit mode-line-face :foreground "#eab700" :weight bold)))
  "Buffer name display"
  :group 'monkey-mode-line-faces)

(defface monkeyml/position-face
  '((t (:inherit mode-line-face)))
  "Position display"
  :group 'monkey-mode-line-faces)

(defface monkeyml/col-over-face
  '((t (:inherit monkeyml/position-face :forground "#000000" :background "#eab700")))
  "Column over threshold indicator face"
  :group 'monkey-mode-line-faces)

(defface monkeyml/major-mode-face
  '((t (:inherit mode-line-face :foreground "gray80")))
  "Major mode display"
  :group 'monkey-mode-line-faces)

(defface monkeyml/minor-mode-face
  '((t (:inherit mode-line-face :foreground "gray40")))
  "Minor mode display"
  :group 'monkey-mode-line-faces)

(defface monkeyml/process-face
  '((t (:inherit mode-line-face :foreground "#718c00")))
  "Process display"
  :group 'monkey-mode-line-faces)

(defcustom monkeyml/buffer-name-width 30
  "Maximum size of the buffer name in the mode-line."
  :type 'integer
  :group 'monkey-mode-line)

(defconst monkeyml/mode-line-active-foreground-original (internal-get-lisp-face-attribute 'mode-line :foreground))
(defconst monkeyml/mode-line-active-background-original (internal-get-lisp-face-attribute 'mode-line :background))
(defconst monkeyml/mode-line-inactive-foreground-original (internal-get-lisp-face-attribute 'mode-line-inactive :foreground))
(defconst monkeyml/mode-line-inactive-background-original (internal-get-lisp-face-attribute 'mode-line-inactive :background))

; preserve the original mode-line so we can revert if necessary
(defvar monkeyml/default-mode-line mode-line-format)

(defun monkeyml/mode-line-revert ()
  "Revert changes to the mode line made by Monkey Mode Line package."
  (interactive)
  (set-face-attribute 'mode-line nil
                      :foreground monkeyml/mode-line-active-foreground-original
                      :background monkeyml/mode-line-active-background-original)
  (set-face-attribute 'mode-line-inactive nil
                      :foreground monkeyml/mode-line-inactive-foreground-original
                      :background monkeyml/mode-line-inactive-background-original)
  (setq-default mode-line-format monkeyml/default-mode-line))

(defun monkeyml/buffer-status ()
  (cond (buffer-read-only
             (propertize " R " 'face 'monkeyml/read-only-face))
            ((buffer-modified-p)
             (propertize " * " 'face 'monkeyml/modified-face))
            (t (propertize " - " 'face 'monkeyml/read-write-face))))

(defun monkeyml/modes ()
  (let ((recursive-edit-help-echo "Recursive edit, type C-M-c to get out"))
    (list " "
          (propertize "%[" 'help-echo recursive-edit-help-echo)
          `(:propertize ("" mode-name)
                        face monkeyml/major-mode-face
                        help-echo "Major mode\n\ mouse-1: Display major mode menu\n\ mouse-2: Show help for major mode\n\ mouse-3: Toggle minor modes"
                        mouse-face mode-line-highlight
                        local-map ,mode-line-major-mode-keymap)
          `(:propertize ("" mode-line-process)
                        face monkeyml/process-face)
          `(:propertize ("" minor-mode-alist)
                        face monkeyml/minor-mode-face
                        mouse-face mode-line-highlight
                        help-echo "Minor mode\n\ mouse-1: Display minor mode menu\n\ mouse-2: Show help for minor mode\n\ mouse-3: Toggle minor modes"
                        local-map ,mode-line-minor-mode-keymap)
          (propertize "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
                      'mouse-face 'mode-line-highlight
                      'local-map (make-mode-line-mouse-map
                                  'mouse-2 #'mode-line-widen))
          (propertize "%]" 'help-echo recursive-edit-help-echo)
          " ")))

(defun monkeyml/buffer-name ()
  '(:propertize mode-line-buffer-identification face monkeyml/buffer-name-face))

(defun monkeyml/fill-size ()
  (let* ((left (length (format-mode-line (monkeyml/modes))))
         (right (+ 4 ; reserve spacing
                   (length (format-mode-line (monkeyml/position))) 1
                   (length (format-mode-line global-mode-string)) 1
                   (max 0
                        (or
                         (when (and (boundp 'which-function-mode) which-function-mode)
                          (length (format-mode-line which-func-format)))
                         0))))
         (center (max 0 (- (window-body-width) (+ right left)))))
    center))

(defun monkeyml/padded-buffer-name ()
  (let* ((buf-len (+ (length (format-mode-line mode-line-buffer-identification))
                     (length (format-mode-line vc-mode))))
         (pad (max 0  (- (monkeyml/fill-size) buf-len)))
         (hpad (/ pad 2)))
    (list (if (< 0 hpad) (make-string hpad ?\ ))
          '(:propertize mode-line-buffer-identification face monkeyml/buffer-name-face)
          '(vc-mode vc-mode)
          (if (< 0 hpad) (make-string hpad ?\ )))))

(defun monkeyml/position ()
  '((:propertize "%4l:" face monkeyml/position-face)
    (:eval (propertize "%3c" 'face
                       (if (>= (current-column) 80)
                           'monkeyml/col-over-face
                         'monkeyml/position-face)))))

(defun monkeyml/monkey-mode-line ()
  "Enable the Monkey Mode Line."
  (interactive)
  (set-face-attribute 'mode-line nil
                      :foreground "gray80" :background "gray20"
                      :inverse-video nil
                      :box '(:line-width 6 :color "gray20" :style nil))
  (set-face-attribute 'mode-line-inactive nil
                      :foreground "gray60" :background "gray14"
                      :inverse-video nil
                      :box '(:line-width 6 :color "gray14" :style nil))
    (setq-default
     mode-line-format
     '(
       ; Left
       ("%e")
       (:eval (monkeyml/buffer-status))

       ; Modes
       (:eval (monkeyml/modes))
       ; Center
       ; Buffer identifier
       (:eval (monkeyml/padded-buffer-name))

       ; Right
       ; which function
       (:eval
        (when (and (boundp 'which-function-mode) which-function-mode)
          which-func-format))

       ; Position
       (:eval (monkeyml/position))
       " "
       ; global
       (global-mode-string global-mode-string)
       )
    )
  )

(provide 'monkey-mode-line)
