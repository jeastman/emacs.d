;; monkey-spaceline.el -- Monkey Spaceline Theme

;; Author: John Eastman <john.eastman@gmail.com>
;; Version: 1.0
;; Keywords mode-line powerline spaceline
;; Package-Requires: ((spaceline "2.0.1"))

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Theme for the spaceline mode-line library.

;;; Code:
(require 'spaceline)
(require 'spaceline-all-the-icons)

;; Extra mode line faces
(defface monkey-read-write-face
  '((t (:inherit mode-line-face :family "FontAwsome" :foreground "gray60")))
  "Read write but unmodified state face"
  :group 'monkey-spaceline-faces)

(defface monkey-read-only-face
  '((t (:inherit mode-line-face :family "FontAwsome" :background "#4271ae" :foreground "gray60" :box '(:line-width 2 :color "#4271ae"))))
  "Read only state face"
  :group 'monkey-spaceline-faces)

(defface monkey-modified-face
  '((t (:inherit mode-line-face :family "FontAwsome" :foreground "#f36c60" :background "gray60" :box '(:line-width 2 :color "#f36c60"))))
  "Modified state face"
  :group 'monkey-spaceline-faces)

(defface monkey-info-face
  '((t (:foreground "gray60")))
  "Face for `monkey' info feedback in the modeline."
  :group 'monkey-spaceline-faces)

(defun monkey--face-foreground (face)
  "Get the foreground of FACE or `default' face."
  (or (face-foreground face)
      (face-foreground 'default)))

(defun monkey--highlight ()
  "Return the `mouse-face' highlight face to be used when propertizing text.
This is done as a function rather than a static face as it
doesn't inherit all properties of a face."
  `((foreground-color . ,(monkey--face-foreground 'monkey-info-face))))

(spaceline-define-segment monkey-version-control
  "Version control information."
  (when vc-mode
    (powerline-raw
     (s-trim (concat vc-mode
                     (when (buffer-file-name)
                       (pcase (vc-state (buffer-file-name))
                         (`up-to-date " ")
                         (`edited " ✱")
                         (`added " ✚")
                         (`unregistered " ◼")
                         (`removed " ✖")
                         (`needs-merge " ═")
                         (`needs-update " ⬇")
                         (`ignored " ✭")
                         (_ " ??"))))))))


(spaceline-define-segment monkey-buffer-modified
  "Buffer modified marker"
  (let* ((buffer-state (format-mode-line "%*"))
         (icon (cond
                ((string= buffer-state "-") "square-o")
                ((string= buffer-state "*") "pencil")
                ((string= buffer-state "%") "lock")))
         (fg-color (cond
                    ((string= buffer-state "-") "gray60")
                    ((string= buffer-state "*") "#f36c60")
                    ((string= buffer-state "%") "#4271ae"))))
    (propertize (all-the-icons-faicon icon :v-adjust 0.0)
                'face `(:family ,(all-the-icons-faicon-family) :height ,(spaceline-all-the-icons--height 1.1) :background "#1c1f26" :foreground ,fg-color)
                'mouse-face (spaceline-all-the-icons--highlight)
                'local-map (make-mode-line-mouse-map 'mouse-1 'read-only-mode)))
  :tight t)

(spaceline-define-segment monkey-buffer-modified-orig
  "Buffer modified marker."
  (let* ((buffer-state (format-mode-line "%*"))
         (icon (cond
                ((string= buffer-state "-") "square-o")
                ((string= buffer-state "*") "pencil")
                ((string= buffer-state "%") "lock")))
         (face (cond
                ((string= buffer-state "-") `(:family ,(all-the-icons-faicon-family) :height 1.1 :background ,(face-background 'mode-line)))
                ((string= buffer-state "*") `(:family ,(all-the-icons-faicon-family) :background "f36c60" :height 1.1 :box '(:line-width 2 :color "#f36c60")))
                ((string= buffer-state "%") `(:family ,(all-the-icons-faicon-family) :background "#4271ae" :height 1.1 :box '(:line-width 2 :color "#4271ae"))))))
    (propertize (concat " " (all-the-icons-faicon icon :v-adjust 0.0) " ")
                'face face)))


(spaceline-define-segment monkey-buffer-modified-busted
  "Buffer modified marker."
  (let* ((buffer-state (format-mode-line "%*"))
         (icon (cond
                ((string= buffer-state "-") "square-o")
                ((string= buffer-state "*") "pencil")
                ((string= buffer-state "%") "lock")))
         (face (cond
                ((string= buffer-state "-") `((:family ,(all-the-icons-faicon)
                                                       :height 1.1
                                                       :foreground "gray60"
                                                       :inherit mode-line-face)))
                ((string= buffer-state "*") `((:family ,(all-the-icons-faicon)
                                                       :height 1.1
                                                       :forground "gray60"
                                                       :background "#f36c60"
                                                       :inherit mode-line-face)))
                ((string= buffer-state "%") `((:family ,(all-the-icons-faicon)
                                                       :height 1.1
                                                       :forground "gray60"
                                                       :background "#4271ae"
                                                       :inherit mode-line-face))))))
    (propertize (all-the-icons-faicon "lock" :v-adjust 0.0)
                'face face
                'mouse-face (monkey--highlight)
                'local-map (make-mode-line-mouse-map 'mouse-1 'read-only-mode))))

(spaceline-define-segment monkey-clock
  "Display the time."
  nil)

(setq powerline-default-separator 'wave)

(defun spaceline-monkey-theme (&rest additional-segments)
  "Based on spaceline--theme function.

ADDITIONAL-SEGMENTS are inserted on the right, between `global'
and `buffer-position'."
  (spaceline-install
    '(monkey-buffer-modified
      buffer-id
      remote-host
      additional-segments
      major-mode
      (process :when active)
      ((flycheck-error flycheck-warning flycheck-info)
       :when active)
      (minor-modes :when active)
      (mu4e-alert-segment :when active)
      (monkey-version-control :when active))

    `(which-function
      (battery :when active)
      selection-info
      ((point-position line-column) :separator " | ")
      (global :when active)
      ,@additional-segments
      buffer-position
      hud))

  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

(defconst spaceline-monkey-theme2 '("%e" (:eval (spaceline-ml-monkey)))
  "Constant version of variable `spaceline-monkey-theme' to allow to be set manually.")

(defun spaceline-monkey-theme2 (&rest additional-segments)
  "Install the monkey theme
Add ADDITIONAL-SEGMENTS to the end of the theme."
  (interactive)
  (spaceline-compile
    "monkey"
    '(monkey-buffer-modified
      buffer-id
      remote-host
      additional-segments
      major-mode
      (process :when active)
      ((flycheck-error flycheck-warning flycheck-info)
       :when active)
      (minor-modes :when active)
      (mu4e-alert-segment :when active)
      (monkey-version-control :when active))

    `(which-function
      (battery :when active)
      selection-info
      ((point-position line-column) :separator " | ")
      (global :when active)
      ,@additional-segments
      buffer-position
      hud))
  (setq-default mode-line-format spaceline-monkey-theme2))

  (provide 'monkey-spaceline)
;;; monkey-spaceline.el ends here
