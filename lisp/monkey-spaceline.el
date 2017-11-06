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

;; Extra mode line faces
(defface monkey-read-write-face
  '((t (:inherit mode-line-face :foreground "gray60")))
  "Read write but unmodified state face"
  :group 'monkey-spaceline-faces)

(defface monkey-read-only-face
  '((t (:inherit mode-line-face :background "#4271ae" :foreground "gray60" :box '(:line-width 2 :color "#4271ae"))))
  "Read only state face"
  :group 'monkey-spaceline-faces)

(defface monkey-modified-face
  '((t (:inherit mode-line-face :foreground "#f36c60" :background "#f36c60" :box '(:line-width 2 :color "#f36c60"))))
  "Modified state face"
  :group 'monkey-spaceline-faces)

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
  "Buffer modified marker."
  (cond (buffer-read-only
         (propertize " R " 'face 'monkey-read-only-face))
        ((buffer-modified-p)
         (propertize " * " 'face 'monkey-modified-face))
        (t (propertize " - " 'face 'monkey-read-write-face))))

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

(provide 'monkey-spaceline)
;;; monkey-spaceline.el ends here
