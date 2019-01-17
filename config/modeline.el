;;; modeline.el --- Modeline configuration  -*- lexical-binding: t -*-

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

(column-number-mode 1)               ; show column numbers
(line-number-mode 1)                 ; show line numbers
(size-indication-mode 1)

(use-package doom-modeline
  :defer t
  :commands (doom-modeline-init)
  :config
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-github t)
  :hook (after-init . doom-modeline-init))

(use-package minions
  :after (doom-modeline)
  :config
  (minions-mode 1))

(use-package spaceline-all-the-icons
  :disabled
  :after (spaceline))

(use-package spaceline
  :disabled
  :config
  (progn
    (dolist (s '((jme:spaceline-read-only "#4271AE" "Read only buffer face.")
                (jme:spaceline-modified "#F36C60" "Modified buffer face.")
                (jme:spaceline-unmodified "#78909C" "Unmodified buffer face.")))
      (eval `(defface ,(nth 0 s)
               `((t (:background ,(nth 1 s)
                                 :foreground "#3E3D31"
                                 :inherit 'mode-line)))
               ,(nth 2 s)
               :group 'spaceline)))

    (defun jme:spaceline-highlight-face-modified ()
      "Set the highlight face depending on the buffer modified status.
               Set `spaceline-highlight-face-func' to
               `spaceline-highlight-face-modified' to use this."
      (cond
       (buffer-read-only 'jme:spaceline-read-only)
       ((buffer-modified-p) 'jme:spaceline-modified)
       (t 'jme:spaceline-unmodified)))

    (setq-default spaceline-highlight-face-func 'jme:spaceline-highlight-face-modified)
    (setq-default spaceline-all-the-icons-separator-type 'cup)
    (spaceline-toggle-all-the-icons-modified-on)
    (spaceline-toggle-all-the-icons-bookmark-off)
    (spaceline-toggle-all-the-icons-dedicated-off)
    (spaceline-toggle-all-the-icons-window-number-off)
    (spaceline-toggle-all-the-icons-eyebrowse-workspace-off)
    (spaceline-toggle-all-the-icons-buffer-size-on)
    (spaceline-toggle-all-the-icons-projectile-on)
    (spaceline-toggle-all-the-icons-mode-icon-on)
    (spaceline-toggle-all-the-icons-buffer-id-on)
    (spaceline-toggle-all-the-icons-buffer-path-off)
    (spaceline-toggle-all-the-icons-process-off)
    (spaceline-toggle-all-the-icons-position-on)
    (spaceline-toggle-all-the-icons-region-info-on)
    (spaceline-toggle-all-the-icons-fullscreen-off)
    (spaceline-toggle-all-the-icons-text-scale-on)
    (spaceline-toggle-all-the-icons-multiple-cursors-off)
    (spaceline-toggle-all-the-icons-narrowed-on)
    (spaceline-toggle-all-the-icons-vc-icon-off)
    (spaceline-toggle-all-the-icons-vc-status-on)
    (spaceline-toggle-all-the-icons-git-status-off)
    (spaceline-toggle-all-the-icons-git-ahead-off)
    (spaceline-toggle-all-the-icons-flycheck-status-on)
    (spaceline-toggle-all-the-icons-flycheck-status-info-off)
    (spaceline-toggle-all-the-icons-package-updates-off)
    (spaceline-toggle-all-the-icons-org-clock-current-task-on)
    (spaceline-toggle-all-the-icons-hud-off)
    (spaceline-toggle-all-the-icons-buffer-position-off)
    (spaceline-toggle-all-the-icons-battery-status-on)
    (spaceline-toggle-all-the-icons-time-on)
    (spaceline-toggle-all-the-icons-which-function-on)
    (spaceline-toggle-all-the-icons-temperature-off)
    (spaceline-toggle-all-the-icons-weather-off)
    (spaceline-toggle-all-the-icons-minor-modes-off)
    (spaceline-toggle-all-the-icons-nyan-cat-off)
    (spaceline-all-the-icons-theme)
    (spaceline-all-the-icons--setup-paradox)))

;;; modeline.el ends here
