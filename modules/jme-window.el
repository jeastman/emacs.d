;; jme-window.el --- Window configuration. -*- lexical-binding: t; -*-

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
(require 'jme-common)

(defun jme-window--enable ()
  "Perform window configuration."
  ;; The menu is kind of a waste of space for me
  ;; Disable it when not on MacOS (darwin)
  (if (eq window-system 'nil)
      (if (fboundp 'menu-bar-mode)
          (jme-common-disable-mode menu-bar-mode))
    (if (fboundp 'menu-bar-mode)
        (if (string-equal system-type "darwin")
            (jme-common-enable-mode menu-bar-mode)
          (jme-common-disable-mode menu-bar-mode))))

  ;; Scroll settings (optimized for trackpad)
  (custom-set-variables '(scroll-step 1)
                        '(mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
                        '(mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling
                        '(mouse-wheel-follow-mouse 't)))                 ; scroll window under mouse

(defun jme-window--disable ()
  "Revert window configuration.")

(jme-common-defconfiguration jme-window "Window configuration")

(provide 'jme-window)
;;; jme-window.el ends here.
