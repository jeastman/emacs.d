;;; minalt-theme.el --- Custom face theme for Emacs

;; Copyright (C) 2011 John Eastman .

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(deftheme minalt
  "")

(custom-theme-set-faces
 'minalt
 '(default ((t (:background "#000000" :foreground "#ffffff"))))
 '(cursor ((t (:background "#000000" :foreground "#ffffff"))))
 '(fringe ((t (:background "#405060"))))
 '(mode-line ((t (:background "#323232" :foreground "#ffffff"))))
 '(mode-line-inactive ((t (:background "#323232" :foreground "#808080"))))
 '(minibuffer-prompt ((t (:foreground "#729fcf" :weight bold))))
 '(region ((t (:background "#323232"))))

 '(font-lock-comment-face ((t (:foreground "#9504f9"))))
 '(font-lock-constant-face ((t (:foreground "#177b82"))))
 '(font-lock-builtin-face ((t (:foreground "#abc4a4"))))
 '(font-lock-function-name-face ((t (:foreground "#1764b8"))))
 '(font-lock-variable-name-face ((t (:foreground "#8ce4af"))))
 '(font-lock-keyword-face ((t (:foreground "#49ff9d"))))
 '(font-lock-string-face ((t (:foreground "#67ffe0"))))
 '(font-lock-doc-string-face ((t (:foreground "#67FFE0"))))
 '(font-lock-type-face ((t (:foreground "#0081ff"))))
 '(font-lock-warning-face ((t (:foreground "#ff0000" :weight bold))))
 '(isearch ((t (:background "#cd00cd" :foreground "#b0e2ff"))))
 '(lazy-highlight ((t (:background "#afeeee"))))
 '(link ((t (:foreground "#0000ff" :underline t))))
 '(link-visited ((t (:foreground "#8b008b" :underline t))))
 '(flymake-errline ((t (:background "IndianRed4" :foreground "#f8f8f8"))))
 '(flymake-warnline ((t (:background "orange3" :foreground "#f8f8f8"))))
 '(whitespace-line ((t (:background "#800f00"))))
 '(button ((t (:underline t))))
 '(header-line ((t (:background "#e5e5e5" :foreground "#333333")))))

(provide-theme 'minalt)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; minalt-theme.el  ends here
