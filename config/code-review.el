;;; code-review.el --- Utilities for managing code reviews -*- lexical-binding: t -*-

;; Author: John Eastman
;; Created: 31 Jan 2019

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
;; Capturing content from source files into org-mode block is
;; based largely on http://howardism.org/Technical/Emacs/capturing-content.html
;; The code presented there has simply been modified to my own tastes.

;;; Code:
(require 'which-func)

(defun jme:org-capture-fileref-snippet (type headers func-name)
  "Capture content of TYPE with HEADERS and possibly a FUNC-NAME.
Given a region of a buffer, this captures some metadata based on
the file from the current buffer which is placed in the caption.
An `org-mode' block is created based on TYPE and populated with
the contents of the marked region.  The metadata is formatted
into a string included before the `org-mode' block."
  (let* ((code-snippet
          (buffer-substring-no-properties (mark) (- (point) 1)))
         (file-name   (buffer-file-name))
         (file-base   (file-name-nondirectory file-name))
         (line-number (line-number-at-pos (region-beginning)))
         (caption-text (format "%s line %s" file-base line-number))
         (initial-txt (if (null func-name)
                          (format "[[file:%s::%s][%s]]:"
                                  file-name line-number file-base)
                        (format "~%s~ (in [[file:%s::%s][%s]]):"
                                func-name file-name line-number
                                file-base))))
    (format "
   %s

   #+CAPTION: %s
   #+begin_%s %s
%s
   #+end_%s" initial-txt caption-text type headers code-snippet type)))

(defun jme:org-capture-clip-snippet (f)
  "Capture a snippet of content from file F.
Given a file, F, this captures the currently selected text
within an Org example block and a backlink to the file."
  (with-current-buffer (find-buffer-visiting f)
    (jme:org-capture-fileref-snippet "example" "" nil)))


(defun jme:org-capture-code-snippet (f)
  "Capture a code snippet from file F.
Given a file, F, this captures the currently selected text within
an Org src block with a language based on the current mode and a
backlink to the function and the file.  Capturing the function
name requires `which-function' to be available enabled by
function `which-function-mode'."
  (with-current-buffer (find-buffer-visiting f)
    (let ((org-src-mode (replace-regexp-in-string "-mode" "" (format "%s" major-mode)))
          (func-name (if (functionp 'which-function)
                         (which-function)
                       nil)))
      (jme:org-capture-fileref-snippet "src" org-src-mode func-name))))

;;; code-review.el ends here
