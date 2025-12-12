;;; jme-markdown.el --- Configuration for markdown support and tooling  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  John Eastman

;; Author: John Eastman <jeastman@miggle.lan>
;; Keywords: convenience, languages

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'straight)
(require 'jme-common)
(straight-use-package 'markdown-mode)
(declare-function markdown-mode "markdown-mode" ())

(defun jme-markdown--pandoc-available-p()
  "Determine if pandoc is available."
  (executable-find "pandoc"))


(defun jme-markdown--markup-convert-region
    (start end from-format to-format &optional output-to-new-buffer yank-only)
  "Convert region (START END) or buffer content from FROM-FORMAT to TO-FORMAT.

If OUTPUT-TO-NEW-BUFFER is non-nil, display the result in a new buffer.
If YANK-ONLY is non-nil, do not modify the buffer—just push converted result
to `kill-ring`.

Requires Pandoc to be installed."
  (unless (executable-find "pandoc")
    (user-error "Pandoc is not installed or not found in PATH"))
  (let* ((src-buffer (current-buffer))
         (output-buffer (generate-new-buffer "*pandoc-output*"))
         (stderr-file (make-temp-file "pandoc-stderr"))
         (exit-code
          (call-process-region
           start end "pandoc"
           nil ;; do not delete region
           (cons output-buffer stderr-file)
           nil "-f" from-format "-t" to-format)))
    (unwind-protect
        (if (not (eq exit-code 0))
            (let ((stderr-output (with-temp-buffer
                                   (insert-file-contents stderr-file)
                                   (buffer-string))))
              (kill-buffer output-buffer)
              (error "Pandoc failed with exit code %d:\n\n%s" exit-code stderr-output))
          (with-current-buffer output-buffer
            (let ((converted-text (buffer-string)))
              ;; Push to kill-ring for yank
              (kill-new converted-text)
              (cond
               (yank-only
                (message "Converted text added to kill-ring."))
               (output-to-new-buffer
                (switch-to-buffer-other-window "*markup-convert*")
                (erase-buffer)
                (insert converted-text)
                (if (string= to-format "org") (org-mode) (markdown-mode)))
               (t
                ;; Replace in-place
                (with-current-buffer src-buffer
                  (save-excursion
                    (goto-char start)
                    (delete-region start end)
                    (insert converted-text))))))))
      (delete-file stderr-file)
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer)))))


(defun jme-markdown--markdown-to-org-convert (start end &optional output-to-new-buffer)
  "Convert Markdown region (START END) to Org-mode using Pandoc.

If OUTPUT-TO-NEW-BUFFER is non-nil, display the result in a new buffer.
See `jme-markdown--markup-convert-region' for behavior."
  (interactive
   (list
    (if (use-region-p) (region-beginning) (point-min))
    (if (use-region-p) (region-end) (point-max))
    current-prefix-arg))
  (jme-markdown--markup-convert-region start end "markdown" "org" output-to-new-buffer))


(defun jme-markdown--org-to-markdown-convert (start end &optional output-to-new-buffer)
  "Convert Org-mode region (START END) to Markdown using Pandoc.

If OUTPUT-TO-NEW-BUFFER is non-nil, display the result in a new buffer.
See `jme-markdown--markup-convert-region' for behavior."
  (interactive
   (list
    (if (use-region-p) (region-beginning) (point-min))
    (if (use-region-p) (region-end) (point-max))
    current-prefix-arg))
  (jme-markdown--markup-convert-region start end "org" "markdown" output-to-new-buffer))

(defun jme-markdown--markup-detect-input-format ()
  "Detect markup input format based on `major-mode'."
  (cond
   ((derived-mode-p 'markdown-mode 'gfm-mode) "markdown")
   ((derived-mode-p 'org-mode) "org")
   ;; fallback guess
   (t (user-error "Cannot detect input format from current buffer mode"))))

(defun jme-markdown--markup-guess-output-format (from-format)
  "Return the likely output format given FROM-FORMAT."
  (pcase from-format
    ("markdown" "org")
    ("gfm"      "org")
    ("org"      "markdown")
    (_ (user-error "Unknown input format: %s" from-format))))

(defun jme-markdown--markup-convert-region-auto (start end &optional output-to-new-buffer)
  "Auto-detect markup language and convert region (START END) using Pandoc.

Converts from the format implied by current `major-mode' to its logical pair.
See `jme-markdown--markup-detect-input-format' for rules.

If OUTPUT-TO-NEW-BUFFER is non-nil, display the result in a new buffer.
With prefix argument, sends output to a new buffer."
  (interactive
   (list
    (if (use-region-p) (region-beginning) (point-min))
    (if (use-region-p) (region-end) (point-max))
    current-prefix-arg))
  (let* ((from-format (jme-markdown--markup-detect-input-format))
         (to-format (jme-markdown--markup-guess-output-format from-format)))
    (jme-markdown--markup-convert-region start end from-format to-format output-to-new-buffer)))


(defun markup-convert-and-yank (start end)
  "Convert region (START END) or buffer and store result in `kill-ring` only.
Auto-detects input/output formats based on major mode."
  (interactive
   (list (if (use-region-p) (region-beginning) (point-min))
         (if (use-region-p) (region-end) (point-max))))
  (let* ((from-format (jme-markdown--markup-detect-input-format))
         (to-format (jme-markdown--markup-guess-output-format from-format)))
    (jme-markdown--markup-convert-region start end from-format to-format nil t)))


(defun jme-markdown--enable()
  "Enable markdown configuration.")

(defun jme-markdown--disable()
  "Disable markdown configuration.")

(jme-common-defconfiguration jme-markdown "Configuration for markdown support and tooling")

(provide 'jme-markdown)
;;; jme-markdown.el ends here
