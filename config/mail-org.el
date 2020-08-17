;;; mail-org.el --- Customizations to handle email handling in Org mode -*- lexical-binding: t -*-

;; Author: John Eastman
;; Created: 30 Apr 2019

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

;; Implementation based largely on content from org-mime library.

;; Thoughts and ideas based on information in blog article by John Kitchin
;; See http://kitchingroup.cheme.cmu.edu/blog/2016/10/29/Sending-html-emails-from-org-mode-with-org-mime/

;; Content from the above article is licenced under the Creative Commons
;; Attribution-ShareAlike 4.0 International License.
;; http://creativecommons.org/licenses/by-sa/4.0/deed.en_US
;; For information on how the adaptation of information in the referenced
;; blog article is to be handled, please see
;; https://creativecommons.org/share-your-work/licensing-considerations/compatible-licenses

;;; Commentary:
;;

;;; Code:
;;(require 'org-mime)
;;(require 'org-mu4e)

(defun jme:mu4e-compose-org-mail ()
  "Compose an email in org format."
  (interactive)
  (mu4e-compose-new)
  (org-mu4e-compose-org-mode))

(defun jme:htmlize-and-send ()
  "When in an org-mu4e-compose-org-mode message, htmlize and send it."
  (interactive)
  (when (member 'org~mu4e-mime-switch-headers-or-body post-command-hook)
    (jme:org-mime-htmlize)
    (message-send-and-exit)))

;; org-mime
;; Below are customized versions of the related org-mime functions.
;; I've attempted to stay as close as possible to the original functions,
;; including debug support and comments.
;; They have been customized to handle what I belive to be missing features.
;; Most notably, including a plain text mime-part and proper handling of attachments.

(defun jme:org-mime-multipart (plain html &optional images)
  "Markup a multipart/alternative with PLAIN and HTML alternatives.
If html portion of message includes IMAGES they are wrapped in multipart/related part."
  (concat "<#multipart type=alternative><#part type=text/plain>"
          ;; CHANGE: Include plain text part
          plain
          (when images "<#multipart type=related>")
          "<#part type=text/html>"
          (if org-mime-beautify-quoted-mail
              (org-mime-beautify-quoted html)
            html)
          images
          (when images "<#/multipart>\n")
          "<#/multipart>\n"))

(defun jme:org-mime-insert-content (body file s opts)
  "Insert BODY content as multipart, using temp FILE, body string S and options OPTS."
  (let* ((files (with-temp-buffer
                  (insert body)
                  (org-mime-extract-non-image-files)))
         ;; CHANGE: above allows the files to be extracted from original content.

         ;; dvipng for inline latex because MathJax doesn't work in mail
         ;; Also @see https://github.com/org-mime/org-mime/issues/16
         ;; (setq org-html-with-latex nil) sometimes useful
         (org-html-with-latex org-mime-org-html-with-latex-default)
         ;; we don't want to convert org file links to html
         (org-html-link-org-files-as-html nil)
         (org-link-file-path-type 'absolute)
         ;; makes the replies with ">"s look nicer
         (org-export-preserve-breaks org-mime-preserve-breaks)
         ;; org 9
         (org-html-htmlize-output-type 'inline-css)
         ;; org 8
         (org-export-htmlize-output-type 'inline-css)
         ;; CHANGE: Create plain text part
         (plain (org-mime--export-string s 'ascii opts))
         (html-and-images (org-mime-replace-images (org-mime--export-string s 'html opts)
                                                   file))
         (images (cdr html-and-images))
         (html (org-mime-apply-html-hook (car html-and-images))))

    ;; If there are files that were attached, we should remove the links,
    ;; and mark them as attachments. The links don't work in the html file.
    (when files
      (progn
        (mapc (lambda (f)
                (setq html (replace-regexp-in-string
                            ;; CHANGE: support proper regex for files
                            (format "<a href=\"file://%s\">file://%s</a>"
                                    (regexp-quote f) (regexp-quote f))
                            (format "%s (attached)" (file-name-nondirectory f))
                            html)))
              files)
        ;; CHANGE: update references in plain text part
        (mapc (lambda (f)
                (setq plain (replace-regexp-in-string
                             (format "<file:%s>" (regexp-quote f))
                             (format "%s (attached)" (file-name-nondirectory f))
                             plain)))
              files)))

    (insert (jme:org-mime-multipart plain html (if images (mapconcat 'identity images "\n"))))

    ;; Attach any residual files
    (when files
      (mapc (lambda (f)
              (when org-mime-debug (message "attaching: %s" f))
              (mml-attach-file f))
            files))))

(defun jme:org-mime-htmlize ()
  "Export a portion of an email to multipart using `org-mode'.
If called with an active region only export that region, otherwise entire body.
Similar to `org-mime' `org-mime-htmlize' except that it also includes a plain part."
  (interactive)
  (when org-mime-debug (message "jme:org-mime-htmlize called"))
  (let* ((region-p (org-region-active-p))
         (html-start (funcall org-mime-find-html-start
                              (or (and region-p (region-beginning))
                                  (save-excursion
                                    (goto-char (point-min))
                                    (search-forward mail-header-separator)
                                    (+ (point) 1)))))
         (html-end (or (and region-p (region-end))
                       (point-max)))
         (body (buffer-substring
                html-start html-end))
         (str (concat org-mime-default-header body))
         (file (make-temp-name (expand-file-name
                                "mail" temporary-file-directory)))


         ;; to hold attachments for inline html images
         (opts (if (fboundp 'org-export--get-inbuffer-options)
                   (org-export--get-inbuffer-options))))

    ;; insert new current
    (delete-region html-start html-end)
    (goto-char html-start)
    (jme:org-mime-insert-content body file str opts)))

;;; mail-org.el ends here
