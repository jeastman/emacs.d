;;; 6542-13mbpr-pkg.el --- Machine Specific Packages -*- lexical-binding: t -*-

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

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; General Email settings
(setq message-kill-buffer-on-exit t)

(use-package org-mime
  :bind (:map message-mode-map
              ("C-c h" . org-mime-htmlize))
  :custom
  (org-mime-default-header "#+OPTIONS: latex:t toc:nil H:3 num:nil\n")
  :init
  (progn
    (defun jme/org-mime-html-hook ()
      "Update the pre tag css for org-mime exports."
      (let* ((my-pre-bg (face-background 'default))
             (my-pre-fg (face-foreground 'default)))
        (org-mime-change-element-style
         "pre" (format "background-color: %s; color: %s; padding: 0.5em;"
                       my-pre-bg my-pre-fg))))
    (setq org-mime-preserve-breaks nil)
    (add-hook 'org-mime-html-hook 'jme/org-mime-html-hook)))

(defun jme:notmuch-tag-star-icon ()
  "Return SVG data representing a star icon.
This can be used with `notmuch-tag-format-image-data'"
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
   <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
   <svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"16\" height=\"16\" viewBox=\"0 0 16 16\">
     <g transform=\"translate(-242.81601,-315.59635)\">
       <path
         d=\"m 290.25762,334.31206 -17.64143,-11.77975 -19.70508,7.85447 5.75171,-20.41814 -13.55925,-16.31348 21.19618,-0.83936 11.325,-17.93675 7.34825,19.89939 20.55849,5.22795 -16.65471,13.13786 z\"
         transform=\"matrix(0.2484147,-0.02623394,0.02623394,0.2484147,174.63605,255.37691)\"
         style=\"fill:#ff0000;fill-rule:evenodd;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1\" />
     </g>
   </svg>")

 (defun jme:notmuch-tag-flag-icon ()
  "Return SVG data representing a flag icon.
This can be used with `notmuch-tag-format-image-data'"
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
   <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
   <svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"16\" height=\"16\" viewBox=\"0 0 16 16\">
     <path fill=\"#F36C60\" d=\"M5.016 16c-1.066-2.219-0.498-3.49 0.321-4.688 0.897-1.312 1.129-2.61 1.129-2.61s0.706 0.917 0.423 2.352c1.246-1.387 1.482-3.598 1.293-4.445 2.817 1.969 4.021 6.232 2.399 9.392 8.631-4.883 2.147-12.19 1.018-13.013 0.376 0.823 0.448 2.216-0.313 2.893-1.287-4.879-4.468-5.879-4.468-5.879 0.376 2.516-1.364 5.268-3.042 7.324-0.059-1.003-0.122-1.696-0.649-2.656-0.118 1.823-1.511 3.309-1.889 5.135-0.511 2.473 0.383 4.284 3.777 6.197z\"></path>
   </svg>")

 (defun jme:notmuch-tag-inbox-icon ()
   "Return SVG data representing inbox icon.
 This can be used with `notmuch-tag-format-image-data'"
 "<?xml version=\"1.0\" encoding=\"utf-8\"?>
  <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
  <svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"16\" height=\"16\" viewBox=\"0 0 16 16\">
    <path fill=\"#c0ff3e\" d=\"M8 9l4-4h-3v-4h-2v4h-3zM11.636 7.364l-1.121 1.121 4.064 1.515-6.579 2.453-6.579-2.453 4.064-1.515-1.121-1.121-4.364 1.636v4l8 3 8-3v-4z\"></path>
  </svg>")

 (defun jme:notmuch-tag-activity-icon ()
   "Return SVG data representing activity stream icon.
 This can be used with `notmuch-tag-format-image-data'"
   "<?xml version=\"1.0\" encoding=\"utf-8\"?>
    <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
    <svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"18\" height=\"16\" viewBox=\"0 0 18 16\">
      <path fill=\"#c0ff3e\" d=\"M7.5 0v0c4.142 0 7.5 2.717 7.5 6.069s-3.358 6.069-7.5 6.069c-0.398 0-0.788-0.025-1.169-0.074-1.611 1.605-3.471 1.892-5.331 1.935v-0.393c1.004-0.49 1.813-1.382 1.813-2.402 0-0.142-0.011-0.282-0.032-0.419-1.696-1.113-2.781-2.812-2.781-4.717 0-3.352 3.358-6.069 7.5-6.069zM15.563 13.604c0 0.874 0.567 1.639 1.438 2.059v0.337c-1.611-0.036-3.090-0.283-4.487-1.658-0.33 0.041-0.669 0.063-1.013 0.063-1.492 0-2.866-0.402-3.963-1.079 2.261-0.008 4.395-0.732 6.013-2.042 0.816-0.66 1.459-1.435 1.913-2.302 0.481-0.92 0.724-1.9 0.724-2.913 0-0.163-0.007-0.326-0.020-0.487 1.134 0.936 1.832 2.213 1.832 3.62 0 1.633-0.94 3.089-2.41 4.043-0.018 0.117-0.027 0.237-0.027 0.359z\"></path>
    </svg>")

 (defun jme:notmuch-tag-archive-icon ()
   "Return SVG data representing archive icon.
 This can be used with `notmuch-tag-format-image-data'"
   "<?xml version=\"1.0\" encoding=\"utf-8\"?>
    <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
    <svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"16\" height=\"16\" viewBox=\"0 0 16 16\">
      <path fill=\"#c0ff3e\" d=\"M15 4h-4v-1c0-0.55-0.45-1-1-1h-4c-0.55 0-1 0.45-1 1v1h-4c-0.55 0-1 0.45-1 1v9c0 0.55 0.45 1 1 1h14c0.55 0 1-0.45 1-1v-9c0-0.55-0.45-1-1-1zM6 3.002c0.001-0.001 0.001-0.001 0.002-0.002h3.996c0.001 0.001 0.001 0.001 0.002 0.002v0.998h-4v-0.998zM15 8h-2v1.5c0 0.275-0.225 0.5-0.5 0.5h-1c-0.275 0-0.5-0.225-0.5-0.5v-1.5h-6v1.5c0 0.275-0.225 0.5-0.5 0.5h-1c-0.275 0-0.5-0.225-0.5-0.5v-1.5h-2v-1h14v1z\"></path>
    </svg>")

 (defun jme:notmuch-tag-attachment-icon ()
   "Return SVG data representing attachment icon.
 This can be used with `notmuch-tag-format-image-data'"
   "<?xml version=\"1.0\" encoding=\"utf-8\"?>
    <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
    <svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"16\" height=\"16\" viewBox=\"0 0 16 16\">
      <path fill=\"#c0ff3e\" d=\"M10.404 5.11l-1.015-1.014-5.075 5.074c-0.841 0.841-0.841 2.204 0 3.044s2.204 0.841 3.045 0l6.090-6.089c1.402-1.401 1.402-3.673 0-5.074s-3.674-1.402-5.075 0l-6.394 6.393c-0.005 0.005-0.010 0.009-0.014 0.013-1.955 1.955-1.955 5.123 0 7.077s5.123 1.954 7.078 0c0.004-0.004 0.008-0.009 0.013-0.014l0.001 0.001 4.365-4.364-1.015-1.014-4.365 4.363c-0.005 0.004-0.009 0.009-0.013 0.013-1.392 1.392-3.656 1.392-5.048 0s-1.392-3.655 0-5.047c0.005-0.005 0.009-0.009 0.014-0.013l-0.001-0.001 6.395-6.393c0.839-0.84 2.205-0.84 3.045 0s0.839 2.205 0 3.044l-6.090 6.089c-0.28 0.28-0.735 0.28-1.015 0s-0.28-0.735 0-1.014l5.075-5.075z\"></path>
    </svg>")

  (defun jme:notmuch-tag-jira-icon ()
   "Return SVG data representing jira icon.
 This can be used with `notmuch-tag-format-image-data'"
   "<?xml version=\"1.0\" encoding=\"utf-8\"?>
    <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
    <svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"16\" height=\"16\" viewBox=\"0 0 16 16\">
      <path fill=\"#c0ff3e\" d=\"M16 9v-1h-3.020c-0.092-1.136-0.497-2.172-1.12-3.004h2.53l1.095-4.379-0.97-0.243-0.905 3.621h-2.729c-0.014-0.011-0.028-0.021-0.042-0.032 0.105-0.305 0.162-0.632 0.162-0.972 0-1.653-1.343-2.992-3-2.992s-3 1.34-3 2.992c0 0.34 0.057 0.667 0.162 0.972-0.014 0.011-0.028 0.021-0.042 0.032h-2.729l-0.905-3.621-0.97 0.243 1.095 4.379h2.53c-0.623 0.832-1.028 1.868-1.12 3.004h-3.020v1h3.021c0.059 0.713 0.242 1.388 0.526 1.996h-1.937l-1.095 4.379 0.97 0.243 0.905-3.621h1.756c0.917 1.219 2.303 1.996 3.854 1.996s2.937-0.777 3.854-1.996h1.756l0.905 3.621 0.97-0.243-1.095-4.379h-1.937c0.283-0.608 0.466-1.283 0.526-1.996h3.021z\"></path>
    </svg>")

  (defun jme:notmuch-tag-notification-icon ()
   "Return SVG data representing notification icon.
 This can be used with `notmuch-tag-format-image-data'"
   "<?xml version=\"1.0\" encoding=\"utf-8\"?>
    <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
    <svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"16\" height=\"16\" viewBox=\"0 0 16 16\">
      <path fill=\"#c0ff3e\" d=\"M8 1.5c-1.736 0-3.369 0.676-4.596 1.904s-1.904 2.86-1.904 4.596c0 1.736 0.676 3.369 1.904 4.596s2.86 1.904 4.596 1.904c1.736 0 3.369-0.676 4.596-1.904s1.904-2.86 1.904-4.596c0-1.736-0.676-3.369-1.904-4.596s-2.86-1.904-4.596-1.904zM8 0v0c4.418 0 8 3.582 8 8s-3.582 8-8 8c-4.418 0-8-3.582-8-8s3.582-8 8-8zM7 11h2v2h-2zM7 3h2v6h-2z\"></path>
    </svg>")

  (defun jme:notmuch-tag-sent-icon ()
   "Return SVG data representing sent icon.
 This can be used with `notmuch-tag-format-image-data'"
   "<?xml version=\"1.0\" encoding=\"utf-8\"?>
    <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
    <svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"16\" height=\"16\" viewBox=\"0 0 16 16\">
      <path fill=\"#c0ff3e\" d=\"M11 1l-5 5h-3l-3 4c0 0 3.178-0.885 5.032-0.47l-5.032 6.47 6.592-5.127c0.919 2.104-0.592 5.127-0.592 5.127l4-3v-3l5-5 1-5-5 1z\"></path>
    </svg>")

  (defun jme:notmuch-tag-spam-icon ()
   "Return SVG data representing inbox icon.
 This can be used with `notmuch-tag-format-image-data'"
   "<?xml version=\"1.0\" encoding=\"utf-8\"?>
    <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
    <svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"16\" height=\"16\" viewBox=\"0 0 16 16\">
      <path fill=\"#c0ff3e\" d=\"M8 0c-4.418 0-8 3.582-8 8s3.582 8 8 8 8-3.582 8-8-3.582-8-8-8zM4 4h8c0.143 0 0.281 0.031 0.409 0.088l-4.409 5.143-4.409-5.143c0.127-0.058 0.266-0.088 0.409-0.088zM3 11v-6c0-0.021 0.001-0.042 0.002-0.063l2.932 3.421-2.9 2.9c-0.023-0.083-0.034-0.17-0.034-0.258zM12 12h-8c-0.088 0-0.175-0.012-0.258-0.034l2.846-2.846 1.413 1.648 1.413-1.648 2.846 2.846c-0.083 0.023-0.17 0.034-0.258 0.034zM13 11c0 0.088-0.012 0.175-0.034 0.258l-2.9-2.9 2.932-3.421c0.001 0.021 0.002 0.042 0.002 0.063v6z\"></path>
    </svg>")

  (defun jme:notmuch-tag-trash-icon ()
   "Return SVG data representing inbox icon.
 This can be used with `notmuch-tag-format-image-data'"
   "<?xml version=\"1.0\" encoding=\"utf-8\"?>
    <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
    <svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"16\" height=\"16\" viewBox=\"0 0 16 16\">
      <path fill=\"#c0ff3e\" d=\"M3 16h10l1-11h-12zM10 2v-2h-4v2h-5v3l1-1h12l1 1v-3h-5zM9 2h-2v-1h2v1z\"></path>
    </svg>")

  (defun jme:notmuch-tag-to-me-icon ()
   "Return SVG data representing to-me icon.
 This can be used with `notmuch-tag-format-image-data'"
   "<?xml version=\"1.0\" encoding=\"utf-8\"?>
    <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
    <svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"16\" height=\"16\" viewBox=\"0 0 16 16\">
      <path fill=\"#c0ff3e\" d=\"M6.5 1.5c0-0.828 0.672-1.5 1.5-1.5s1.5 0.672 1.5 1.5c0 0.828-0.672 1.5-1.5 1.5s-1.5-0.672-1.5-1.5z\"></path>
      <path fill=\"#c0ff3e\" d=\"M10 5l5.15-2.221-0.371-0.929-6.279 2.15h-1l-6.279-2.15-0.371 0.929 5.15 2.221v4l-2.051 6.634 0.935 0.355 2.902-6.489h0.429l2.902 6.489 0.935-0.355-2.051-6.634z\"></path>
    </svg>")

  (defun jme:notmuch-tag-replied-icon ()
   "Return SVG data representing replied icon.
 This can be used with `notmuch-tag-format-image-data'"
   "<?xml version=\"1.0\" encoding=\"utf-8\"?>
    <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
    <svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"16\" height=\"16\" viewBox=\"0 0 16 16\">
      <path fill=\"#c0ff3e\" d=\"M7 12.119v3.881l-6-6 6-6v3.966c6.98 0.164 6.681-4.747 4.904-7.966 4.386 4.741 3.455 12.337-4.904 12.119z\"></path>
    </svg>")

(defun jme:notmuch-mua-user-agent ()
  "Generate a `User-Agent:' string suitable for notmuch."
  (concat "Emacs/" emacs-version " (Org/" (org-version)")"))

(use-package notmuch
  :commands (notmuch)
  :custom
  (notmuch-search-oldest-first nil)
  (notmuch-show-logo nil)
  (notmuch-archive-tags '("-inbox" "+archive"))
  (notmuch-hello-thousands-separator ",")
  (notmuch-mua-user-agent-function 'jme:notmuch-mua-user-agent)
  (mail-user-agent 'notmuch-user-agent)
  :bind
  (:map notmuch-show-mode-map
        ("d" . (lambda ()
                 "mark message as deleted"
                 (interactive)
                 (notmuch-show-tag '("+deleted" "+trash" "-inbox"))))
        ("S" . (lambda ()
                 "mark message as spam"
                 (interactive)
                 (notmuch-show-tag '("+spam" "-inbox"))))
        :map notmuch-tree-mode-map
        ("d" . (lambda ()
                 "mark message as deleted"
                 (interactive)
                 (notmuch-tree-tag '("+deleted" "+trash" "-inbox"))))
        :map notmuch-search-mode-map
        ("d" . (lambda ()
                 "mark message as deleted"
                 (interactive)
                 (notmuch-search-tag '("+deleted" "+trash" "-inbox"))))
        ("S" . (lambda ()
                 "mark message as spam"
                 (interactive)
                 (notmuch-search-tag '("+spam" "-inbox"))))
        :map notmuch-message-mode-map
        ("C-c M-o" . org-mode)
        :map org-mode-map
        ("C-c M-o" . notmuch-message-mode))
  :init
  (setq notmuch-tag-formats
        '(("unread" (propertize tag (quote face) (quote notmuch-tag-unread)))
          ("flagged" (propertize tag (quote face) (quote notmuch-tag-flagged))
           (notmuch-tag-format-image-data tag (jme:notmuch-tag-flag-icon)))
          ("inbox" (notmuch-tag-format-image-data tag (jme:notmuch-tag-inbox-icon)))
          ("attachment" (notmuch-tag-format-image-data tag (jme:notmuch-tag-attachment-icon)))
          ("archive" (notmuch-tag-format-image-data tag (jme:notmuch-tag-archive-icon)))
          ("jira" (notmuch-tag-format-image-data tag (jme:notmuch-tag-jira-icon)))
          ("notification" (notmuch-tag-format-image-data tag (jme:notmuch-tag-notification-icon)))
          ("sent" (notmuch-tag-format-image-data tag (jme:notmuch-tag-sent-icon)))
          ("spam" (notmuch-tag-format-image-data tag (jme:notmuch-tag-spam-icon)))
          ("trash" (notmuch-tag-format-image-data tag (jme:notmuch-tag-trash-icon)))
          ("to-me" (notmuch-tag-format-image-data tag (jme:notmuch-tag-to-me-icon)))
          ("replied" (notmuch-tag-format-image-data tag (jme:notmuch-tag-replied-icon)))))
  :config
  (require 'org-notmuch))


;; mu4e
;; Since I've loaded mu via brew, there is no local repo for mu4e.
;; Since I am also using straight/use-package, I cannot utilize use-package for mu4e.
;; We fall back to the old, simple require
(require 'mu4e)

(custom-set-variables
 '(shr-color-visible-luminance-min 65)
 '(shr-color-visible-distance-min 5)
 '(mu4e-sent-folder "/Sent")
 '(mu4e-drafts-folder "/Drafts")
 '(mu4e-trash-folder "/Trash")
 '(mu4e-refile-folder "/Archive")
 '(mu4e-use-fancy-chars t)
 '(mu4e-view-show-images t)
 '(mu4e-index-cleanup nil)
 '(mu4e-index-lazy-check t)
 '(mu4e-confirm-quit nil)
 '(mu4e-compose-dont-reply-to-self t)
 '(mu4e-change-filenames-when-moving t)  ; required for mbsync
 '(mu4e-maildir-shortcuts
   '(("/Archive"         . ?A)
     ("/inbox"           . ?i)
     ("/Activity Stream" . ?a)
     ("/Sent"            . ?s)))

 '(mu4e-headers-fields
   '( (:human-date       . 12)
      (:flags            .  6)
      (:from             . 22)
      (:maildir          . 10)
      (:thread-subject   . nil))))

;;; 6542-13mbpr-pkg.el ends here
