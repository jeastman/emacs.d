;;; tomorrow-night-theme.el --- custom theme for faces

;;; Commentary:
;;
;;; Tomorrow Night Theme
;;
;; Originally by Chris Kempson https://github.com/ChrisKempson/Tomorrow-Theme
;; Ported to GNU Emacs by Chris Charles
;; Ported to GNU Emacs 24's built-in theme system by Jim Myhrberg (@jimeh)
;; Additional sections by John Eastman

;; SPEC definition
;; - an alist of the form
;;
;; ((DISPLAY . ATTS)...)
;;
;; Where DISPLAY is a form specifying the conditions to match certain
;; terminals and ATTS is a property list (ATTR VALUE ATTR VALUE...)
;; specifying face attributes and values for frames on those terminals.
;; On each terminal, the first element with a matching DISPLAY specification
;; takes effect, and the remaining elements in SPEC are disregarded.
;; In the ATTS property list, possible attributes are `:family',
;; `:width', `:height', `:weight', `:slant', `:underline',
;; `:overline', `:strike-through', `:box', `:foreground',
;; `:background', `:stipple', `:inverse-video', and `:inherit'.

;;; Code:

(deftheme tomorrow-night-mac
  "A Pastel Coloured Theme")

(let ((class '((class color) (min-colors 89)))
      (background "#1d1f21")
      (current-line "#282a2e")
      (selection "#373b41")
      (foreground "#c5c8c6")
      (comment "#969896")
      (cursor "#aeafad")
      (red "#cc6666")
      (red-1 "#8b0000")
      (orange "#de935f")
      (yellow "#f0c674")
      (green "#b5bd68")
      (aqua "#8abeb7")
      (blue "#81a2be")
      (purple "#b294bb")
      (white "#ffffff")
      (ltsalmon "#ffa07a")
      (ltskyblue "#87cefa")
      (ltgoldenrod "LightGoldenrod")
      (spray0 "#e3e3e3")
      (spray1 "#cadbd8")
      (spray2 "#9db8bd")
      (spray3 "#6f9ca6")
      (spray4 "#58595e")
      (spray5 "#3d3c39")
      (sansfont "Helvetica Neue-16")
      (sansfamily "Helvetica Neue")
      (monofont "monoOne-14")
      (monofamily "monoOne")
      ;(modelinefont "Helvetica Neue-12")
      )

  (custom-theme-set-faces
   'tomorrow-night-mac

   ;; Built-in stuff (Emacs 23)
   `(default ((t (:background ,background :foreground ,foreground :font ,monofont))))
   `(variable-pitch ((t (:font ,sansfont))))
   `(error ((t (:foreground ,red))))
   `(escape-glyph ((t (:foreground ,aqua))))
   `(fringe ((t (:background ,current-line))))
   `(highlight ((t (:background ,green :foreground ,current-line))))
   `(link ((t (:foreground ,blue))))
   `(link-visited ((t (:foreground ,purple))))
   `(minibuffer-prompt ((t (:foreground ,blue))))
;;   `(mode-line ((t (:background ,selection :foreground ,foreground))))
;;   `(mode-line ((t (:background ,comment :foreground ,background :font ,modelinefont))))
;;   `(powerline-active ((t (:background ,spray2 :foreground ,background :font ,modelinefont))))
;;   `(powerline-active1 ((t (:background ,spray2 :foreground ,background :font ,modelinefont))))
;;   `(powerline-active2 ((t (:background ,spray4 :foreground ,foreground :font ,modelinefont))))
;;   `(mode-line-inactive ((t (:background ,current-line :foreground ,foreground))))
;;   `(mode-line-inactive ((t (:background ,comment :foreground ,selection :font ,modelinefont))))
;;   `(powerline-inactive1 ((t (:background ,spray1 :foreground ,selection :font ,modelinefont))))
;;   `(powerline-inactive2 ((t (:background ,spray4 :foreground ,foreground :font ,modelinefont))))
   `(header-line ((t (:background "grey20" :foreground "grey90" :box nil :font ,monofont))))
   `(region ((t (:background ,selection))))
   `(secondary-selection ((t (:background ,blue))))
   `(shadow ((t (:foreground ,comment))))
   `(success ((t (:foreground ,green))))
   `(trailing-whitespace ((t (:background ,red))))
   `(warning ((t (:foreground ,orange))))

   ;; Font-lock stuff
   `(font-lock-builtin-face ((t (:foreground ,aqua))))
   `(font-lock-comment-face ((t (:foreground ,comment))))
   `(font-lock-constant-face ((t (:foreground ,aqua))))
   `(font-lock-doc-string-face ((t (:foreground ,comment))))
   `(font-lock-function-name-face ((t (:foreground ,blue))))
   `(font-lock-keyword-face ((t (:foreground ,purple))))
   `(font-lock-preprocessor-face ((t (:forground ,comment))))
   `(font-lock-string-face ((t (:foreground ,green))))
   `(font-lock-type-face ((t (:foreground ,yellow))))
   `(font-lock-variable-name-face ((t (:foreground ,orange))))
   `(font-lock-warning-face ((t (:foreground ,red))))
   `(font-lock-reference-face ((,class (:foreground ,comment))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,yellow))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,red))))

   ;; hl-line-mode
   `(hl-line ((t (:background ,current-line))))

   ;; linum-mode
   `(linum ((t (:background ,current-line :foreground ,foreground))))

   ;; org-mode
   `(org-default ((t (:font ,sansfont))))
   `(org-hide ((t (:foreground ,current-line :font ,sansfont))))
   `(org-level-1 ((,class (:foreground ,purple :height 1.3 :family ,sansfamily))))
   `(org-level-2 ((,class (:foreground ,blue :height 1.2 :family ,sansfamily))))
   `(org-level-3 ((,class (:foreground ,aqua :height 1.1 :family ,sansfamily))))
   `(org-level-4 ((,class (:foreground ,green :font ,sansfont))))
   `(org-level-5 ((,class (:foreground ,yellow :font ,sansfont))))
   `(org-level-6 ((,class (:foreground ,orange :font ,sansfont))))
   `(org-level-7 ((,class (:foreground ,aqua :font ,sansfont))))
   `(org-level-8 ((,class (:foreground ,blue :font ,sansfont))))
   `(org-special-keyword ((,class (:foreground ,ltsalmon :font ,sansfont))))
   `(org-drawer ((,class (:background ,background :foreground ,ltskyblue :font ,sansfont))))
   `(org-column ((,class (:background ,yellow :foreground ,background :font ,sansfont))))
   `(org-column-title ((,class (:background ,background :underline t :weight bold :font ,sansfont))))
   `(org-warning ((,class (:foreground ,red-1 :weight bold :font ,sansfont))))
   `(org-archived ((,class (:slant italic :font ,sansfont))))
   `(org-link ((,class (:foreground ,blue :underline t :font ,sansfont))))
   `(org-footnote ((,class (:foreground ,purple :underline t :font ,sansfont))))
   `(org-ellipsis ((,class (:foreground ,ltgoldenrod :underline t :font ,sansfont))))
   `(org-target ((,class (:underline t :font ,sansfont))))
   `(org-date ((,class (:foreground ,purple :underline t :font ,sansfont))))
   `(org-date-selected ((,class (:foreground ,red-1 :inverse-video t :font ,sansfont))))
   `(org-sexp-date ((,class (:foreground ,purple :underline t :font ,sansfont))))
   `(org-tag ((,class (:bold t :weight bold :font ,sansfont))))
   `(org-list-dt ((,class (:bold t :weight bold :font ,sansfont))))
   `(org-todo ((,class (:bold t :foreground ,red :weight bold :font ,monofont
                              :box (:line-width 1 :style none)))))
   `(org-done ((,class (:bold t :weight bold :foreground ,green :font ,monofont
                              :box (:line-width 1 :style none)))))
   `(org-agenda-done ((,class (:foreground ,green :font ,sansfont
                              :box (:line-width 1 :style none)))))
   `(org-headline-done ((,class (:foreground ,ltsalmon :font ,sansfont))))
   `(org-priority ((,class (:foreground ,ltsalmon :slant italic :font ,sansfont))))
   `(org-checkbox ((,class (:background ,comment :foreground ,background :font ,monofont
                                   :box (:line-width 1 :style released-button)))))
   `(org-table ((,class (:foreground ,ltskyblue :font ,monofont))))
   `(org-formula ((,class (:foreground ,red :bold t :weight bold :slant italic :font ,monofont))))
   `(org-code ((,class (:font ,monofont))))
   `(org-meta-line ((,class (:font ,monofont))))
   `(org-document-title ((,class (:foreground ,aqua :background ,background :weight bold :height 1.5 :family ,sansfamily))))
   `(org-document-info ((,class (:foreground ,aqua :background ,background :weight bold :font ,sansfont))))
   `(org-document-info-keyword ((,class (:foreground ,comment :background ,background :font ,monofont))))
   `(org-block ((,class (:font ,monofont))))
   `(org-block-background ((,class (:font ,monofont))))
   `(org-verbatim ((,class (:font ,monofont))))
   `(org-clock-overlay ((,class (:font ,monofont))))
   `(org-agenda-structure
     ((,class (:inherit font-lock-comment-face :font ,monofont))))
   `(org-agenda-date-today
     ((,class (:foreground ,orange :slant italic :weight bold :font ,monofont))) t)
   `(org-scheduled ((,class (:font ,monofont))))
   `(org-scheduled-today ((,class (:font ,monofont))))
   `(org-agenda-dimmed-todo-face ((,class (:font ,monofont))))
   `(org-scheduled-previously ((,class (:font ,monofont))))
   `(org-upcoming-deadline ((,class (:font ,monofont))))
   `(org-agenda-restriction-lock ((,class (:font ,monofont))))
   `(org-time-grid ((,class (:font ,monofont))))

   ;; show-paren-mode
   `(show-paren-match ((t (:background ,blue :foreground ,current-line))))
   `(show-paren-mismatch ((t (:background ,orange :foreground ,current-line))))

   ;; whitespace-mode
   `(whitespace-empty ((t (:background ,yellow :foreground ,red))))
   `(whitespace-hspace ((t (:background ,selection :foreground ,comment))))
   `(whitespace-indentation ((t (:background ,yellow :foreground ,red))))
;   `(whitespace-line ((t (:background ,current-line :foreground ,purple))))
   `(whitespace-newline ((t (:foreground ,comment))))
   `(whitespace-space ((t (:background ,current-line :foreground ,comment))))
   `(whitespace-space-after-tab ((t (:background ,yellow :foreground ,red))))
   `(whitespace-space-before-tab ((t (:background ,orange :foreground ,red))))
   `(whitespace-tab ((t (:background ,selection :foreground ,comment))))
   `(whitespace-trailing ((t (:background ,red :foreground ,yellow))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,purple))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,blue))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,aqua))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,green))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,yellow))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,orange))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,red))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,comment))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,foreground))))

   ;; auctex
   `(font-latex-bold-face ((t (:foreground ,green))))
   `(font-latex-doctex-documentation-face ((t (:background ,current-line))))
   `(font-latex-italic-face ((t (:foreground ,green))))
   `(font-latex-math-face ((t (:foreground ,orange))))
   `(font-latex-sectioning-0-face ((t (:foreground ,yellow))))
   `(font-latex-sectioning-1-face ((t (:foreground ,yellow))))
   `(font-latex-sectioning-2-face ((t (:foreground ,yellow))))
   `(font-latex-sectioning-3-face ((t (:foreground ,yellow))))
   `(font-latex-sectioning-4-face ((t (:foreground ,yellow))))
   `(font-latex-sectioning-5-face ((t (:foreground ,yellow))))
   `(font-latex-sedate-face ((t (:foreground ,aqua))))
   `(font-latex-string-face ((t (:foreground ,yellow))))
   `(font-latex-verbatim-face ((t (:foreground ,orange))))
   `(font-latex-warning-face ((t (:foreground ,red))))

   ;; auto-complete
   `(ac-completion-face ((,class (:foreground ,comment :underline t))))
   `(ac-candidate-face ((,class (:background ,cursor :foreground ,background))))
   `(ac-selection-face ((,class (:background ,purple :foreground ,background))))
   `(popup-tip-face ((,class (:background ,green :foreground ,red-1))))
   `(popup-scroll-bar-foreground-face ((,class (:background ,foreground))))
   `(popup-scroll-bar-background-face ((,class (:background ,comment))))
   `(popup-isearch-match ((,class (:background ,background :foreground ,aqua))))

   ;; diff
   `(diff-added ((,class (:foreground ,green))))
   `(diff-changed ((,class (:foreground ,yellow))))
   `(diff-removed ((,class (:foreground ,red))))
   `(diff-header ((,class (:background ,current-line))))
   `(diff-file-header ((,class (:background ,current-line :foreground ,foreground :bold t))))

   ;; erc
   `(erc-action-face ((,class (:inherit erc-default-face))))
   `(erc-bold-face ((,class (:weight bold))))
   `(erc-current-nick-face ((,class (:foreground ,blue :weight bold))))
   `(erc-dangerous-host-face ((,class (:inherit font-lock-warning))))
   `(erc-default-face ((,class (:foreground ,foreground))))
   `(erc-direct-msg-face ((,class (:inherit erc-default))))
   `(erc-error-face ((,class (:inherit font-lock-warning))))
   `(erc-fool-face ((,class (:inherit erc-default))))
   `(erc-highlight-face ((,class (:inherit hover-highlight))))
   `(erc-input-face ((,class (:foreground ,yellow))))
   `(erc-keyword-face ((,class (:foreground ,blue :weight bold))))
   `(erc-nick-default-face ((,class (:foreground ,yellow :weight bold))))
   `(erc-my-nick-face ((,class (:foreground ,red :weight bold))))
   `(erc-nick-msg-face ((,class (:inherit erc-default))))
   `(erc-notice-face ((,class (:foreground ,green))))
   `(erc-pal-face ((,class (:foreground ,orange :weight bold))))
   `(erc-prompt-face ((,class (:foreground ,orange :background ,background :weight bold))))
   `(erc-timestamp-face ((,class (:foreground ,aqua))))
   `(erc-underline-face ((t (:underline t))))

   ;; flymake
   `(flymake-errline ((,class (:foreground ,white :background ,red :weight bold :underline t))))
   `(flymake-warnline ((,class (:foreground ,yellow :weight bold :underline t))))

   ;; flyspell
   `(flyspell-duplicate ((,class (:foreground ,yellow :weight bold :underline t))))
   `(flyspell-incorrect ((,class (:foreground ,orange :weight bold :underline t))))

   ;; helm
   `(helm-header
     ((,class (:foreground ,green
                           :background ,background
                           :underline nil
                           :box nil))))
   `(helm-source-header
     ((,class (:foreground ,yellow
                           :background ,background
                           :underline nil
                           :weight bold
                           :box (:line-width -1 :style released-button)))))
   `(helm-selection ((,class (:background ,selection :underline nil))))
   `(helm-selection-line ((,class (:background ,selection))))
   `(helm-visible-mark ((,class (:foreground ,background :background ,yellow))))
   `(helm-candidate-number ((,class (:foreground ,green :background ,selection))))

   ;; jabber-mode
   `(jabber-roster-user-away ((,class (:foreground ,green))))
   `(jabber-roster-user-online ((,class (:foreground ,aqua))))
   `(jabber-roster-user-dnd ((,class (:foreground ,red-1))))
   `(jabber-rare-time-face ((,class (:foreground ,green))))
   `(jabber-chat-prompt-local ((,class (:foreground ,aqua))))
   `(jabber-chat-prompt-foreign ((,class (:foreground ,red))))
   `(jabber-activity-face((,class (:foreground ,red))))
   `(jabber-activity-personal-face ((,class (:foreground ,blue))))
   `(jabber-title-small ((,class (:height 1.1 :weight bold))))
   `(jabber-title-medium ((,class (:height 1.2 :weight bold))))
   `(jabber-title-large ((,class (:height 1.3 :weight bold))))

   ;; magit
   `(magit-section-title ((,class (:foreground ,purple))))
   `(magit-branch ((,class (:foreground ,yellow))))
   `(magit-item-highlight ((,class (:foreground ,foreground :background ,selection))))
   `(magit-diff-add ((,class (:foreground ,green))))
   `(magit-diff-del ((,class (:foreground ,red))))
   `(magit-diff-hunk-header ((,class (:foreground ,orange))))
   )

  (custom-theme-set-variables
   'tomorrow-night-mac

   `(ansi-color-names-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [,background ,red ,green ,yellow ,blue ,purple ,blue ,foreground])
   `(ansi-term-color-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [unspecified ,background ,red ,green ,yellow ,blue ,purple ,blue ,foreground])))

(provide-theme 'tomorrow-night-mac)

;;; tomorrow-night-theme.el ends here
