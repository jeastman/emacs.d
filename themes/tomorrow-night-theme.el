;;; tomorrow-night-theme.el --- custom theme for faces

;;; Commentary:
;;
;;; Tomorrow Night Theme
;;
;; Originally by Chris Kempson https://github.com/ChrisKempson/Tomorrow-Theme
;; Ported to GNU Emacs by Chris Charles
;; Ported to GNU Emacs 24's built-in theme system by Jim Myhrberg (@jimeh)
;; Additional sections by John Eastman

;;; Code:

(deftheme tomorrow-night
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
      (white "#ffffff"))

  (custom-theme-set-faces
   'tomorrow-night

   ;; Built-in stuff (Emacs 23)
   `(default ((t (:background ,background :foreground ,foreground))))
   `(error ((t (:foreground ,red))))
   `(escape-glyph ((t (:foreground ,aqua))))
   `(fringe ((t (:background ,current-line))))
   `(highlight ((t (:background ,green :foreground ,current-line))))
   `(link ((t (:foreground ,blue))))
   `(link-visited ((t (:foreground ,purple))))
   `(minibuffer-prompt ((t (:foreground ,blue))))
   `(mode-line ((t (:background ,selection :foreground ,foreground))))
   `(mode-line-inactive ((t (:background ,current-line :foreground ,foreground))))
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
   `(org-hide ((t (:foreground ,current-line))))
   `(org-document-title ((,class (:foreground ,aqua :background ,background :weight bold :height 1.5))))
   `(org-document-info ((,class (:foreground ,aqua :background ,background :weight bold))))
   `(org-document-info-keyword ((,class (:foreground ,comment :background ,background))))
   `(org-agenda-date-today
     ((,class (:foreground ,orange :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((,class (:inherit font-lock-comment-face))))
   `(org-archived ((,class (:slant italic))))
   `(org-checkbox ((,class (:background ,comment :foreground ,background
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((,class (:foreground ,purple :underline t))))
   `(org-done ((,class (:bold t :weight bold :foreground ,green
                              :box (:line-width 1 :style none)))))
   `(org-todo ((,class (:bold t :foreground ,red :weight bold
                              :box (:line-width 1 :style none)))))
   `(org-level-1 ((,class (:foreground ,purple :height 1.3))))
   `(org-level-2 ((,class (:foreground ,blue :height 1.2))))
   `(org-level-3 ((,class (:foreground ,aqua :height 1.1))))
   `(org-level-4 ((,class (:foreground ,green))))
   `(org-level-5 ((,class (:foreground ,yellow))))
   `(org-level-6 ((,class (:foreground ,orange))))
   `(org-level-7 ((,class (:foreground ,aqua))))
   `(org-level-8 ((,class (:foreground ,blue))))
   `(org-link ((,class (:foreground ,blue :underline t))))
   `(org-tag ((,class (:bold t :weight bold))))
   `(org-column ((,class (:background ,yellow :foreground ,background))))
   `(org-column-title ((,class (:background ,background :underline t :weight bold))))

   ;; show-paren-mode
   `(show-paren-match ((t (:background ,blue :foreground ,current-line))))
   `(show-paren-mismatch ((t (:background ,orange :foreground ,current-line))))

   ;; whitespace-mode
   `(whitespace-empty ((t (:background ,yellow :foreground ,red))))
   `(whitespace-hspace ((t (:background ,selection :foreground ,comment))))
   `(whitespace-indentation ((t (:background ,yellow :foreground ,red))))
   `(whitespace-line ((t (:background ,current-line :foreground ,purple))))
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
   `(erc-my-nick-face ((,class (:foreground ,red :weigth bold))))
   `(erc-nick-msg-face ((,class (:inherit erc-default))))
   `(erc-notice-face ((,class (:foreground ,green))))
   `(erc-pal-face ((,class (:foreground ,orange :weight bold))))
   `(erc-prompt-face ((,class (:foreground ,orange :background ,background :weight bold))))
   `(erc-timestamp-face ((,class (:foreground ,aqua))))
   `(erc-underline-face ((t (:underline t))))
   
   ;; flymake
   `(flymake-errline ((,class (:foreground ,red-1 :weight bold :underline t))))
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
   'tomorrow-night

   `(ansi-color-names-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [,background ,red ,green ,yellow ,blue ,purple ,blue ,foreground])
   `(ansi-term-color-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [unspecified ,background ,red ,green ,yellow ,blue ,purple ,blue ,foreground])))

(provide-theme 'tomorrow-night)

;;; tomorrow-night-theme.el ends here
