;;; jme-shell-test.el --- Tests for jme-shell  -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for shell configuration module.

;;; Code:

(require 'cl-lib)
(require 'ert)

(let ((modules-dir (expand-file-name "modules" (file-name-directory (directory-file-name
                                                                    (file-name-directory
                                                                     (or load-file-name buffer-file-name)))))))
  (add-to-list 'load-path modules-dir))

;; `jme-shell' requires straight at load time.  Provide a minimal stub in tests.
(unless (featurep 'straight)
  (defun straight-use-package (&rest _args)
    "Test stub for straight package installation."
    nil)
  (provide 'straight))

(require 'jme-shell)

(ert-deftest jme-shell-test-prompt-regexp-matches-user-and-root-prompts ()
  (should (string-match-p jme-shell--prompt-regexp " ✘> "))
  (should (string-match-p jme-shell--prompt-regexp " # ")))

(ert-deftest jme-shell-test-prompt-function-root-prompt-matches-regexp ()
  (cl-letf (((symbol-function 'eshell/pwd) (lambda (&rest _args) "/tmp"))
            ((symbol-function 'jme-shell--current-vc-status) (lambda () ""))
            ((symbol-function 'jme-shell--time-diff) (lambda (&rest _args) ""))
            ((symbol-function 'user-uid) (lambda () 0)))
    (setq eshell-last-command-status 0)
    (let* ((prompt (jme-shell--prompt-function))
           (prompt-line (car (last (split-string prompt "\n")))))
      (should (string-suffix-p "# " prompt-line))
      (should (string-match-p jme-shell--prompt-regexp prompt-line)))))

(ert-deftest jme-shell-test-parse-git-branch-name ()
  (should (equal (jme-shell--parse-git-branch-name
                  '("# branch.oid abcdef"
                    "# branch.head feature/test"))
                 "feature/test")))

(ert-deftest jme-shell-test-vc-icons-includes-ahead-behind-and-change-icons ()
  (let ((icons (jme-shell--vc-icons
                '("# branch.ab +2 -1"
                  "1 .M N... 100644 100644 100644 abcdef abcdef path"
                  "2 RM N... 100644 100644 100644 100644 abcdef abcdef R100 new\told"
                  "? untracked.txt"))))
    (should (member "⇡" icons))
    (should (member "⇣" icons))
    (should (member "✎" icons))
    (should (member "»" icons))
    (should (member "?" icons))))

(ert-deftest jme-shell-test-disable-is-safe-and-idempotent-without-eshell-loaded ()
  (let ((had-output (boundp 'eshell-output-filter-functions))
        (had-map (boundp 'eshell-mode-map))
        (old-output (and (boundp 'eshell-output-filter-functions)
                         eshell-output-filter-functions))
        (old-map (and (boundp 'eshell-mode-map)
                      eshell-mode-map)))
    (unwind-protect
        (progn
          (when had-output
            (makunbound 'eshell-output-filter-functions))
          (when had-map
            (makunbound 'eshell-mode-map))
          (cl-letf (((symbol-function 'eshell-syntax-highlighting-global-mode)
                     (lambda (&optional _arg) nil)))
            (should (condition-case nil
                        (progn (jme-shell--disable) t)
                      (error nil)))
            (should (condition-case nil
                        (progn (jme-shell--disable) t)
                      (error nil)))))
      (if had-output
          (setq eshell-output-filter-functions old-output)
        (ignore-errors (makunbound 'eshell-output-filter-functions)))
      (if had-map
          (setq eshell-mode-map old-map)
        (ignore-errors (makunbound 'eshell-mode-map))))))

(ert-deftest jme-shell-test-disable-removes-eshell-settings-when-bound ()
  (let ((had-output (boundp 'eshell-output-filter-functions))
        (had-map (boundp 'eshell-mode-map))
        (old-output (and (boundp 'eshell-output-filter-functions)
                         eshell-output-filter-functions))
        (old-map (and (boundp 'eshell-mode-map)
                      eshell-mode-map)))
    (unwind-protect
        (progn
          (setq eshell-output-filter-functions '(eshell-truncate-buffer other-filter)
                eshell-mode-map (make-sparse-keymap))
          (define-key eshell-mode-map (kbd "C-d") #'ignore)
          (cl-letf (((symbol-function 'eshell-syntax-highlighting-global-mode)
                     (lambda (&optional _arg) nil)))
            (jme-shell--disable))
          (should-not (memq 'eshell-truncate-buffer eshell-output-filter-functions))
          (should-not (lookup-key eshell-mode-map (kbd "C-d"))))
      (if had-output
          (setq eshell-output-filter-functions old-output)
        (ignore-errors (makunbound 'eshell-output-filter-functions)))
      (if had-map
          (setq eshell-mode-map old-map)
        (ignore-errors (makunbound 'eshell-mode-map))))))

(provide 'jme-shell-test)
;;; jme-shell-test.el ends here.
