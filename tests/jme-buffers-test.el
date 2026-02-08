;;; jme-buffers-test.el --- Tests for jme-buffers  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for buffer configuration module.

;;; Code:

(require 'cl-lib)
(require 'ert)

(let ((modules-dir (expand-file-name "modules" (file-name-directory (directory-file-name
                                                                    (file-name-directory
                                                                     (or load-file-name buffer-file-name)))))))
  (add-to-list 'load-path modules-dir))

(unless (featurep 'straight)
  (defun straight-use-package (&rest _args)
    "No-op stub for tests."
    nil)
  (provide 'straight))

(unless (featurep 'all-the-icons-ibuffer)
  (defun all-the-icons-ibuffer-mode (&optional _arg)
    "No-op stub for tests."
    nil)
  (provide 'all-the-icons-ibuffer))

(require 'jme-buffers)

(ert-deftest jme-buffers-test-ibuffer-latex-regex ()
  "Ensure the LaTeX group matches literal .tex files."
  (jme-buffers--configure-ibuffer)
  (let* ((main (assoc "Main" ibuffer-saved-filter-groups))
         (latex (assoc "LaTeX" (cdr main))))
    (should (equal (cadr latex) '(name . "\\.tex$")))))

(ert-deftest jme-buffers-test-ibuffer-hook-removal ()
  "Ensure ibuffer hooks are added and removed."
  (let ((original (default-value 'ibuffer-mode-hook)))
    (unwind-protect
        (progn
          (setq-default ibuffer-mode-hook nil)
          (jme-buffers--enable)
          (should (memq #'jme-buffers--ibuffer-apply-groups
                        (default-value 'ibuffer-mode-hook)))
          (should (memq #'all-the-icons-ibuffer-mode
                        (default-value 'ibuffer-mode-hook))))
      (jme-buffers--disable)
      (setq-default ibuffer-mode-hook original))
    (should-not (memq #'jme-buffers--ibuffer-apply-groups
                      (default-value 'ibuffer-mode-hook)))
    (should-not (memq #'all-the-icons-ibuffer-mode
                      (default-value 'ibuffer-mode-hook)))))

(ert-deftest jme-buffers-test-c-z-restore ()
  "Ensure the \\`suspend-frame\\' binding is restored on disable."
  (let ((original (lookup-key (current-global-map) (kbd "C-z"))))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _frame) t)))
      (unwind-protect
          (progn
            (jme-buffers--enable)
            (should (eq (lookup-key (current-global-map) (kbd "C-z")) #'bury-buffer))
            (jme-buffers--disable)
            (should (equal (lookup-key (current-global-map) (kbd "C-z")) original)))
        (define-key (current-global-map) (kbd "C-z") original)
        (setq jme-buffers--did-bind-c-z nil)
        (setq jme-buffers--saved-c-z-binding nil)))))

(provide 'jme-buffers-test)
;;; jme-buffers-test.el ends here
