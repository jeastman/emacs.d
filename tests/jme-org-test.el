;;; jme-org-test.el --- Tests for jme-org -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for org configuration module.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org)

(let ((modules-dir (expand-file-name "modules" (file-name-directory (directory-file-name
                                                                     (file-name-directory
                                                                      (or load-file-name buffer-file-name)))))))
  (add-to-list 'load-path modules-dir))

(unless (featurep 'straight)
  (defun straight-use-package (&rest _args)
    "No-op stub for tests."
    nil)
  (provide 'straight))

(unless (featurep 'org-modern-indent)
  (define-minor-mode org-modern-indent-mode
    "No-op stub for tests.")
  (provide 'org-modern-indent))

(require 'jme-org)

(defun jme-org-test--restore-global-key (key definition)
  "Restore global KEY to DEFINITION."
  (if definition
      (global-set-key (kbd key) definition)
    (global-unset-key (kbd key))))

(ert-deftest jme-org-test-configure-capture-keys-are-unique ()
  "Ensure capture templates do not reuse keys."
  (let ((org-directory "/tmp")
        (org-capture-templates nil))
    (jme-org--configure-capture)
    (let* ((keys (mapcar #'car org-capture-templates))
           (table (make-hash-table :test #'equal)))
      (dolist (key keys)
        (puthash key (1+ (gethash key table 0)) table))
      (maphash (lambda (key count)
                 (should (= count 1))
                 (when (equal key "E")
                   (should (= count 1))))
               table))))

(ert-deftest jme-org-test-archive-done-tasks-matches-subheadings ()
  "Ensure archiving scans all heading levels."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Parent\n")
    (insert "** DONE Child\n")
    (insert "CLOSED: [2020-01-01 Wed 00:00]\n\n")
    (let ((org-done-keywords '("DONE"))
          (jme-org-archive-expiry-days 0)
          (archived 0))
      (cl-letf (((symbol-function 'save-buffer) #'ignore)
                ((symbol-function 'org-archive-subtree)
                 (lambda (&optional _find-done)
                   (setq archived (1+ archived))
                   (let ((beg (point))
                         (end (save-excursion (org-end-of-subtree t t))))
                     (delete-region beg end)))))
        (jme-org-archive-done-tasks))
      (should (= archived 1)))))

(ert-deftest jme-org-test-enable-disable-preserves-safe-locals-and-babel-args ()
  "Ensure enable/disable mutate state safely."
  (let ((orig-safe (default-value 'safe-local-variable-values))
        (orig-hook (default-value 'org-mode-hook))
        (orig-ct (lookup-key org-mode-map (kbd "C-c t")))
        (orig-c-l (lookup-key (current-global-map) (kbd "C-c l")))
        (orig-c-L (lookup-key (current-global-map) (kbd "C-c L")))
        (orig-c-c (lookup-key (current-global-map) (kbd "C-c c")))
        (orig-c-u-i (lookup-key (current-global-map) (kbd "C-c u i")))
        babel-symbol
        babel-value)
    (unwind-protect
        (progn
          (setq-default safe-local-variable-values '((foo . bar)))
          (cl-letf (((symbol-function 'jme-org--configure-latex) #'ignore)
                    ((symbol-function 'jme-org--configure-capture) #'ignore)
                    ((symbol-function 'org-clock-persistence-insinuate) #'ignore)
                    ((symbol-function 'org-babel-do-load-languages)
                     (lambda (sym value)
                       (setq babel-symbol sym)
                       (setq babel-value value)
                       nil)))
            (jme-org--enable))
          (should (eq babel-symbol 'org-babel-load-languages))
          (should (equal babel-value org-babel-load-languages))
          (should (member '(foo . bar) (default-value 'safe-local-variable-values)))
          (should (member jme-org--safe-local-after-save-entry
                          (default-value 'safe-local-variable-values)))
          (should (advice-member-p #'jme-org--org-capture-make-full-window-frame-advice
                                   #'org-capture))
          (should (advice-member-p #'jme-org--org-capture-delete-capture-frame-advice
                                   #'org-capture-finalize))
          (jme-org--disable)
          (should (member '(foo . bar) (default-value 'safe-local-variable-values)))
          (should-not (member jme-org--safe-local-after-save-entry
                              (default-value 'safe-local-variable-values)))
          (should-not (advice-member-p #'jme-org--org-capture-make-full-window-frame-advice
                                       #'org-capture))
          (should-not (advice-member-p #'jme-org--org-capture-delete-capture-frame-advice
                                       #'org-capture-finalize)))
      (setq-default safe-local-variable-values orig-safe)
      (setq-default org-mode-hook orig-hook)
      (define-key org-mode-map (kbd "C-c t") orig-ct)
      (jme-org-test--restore-global-key "C-c l" orig-c-l)
      (jme-org-test--restore-global-key "C-c L" orig-c-L)
      (jme-org-test--restore-global-key "C-c c" orig-c-c)
      (jme-org-test--restore-global-key "C-c u i" orig-c-u-i))))

(provide 'jme-org-test)
;;; jme-org-test.el ends here
