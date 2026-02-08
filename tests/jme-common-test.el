;;; jme-common-test.el --- Tests for jme-common  -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for jme-common utilities.

;;; Code:

(require 'ert)

(let ((module-file (expand-file-name "../modules/jme-common.el"
                                     (file-name-directory (or load-file-name
                                                             buffer-file-name)))))
  (load-file module-file))

(defcustom jme-test-custom-nil nil
  "Custom var with nil default for tests."
  :group 'emacs)

(jme-common-defconfiguration jme-test-config
  "Test configuration for jme-common docstring coverage.")

(ert-deftest jme-common-default-value-for-symbol-no-default ()
  (let ((sym (make-symbol "jme-test--no-default")))
    (should (eq (jme-common-default-value-for-symbol sym)
                jme-common--no-default))))

(ert-deftest jme-common-default-value-for-symbol-nil-default ()
  (should (eq (jme-common-default-value-for-symbol 'jme-test-custom-nil) nil))
  (should (not (eq (jme-common-default-value-for-symbol 'jme-test-custom-nil)
                   jme-common--no-default))))

(ert-deftest jme-common-revert-symbols-restores-nil ()
  (setq jme-test-custom-nil t)
  (jme-common-revert-symbols '(jme-test-custom-nil))
  (should (eq jme-test-custom-nil nil)))

(ert-deftest jme-common-remove-from-list-returns-updated-list ()
  (let ((items (list 'a 'b 'c)))
    (setq items (jme-common-remove-from-list items 'b))
    (should (equal items '(a c)))))

(ert-deftest jme-common-defconfiguration-docstring-uses-getter-form ()
  (let ((doc (documentation 'jme-test-config)))
    (should (string-match-p "Toggle configuration jme-test-config" doc))
    (should (string-match-p
             "default-value[^)]*jme-test-config"
             doc))))

(provide 'jme-common-test)
;;; jme-common-test.el ends here.
