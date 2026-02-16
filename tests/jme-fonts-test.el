;;; jme-fonts-test.el --- Tests for jme-fonts  -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for font configuration module.

;;; Code:

(require 'cl-lib)
(require 'ert)

(let ((modules-dir (expand-file-name "modules" (file-name-directory (directory-file-name
                                                                    (file-name-directory
                                                                     (or load-file-name buffer-file-name)))))))
  (add-to-list 'load-path modules-dir))

(require 'jme-fonts)

(ert-deftest jme-fonts-test-set-font-config-unknown-config-errors ()
  (let ((window-system t))
    (should-error (jme-fonts-set-font-config 'does-not-exist) :type 'user-error)))

(ert-deftest jme-fonts-test-set-font-config-updates-state-on-success ()
  (let ((window-system t)
        (jme-fonts--font-config-hist nil)
        (jme-fonts--current-config nil)
        (jme-fonts--saved-face-attributes nil))
    (cl-letf (((symbol-function 'jme-fonts--snapshot-managed-faces) (lambda () t))
              ((symbol-function 'jme-fonts--apply-face-attribute) (lambda (&rest _args) t)))
      (should (jme-fonts-set-font-config 'alternate))
      (should (eq jme-fonts--current-config 'alternate))
      (should (equal (car jme-fonts--font-config-hist) "alternate")))))

(ert-deftest jme-fonts-test-set-font-config-does-not-update-on-failure ()
  (let ((window-system t)
        (jme-fonts--font-config-hist nil)
        (jme-fonts--current-config nil)
        (jme-fonts--saved-face-attributes nil)
        (calls 0))
    (cl-letf (((symbol-function 'jme-fonts--snapshot-managed-faces) (lambda () t))
              ((symbol-function 'jme-fonts--apply-face-attribute)
               (lambda (&rest _args)
                 (setq calls (1+ calls))
                 (< calls 3))))
      (should-not (jme-fonts-set-font-config 'default))
      (should-not jme-fonts--current-config)
      (should-not jme-fonts--font-config-hist))))

(ert-deftest jme-fonts-test-disable-restores-saved-face-attributes ()
  (let ((jme-fonts--saved-face-attributes
         '((default :family "A" :weight normal :height 100)
           (fixed-pitch :family "B" :weight medium :height 110)))
        (jme-fonts--current-config 'default)
        (restored nil))
    (cl-letf (((symbol-function 'set-face-attribute)
               (lambda (face _frame &rest attributes)
                 (push (cons face attributes) restored))))
      (jme-fonts--disable)
      (should-not jme-fonts--saved-face-attributes)
      (should-not jme-fonts--current-config)
      (should (= (length restored) 2)))))

(ert-deftest jme-fonts-test-prompt-default-prefers-current-config ()
  (let ((jme-fonts--current-config 'alternate)
        (jme-fonts--font-config-hist '("default")))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection _predicate _require-match _initial-input _hist def)
                 (should (equal def "alternate"))
                 (should (member "default" collection))
                 def)))
      (should (equal (jme-fonts--set-fonts-prompt) "alternate")))))

(ert-deftest jme-fonts-test-unload-function-is-defined ()
  (should (fboundp 'jme-fonts-unload-function)))

(provide 'jme-fonts-test)
;;; jme-fonts-test.el ends here.
