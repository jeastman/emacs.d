;;; run-tests.el --- Batch test runner  -*- lexical-binding: t; -*-

;;; Commentary:
;; Batch runner for ERT tests under the tests directory.

;;; Code:

(require 'ert)

(let* ((tests-dir (file-name-directory (or load-file-name buffer-file-name)))
       (test-files (directory-files tests-dir t "-test\\.el\\'")))
  (dolist (file test-files)
    (load-file file)))

(ert-run-tests-batch-and-exit)

;;; run-tests.el ends here.
