# Tests

This directory contains ERT tests for the Emacs configuration.

**Run all tests**

```sh
emacs -Q --batch -l tests/run-tests.el
```

**Run a single test file**

```sh
emacs -Q --batch -l tests/run-tests.el \
  -l tests/jme-common-test.el \
  -f ert-run-tests-batch-and-exit
```
