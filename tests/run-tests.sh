#!/usr/bin/env sh
set -eu

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"

exec emacs -Q --batch -l "${ROOT_DIR}/tests/run-tests.el"
