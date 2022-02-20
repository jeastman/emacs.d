;;; early-init.el --- Emacs configuration file -*- lexical-binding: t; -*-

;; Author: John Eastman
;; Created: 12 Feb 2022

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this file; see the file COPYING.  If not, see see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Emacs 27 inroduced the ability to load configuration ar startup
;; earlier than when the normal init file is processed.
;;
;; This file is loaded before the package system and GUI is initialized,
;; so it is intended to customize variables that affect frame appearance as
;; well as the package initialization process.

;;; Code:

;; Inspired by the findings of the Doom Emacs team,
;; garbage collection is modified during startup
;; with the following values.
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

;; Ensure we set garbage collection back to reasonable
;; values once startup is finished.
;; See https://akrl.sdf.org/ for details on Garbage Collector Magic Hack
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216 ; 16mb
          gc-cons-percentage 0.1)
    ;; Garbage Collector Magic Hack
    ;; GCMH managed with use-pacakge in init.el.
    (gcmh-mode 1)))

;; prevent package.el loading packages prior to init-file loading
;; since using straight.el
(setq package-enable-at-startup nil)

;; Cause `load' to prefer the newest version of a file.
;; Time could be cut down by specifying `noninteractive' instead,
;; but the mtime checks for *.elc/native files is tolerable
(setq load-prefer-newer t)

;; Native compliation config
(when (featurep 'native-compile)
  ;; Set compilation to max optimization level.
  ;; 2 is max optimization level fully adherent to the language semantic.
  (setq native-comp-speed 2)
  ;; Compiler verbosity.
  ;; Intended for debugging compiler itself.
  ;; 0 is no logging.
  (setq native-comp-verbose 0)
  ;; Default number of subprocesses used for async native compilation.
  ;; 0 means to use half the number of CPU's execution units.
  (setq native-comp-async-jobs-number 0)
  ;; Compile loaded .elc files asynchronously
  (setq native-comp-deferred-compilation t)
  ;; Whether to report warnings and errors from async native compliation.
  ;; `silent' logs warnings, but does not pop up buffer
  (setq native-comp-async-report-warnings-errors 'silent)
  ;; Whether to query the user about killing async compile on exit.
  (setq native-comp-async-query-on-exit nil)
  ;; Emit a warning if byte-code file being loaded has no source file.
  (setq native-comp-warning-on-missing-source t))

;;; Pre-configure frame items so that UI does not flash

;; Disable the toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; Do not show scroll bars
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; Make the fringe slightly wider
(if (fboundp 'fringe-mode) (fringe-mode 16))
;; Don't blink the cursor
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0))

;; Do not show the startup message
(setq inhibit-startup-message t)
;; Do not show the startup screen
(setq inhibit-startup-screen t)
;; Do not show buffer menu when more than 2 files are loaded
(setq inhibit-startup-buffer-menu t)
;; Inhibit initial message
(setq inhibit-startup-echo-area-message user-login-name)


;; Default Frame
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(height . 48) default-frame-alist)
(push '(width . 80) default-frame-alist)
(push '(left-fringe . 16) default-frame-alist)
(push '(right-fringe . 16) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push `(alpha . (95 . 95)) default-frame-alist)

;; Initial Frame
(push '(height . 48) initial-frame-alist)
(push '(width . 160) initial-frame-alist)
