;;; jme-llm.el --- Configuration for working with LLMs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  John Eastman

;; Author: John Eastman <john.eastman@gmail.com>
;; Keywords: convenience, languages

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides configuration of elfeed for reading RSS content.

;;; Code:
(require 'straight)
(require 'jme-common)
(straight-use-package 'ellama)
(declare-function make-llm-ollama "llm-ollama" (&key DEFAULT-CHAT-TEMPERATURE DEFAULT-CHAT-MAX-TOKENS DEFAULT-CHAT-NON-STANDARD-PARAMS SCHEME HOST PORT CHAT-MODEL EMBEDDING-MODEL))

(defun jme-llm--enable()
  "Enable LLM configuration."
  (setopt ellama-keymap-prefix "C-c e")
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama
           :chat-model "llama3.2-16k"
           :embedding-model "nomic-embed-text"))
  (setopt ellama-naming-provider
          (make-llm-ollama
           :chat-model "llama3.2-16k"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("stop" . ("\n")))))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm))

(defun jme-llm--disable()
  "Disable LLM configuration.")

(jme-common-defconfiguration jme-llm "Configuration for working with LLMs.")

(provide 'jme-llm)
;;; jme-llm.el ends here
