;;; johnos-prompt.el --- #JOHNOS Prompt Runner
;;; Commentary:
;;; Code for working with #JOHNOS.
;;;
;;; Code:
(require 'straight)
(straight-use-package 'yaml)

(require 'seq)
(require 'org)
(require 'yaml)
(declare-function org-capture-get "org-capture" (PROPERTY &optional LOCAL))
(declare-function org-element-map "org-element" (DATA TYPES FUN &optional INFO FIRST-MATCH NO-RECURSION WITH-AFFILIATED NO-UNDEFER))
(declare-function org-element-parse-buffer "org-element" (&optional GRANULARITY VISIBLE-ONLY KEEP-DEFERRED))
(declare-function org-element-property "org-element-ast" (PROPERTY NODE &optional DFLT FORCE-UNDEFER))

(defvar johnos-ritual-blueprint-path "~/Documents/johnos/workflows/rituals.yaml"
  "Path to the #JOHNOS ritual automation blueprint YAML file.")
(defvar johnos-prompt-directory "~/Documents/johnos/prompts/"
  "Directory where prompt YAML files are stored.")
(defvar johnos-blueprint-format "
* %s

** Description
%s

** Intended Outcome
%s

** Prompts
- %s
"
  "Format for blueprint entry.")
(defvar johnos-ritual-format "* %s
Entered on %s

** Description
%s

** Intended Outcome
%s

%s
"
  "Format for ritual entry.")

(defun johnos-prompt-run (prompt-id)
  "Run a #JOHNOS prompt by PROMPT-ID.  Opens the YAML file if it exists."
  (interactive "sPrompt ID (e.g., prompt-ddd-aggregate.v1): ")
  (let* ((base-dir (expand-file-name johnos-prompt-directory))
         (prompt-path (expand-file-name (concat prompt-id ".yaml") base-dir)))
    (if (file-exists-p prompt-path)
        (find-file prompt-path)
      (message "⚠️ Prompt not found: %s" prompt-path))))

(defun johnos-prompt-menu ()
  "Interactive menu to browse and open prompt by category."
  (interactive)
  (let* ((categories '(("System Design" . "system-design")
                       ("Architecture" . "architecture")
                       ("AI Dev Ops" . "ai-dev-ops")
                       ("Leadership Coaching" . "leadership-coaching")
                       ("Writing & Logs" . "writing")))
         (category (completing-read "Choose category: " (mapcar 'car categories)))
         (dir (concat (cdr (assoc category categories)) "/")))
    (if dir
        (johnos-select-prompt (expand-file-name dir (expand-file-name johnos-prompt-directory)))
        ;; (find-file (expand-file-name dir (expand-file-name johnos-prompt-directory)))
      (message "Invalid category selection."))))

(defun johnos--read-yaml (file)
  "Read a YAML FILE and return it as a Lisp object."
  (with-temp-buffer
    (insert-file-contents file)
    (yaml-parse-string (buffer-string) :object-type 'alist)))

(defun johnos-prompt-fill-and-copy (prompt-id)
  "Fill a #JOHNOS prompt by PROMPT-ID and copy it to the clipboard."
  (interactive "sPrompt ID (e.g., prompt-ddd-aggregate.v1): ")
  (let* ((base-dir (expand-file-name johnos-prompt-directory))
         (file-path (expand-file-name (concat prompt-id ".yaml") base-dir)))
    (if (not (file-exists-p file-path))
        (message "⚠️ Prompt not found: %s" prompt-id)
      (let* ((prompt-data (johnos--read-yaml file-path))
             (template (alist-get 'prompt prompt-data))
             (title (alist-get 'title prompt-data))
             (inputs (alist-get 'inputs prompt-data))
             (filled-prompt template))
        (seq-do (lambda (input)
          (let* ((input-name (if (stringp input) input (symbol-name (car input))))
                 (user-value (read-string (format "Enter value for %s: " input-name))))
            (setq filled-prompt
                  (replace-regexp-in-string
                   (format "\\[%s\\]" input-name)
                   user-value
                   filled-prompt
                   t t))))
                inputs)
        (kill-new filled-prompt)
        (org-capture nil "Aj") ;; assumes a custom capture template is defined
        (with-current-buffer (org-capture-get :buffer)
          (save-excursion
            (goto-char (point-max))
            (insert (format "
* %s

%s
" title filled-prompt))))
        (message "✅ Prompt filled and copied to clipboard.")))))

(defun johnos-ritual-launch (agent)
  "Launch a ritual for the specified AGENT using `org-capture'."
  (interactive
   (list (completing-read "Choose agent: " '("Agent.Monday"
                                             "Agent.Midweek"
                                             "Agent.Friday"
                                             "Agent.Orbit"
                                             "Agent.Kernel"))))
  (let ((template-key
         (cond
          ((string= agent "Agent.Monday") "Am")
          ((string= agent "Agent.Midweek") "Aw")
          ((string= agent "Agent.Friday") "Af")
          ((string= agent "Agent.Orbit") "Ao")
          ((string= agent "Agent.Kernel") "Ak")
          (t (error "Unknown agent: %s" agent)))))
    (org-capture nil template-key)))

(defun johnos--load-ritual-blueprint ()
  "Load and parse the #JOHNOS ritual automation blueprint."
  (when (file-exists-p johnos-ritual-blueprint-path)
    (with-temp-buffer
      (insert-file-contents johnos-ritual-blueprint-path)
      (yaml-parse-string (buffer-string) :object-type 'alist))))

(defun johnos-ritual-fill-from-blueprint (agent-id)
  "Fill the `org-capture' buffer from the ritual blueprint using AGENT-ID."
  (interactive "SAgent ID (e.g., Agent.Friday): ")
  (let* ((blueprint (johnos--load-ritual-blueprint))
         (workflows (alist-get 'workflows blueprint))
         (ritual (assoc agent-id workflows)))
    (if (not ritual)
        (message "⚠️ No ritual automation found for agent: %s" agent-id)
      (let* ((name (alist-get 'name (cdr ritual)))
             (description (alist-get 'description (cdr ritual)))
             (intended_outcome (alist-get 'intended_outcome (cdr ritual)))
             (prompts (alist-get 'prompt_ids (cdr ritual))))
        (org-capture nil "Ab") ;; Assumes a generic ritual template "Ab"
        (with-current-buffer (org-capture-get :buffer)
          (save-excursion
            (goto-char (point-max))
            (insert (format johnos-blueprint-format
                            name description intended_outcome
                            (mapconcat #'identity prompts "\n- ")))))
        (message "✅ Ritual template for %s loaded from blueprint." agent-id)))))

(defun johnos--load-prompt-content (prompt-id)
  "Load prompt (by PROMPT-ID) YAML and return its content as a string."
  (let* ((path (expand-file-name (concat prompt-id ".yaml") johnos-prompt-directory)))
    (if (not (file-exists-p path))
        (format "⚠️ Missing prompt file: %s" prompt-id)
      (with-temp-buffer
        (insert-file-contents path)
        (let ((prompt-data (yaml-parse-string (buffer-string) :object-type 'alist)))
          (alist-get 'prompt prompt-data))))))

(defun johnos-ritual-execute (agent-id)
  "Execute a full ritual session from the blueprint for AGENT-ID."
  (interactive "sAgent ID (e.g., Agent.Monday): ")
  (let* ((blueprint (johnos--load-ritual-blueprint))
         (workflows (alist-get 'workflows blueprint))
         (ritual (assoc agent-id workflows)))
    (if (not ritual)
        (message "⚠️ No ritual automation found for agent: %s" agent-id)
      (let* ((name (alist-get 'name (cdr ritual)))
             (description (alist-get 'description (cdr ritual)))
             (intended_outcome (alist-get 'intended_outcome (cdr ritual)))
             (prompts (alist-get 'prompt_ids (cdr ritual)))
             (prompt-outputs
              (mapcar (lambda (pid)
                        (format "** Prompt: %s
%s
" pid (johnos--load-prompt-content pid)))
                      prompts)))
        (org-capture nil "Ab") ;; assumes a ritual automation template is assigned to key "Ab"
        (with-current-buffer (org-capture-get :buffer)
          (save-excursion
            (goto-char (point-max))
            (insert (format johnos-ritual-format
                            name (format-time-string "[%Y-%m-%d %a %H:%M]")
                            description intended_outcome
                            (string-join prompt-outputs "
")))))
        (message "✅ Full ritual entry composed for %s" agent-id)))))

(defun johnos-export-prompt-to-yaml ()
  "Export the current org subtree as a YAML prompt file for #JOHNOS."
  (interactive)
  (org-narrow-to-subtree)
  (let* ((id (org-entry-get (point) "ID"))
         (title (org-entry-get (point) "TITLE"))
         (file-name (org-entry-get (point) "EXPORT_FILE_NAME"))
         (description (org-entry-get (point) "DESCRIPTION"))
         (content (org-element-map (org-element-parse-buffer) 'src-block
                    (lambda (src)
                      (when (string= (org-element-property :language src) "text")
                        (org-element-property :value src)))
                    nil t))
         (inputs nil)
         (tags nil))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\*\* Inputs" nil t)
        (forward-line)
        (while (looking-at "^\s-*[-+]\s-*\(.*\)")
          (push (match-string 1) inputs)
          (forward-line)))
      (goto-char (point-min))
      (when (re-search-forward "^\*\* Tags" nil t)
        (forward-line)
        (while (looking-at "^\s-*[-+]\s-*\(.*\)")
          (push (match-string 1) tags)
          (forward-line))))
    (widen)
    (let ((yaml `((id . ,id)
                  (title . ,title)
                  (description . ,description)
                  (prompt . ,content)
                  (inputs . ,(nreverse inputs))
                  (tags . ,(nreverse tags)))))
      (with-temp-file (expand-file-name file-name johnos-prompt-directory)
        (insert (yaml-encode yaml)))
      (message "✅ Exported prompt to %s/%s" johnos-prompt-directory file-name))))

(defun johnos--find-prompt(directory)
  "Prompt the user to find a prompt file from DIRECTORY."
  (let* ((prompt-file (read-file-name "Select prompt: " directory nil t nil
                         (lambda (f)
                           (or
                            (file-directory-p f)
                            (and (file-regular-p f)
                                 (string-match-p "\\.yaml\\'" f)))))))
     (when prompt-file
       prompt-file)))

(defun johnos-select-prompt (directory)
  "Interactively select a file in DIRECTORY."
  (interactive (list (read-directory-name "Select directory: " johnos-prompt-directory)))
  (let ((prompt-file (johnos--find-prompt directory)))
    (if (file-exists-p prompt-file)
        (find-file prompt-file)
      (message "⚠️ Prompt not found: %s" prompt-file))))


(provide 'johnos-prompt)
;;; johnos-prompt.el ends here
