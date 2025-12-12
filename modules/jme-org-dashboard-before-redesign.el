;;; jme-org-dashboard.el --- Generate a modern HTML dashboard from an Org-mode file. -*- lexical-binding: t; -*-

;; Author: John Eastman
;; Created: 25 June 2025

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
;; This package provides tools to manage a structured set of web links within an
;; Org-mode file and export them into a modern, visually polished, stand-alone
;; HTML dashboard.

;;; Code:

(require 'org)
(require 'custom)
(require 'cl-lib)

;;----------------------------------------------------------------------
;;; Customization Variables
;;----------------------------------------------------------------------

(defgroup jme-org-dashboard nil
  "Customization group for the Org Mode Dashboard Generator."
  :group 'org)

(defcustom jme-org-dashboard-output-file (expand-file-name "~/Documents/dashboard.html")
  "The full path where the generated HTML dashboard will be saved."
  :type 'string
  :group 'jme-org-dashboard)

(defcustom jme-org-dashboard-org-file (expand-file-name "~/Documents/dashboard.org")
  "The full path to the source Org file for the dashboard.
This is used by the `org-capture' template to find the target file."
  :type 'file
  :group 'jme-org-dashboard)

(defcustom jme-org-dashboard-default-theme 'dark
  "The default theme for the dashboard.  Can be \\='light or \\='dark."
  :type '(choice (const :tag "Light" light)
                 (const :tag "Dark" dark))
  :group 'jme-org-dashboard)

(defcustom jme-org-dashboard-collapsible-sections t
  "If non-nil, categories in the dashboard will be collapsible."
  :type 'boolean
  :group 'jme-org-dashboard)

(defcustom jme-org-dashboard-sort-order 'manual
  "The sort order for categories and links within them.
\\='manual preserves the order from the Org file.
\\='alphabetical sorts them alphabetically."
  :type '(choice (const :tag "Manual" manual)
                 (const :tag "Alphabetical" alphabetical))
  :group 'jme-org-dashboard)

(defcustom jme-org-dashboard-icon-service-url "https://www.google.com/s2/favicons?sz=64&domain_url=%s"
  "URL template for fetching favicons.  %s will be replaced with the link's domain.
Set to nil to disable automatic favicons."
  :type '(or null string)
  :group 'jme-org-dashboard)

;;----------------------------------------------------------------------
;;; Parsing Engine (Buffer Navigation)
;;----------------------------------------------------------------------

(defun jme-org-dashboard--parse-properties-in-region (start end)
  "Parse a property drawer between START and END of a region."
  (let ((props nil))
    (save-excursion
      (goto-char start)
      (when (re-search-forward "^[ \t]*:PROPERTIES:[ \t]*$" end t)
        (while (and (< (point) end)
                    (not (looking-at "^[ \t]*:END:"))
                    (zerop (forward-line 1)))
          (when (looking-at "^[ \t]*:\\([A-Z_]+\\):[ \t]+\\(.*\\)")
            (let ((key (intern (concat ":" (match-string 1))))
                  (val (match-string 2)))
              (setq props (plist-put props key val)))))))
    props))

(defun jme-org-dashboard--parse-links-in-region (start end)
  "Find all list items containing links within a region from START to END."
  (let ((links nil))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "^[ \t]*- \\(\\[\\[.*\\]\\]\\)" end t)
        (let* ((link-match (match-string 1))
               (link-data nil)
               (props nil))

          (when (string-match "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" link-match)
            (setq link-data (list :url (match-string 1 link-match)
                                  :label (match-string 2 link-match))))

          (when link-data
              (save-excursion
                (goto-char (line-end-position))
                (setq props (jme-org-dashboard--parse-properties-in-region (point) end)))

            ;; Now we combine the parsed link data with any properties found.
            (let ((url (plist-get link-data :url))
                  (label (plist-get link-data :label))
                  (icon (plist-get props :ICON))
                  (note (plist-get props :NOTE))
                  (tags (let ((tags-str (plist-get props :TAGS)))
                           (if tags-str (split-string tags-str "," t " "))))
                  )
              (setq links (push (list :url url :label label :icon icon :note note :tags tags) links)))))))
    (nreverse links)))

;;----------------------------------------------------------------------
;;; HTML Generation
;;----------------------------------------------------------------------

(defun jme-org-dashboard--generate-html (data)
  "Generate the full HTML document from parsed DATA."
  (concat
   "<!DOCTYPE html>\n"
   "<html lang=\"en\">\n"
   (jme-org-dashboard--generate-html-head data)
   (jme-org-dashboard--generate-html-body data)
   "</html>"))

(defun jme-org-dashboard--generate-html-head (data)
  "Generate the <head> section from DATA with a major visual overhaul."
  (concat
   "<head>\n"
   "    <meta charset=\"utf-8\">\n"
   "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
   (format "    <title>%s</title>\n" (plist-get data :title))
   "    <!-- Materialize CSS -->\n"
   "    <link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/css/materialize.min.css\">\n"
   "    <!-- Google Fonts: Roboto and the modern Material Symbols (recommended) -->\n"
   "    <link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">\n"
   "    <link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>\n"
   "    <link href=\"https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700&display=swap\" rel=\"stylesheet\">\n"
   "    <link href=\"https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined\" rel=\"stylesheet\" />\n"
   "    <style>\n"
   "      :root { --accent-color: #03a9f4; }\n"
   "      body.light-theme {\n"
   "        --bg-color: #f8f9fa; --text-color: #212529; --card-bg: #ffffff;\n"
   "        --header-bg: #ffffff; --header-text: #212529; --header-border: #dee2e6;\n"
   "        --search-bg: #f1f3f5; --secondary-text: #6c757d;\n"
   "        --card-shadow: 0 2px 4px rgba(0,0,0,0.05); --card-hover-shadow: 0 5px 15px rgba(0,0,0,0.1);\n"
   "      }\n"
   "      body.dark-theme {\n"
   "        --bg-color: #121212; --text-color: #e0e0e0; --card-bg: #1e1e1e;\n"
   "        --header-bg: #1e1e1e; --header-text: #e0e0e0; --header-border: #333;\n"
   "        --search-bg: #303030; --secondary-text: #9e9e9e;\n"
   "        --card-shadow: 0 2px 4px rgba(0,0,0,0.2); --card-hover-shadow: 0 5px 15px rgba(0,0,0,0.3);\n"
   "      }\n"
   "      body { font-family: 'Roboto', sans-serif; background-color: var(--bg-color); color: var(--text-color); transition: background-color 0.3s; }\n"
   "      nav { background-color: var(--header-bg) !important; color: var(--header-text) !important; border-bottom: 1px solid var(--header-border); }\n"
   "      nav .brand-logo, nav ul a { color: var(--header-text) !important; }\n"
   "      .nav-wrapper .input-field input[type=search] { background-color: var(--search-bg); color: var(--header-text); border: none !important; box-shadow: none !important; border-radius: 8px; padding: 0 15px 0 45px !important; height: 38px !important; margin: 13px 0; }\n"
   "      .nav-wrapper .input-field .label-icon { top: 13px; left: 10px; height: 38px; line-height: 38px; }\n"
   "      main { padding: 2rem 0; }\n"
   "      .collapsible { border: none !important; box-shadow: none !important; }\n"
   "      .collapsible-header {\n"
   "        font-size: 1.6rem; font-weight: 500; color: var(--text-color); background-color: transparent !important; border-bottom: none !important; display: flex; align-items: center; padding-left: 0 !important;\n"
   "      }\n"
   "      .collapsible-header .material-symbols-outlined { font-size: 2rem; margin-right: 0.75rem; color: var(--accent-color); }\n"
   "      .collapsible-body { background-color: transparent !important; border-bottom: none !important; padding: 1.5rem 0 !important; }\n"
   "      .link-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(280px, 1fr)); gap: 1.5rem; }\n"
   "      .link-card {\n"
   "        display: flex; flex-direction: column; background-color: var(--card-bg); padding: 16px; border-radius: 12px;\n"
   "        text-decoration: none; color: var(--text-color); box-shadow: var(--card-shadow); transition: transform 0.2s ease-out, box-shadow 0.2s ease-out;\n"
   "        border: 1px solid transparent;\n"
   "      }\n"
   "      .link-card:hover { transform: translateY(-4px); box-shadow: var(--card-hover-shadow); border-color: var(--accent-color); }\n"
   "      .link-card-header { display: flex; align-items: center; margin-bottom: 12px; }\n"
   "      .link-card-header .icon { width: 32px; height: 32px; margin-right: 12px; flex-shrink: 0; display: flex; align-items: center; justify-content: center; }\n"
   "      .link-card-header .icon .material-symbols-outlined { font-size: 28px; color: var(--accent-color); }\n"
   "      .link-card-header .icon .favicon { width: 100%; height: 100%; object-fit: contain; border-radius: 6px; }\n"
   "      .link-card-header .icon .letter-icon { width: 100%; height: 100%; border-radius: 6px; background-color: var(--accent-color); color: white; display: flex; align-items: center; justify-content: center; font-weight: bold; font-size: 18px; text-transform: uppercase; }\n"
   "      .link-label { font-size: 1.1rem; font-weight: 500; line-height: 1.3; }\n"
   "      .link-note { font-size: 0.9rem; color: var(--secondary-text); margin-top: auto; padding-top: 8px; }\n"
   "      .material-tooltip { z-index: 2000 !important; }\n"
   "      .controls { margin-bottom: 2rem; display: flex; justify-content: flex-end; gap: 1rem; }\n"
   "    </style>\n"
   "</head>"))

(defun jme-org-dashboard--generate-html-body (data)
  "Generate the <body> from DATA section with an improved layout and controls."
  (format
   "<body class=\"%s-theme\">
    <header>
      <nav class=\"z-depth-0\">
        <div class=\"nav-wrapper container\">
          <a href=\"#\" class=\"brand-logo\">%s</a>
          <ul class=\"right\">
            <li style=\"width: 250px;\">
              <div class=\"input-field\">
                <input id=\"search\" type=\"search\" autocomplete=\"off\">
                <label class=\"label-icon\" for=\"search\"><i class=\"material-symbols-outlined\">search</i></label>
              </div>
            </li>
            <li><a href=\"#\" id=\"theme-toggle\" class=\"tooltipped\" data-position=\"bottom\" data-tooltip=\"Toggle Theme\"><i class=\"material-symbols-outlined\">brightness_6</i></a></li>
          </ul>
        </div>
      </nav>
    </header>

    <main class=\"container\">
      <div class=\"controls\">
        <a href=\"#\" id=\"expand-all\" class=\"waves-effect waves-light btn-small\"><i class=\"material-symbols-outlined left\">unfold_more</i>Expand All</a>
        <a href=\"#\" id=\"collapse-all\" class=\"waves-effect waves-light btn-small\"><i class=\"material-symbols-outlined left\">unfold_less</i>Collapse All</a>
      </div>
      %s
    </main>

    <footer class=\"page-footer transparent\">
        <div class=\"footer-copyright grey-text text-darken-1\">
            <div class=\"container center-align\">
                Generated from Org-mode with jme-org-dashboard. %s
            </div>
        </div>
    </footer>

    <!-- JavaScript -->
    <script src=\"https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/js/materialize.min.js\"></script>
    <script>%s</script>
</body>"
   (if (eq (plist-get data :theme) 'dark) "dark" "light")
   (plist-get data :title)
   (jme-org-dashboard--generate-categories-html (plist-get data :categories))
   (if-let (author (plist-get data :author)) (format "Authored by %s." author) "")
   (jme-org-dashboard--generate-javascript)))

(defun jme-org-dashboard--generate-categories-html (categories)
  "Generate HTML for all CATEGORIES, now with better icons and starting expanded."
  (if jme-org-dashboard-collapsible-sections
      (format "<ul class=\"collapsible expandable z-depth-0\">%s</ul>"
              (mapconcat (lambda (cat)
                           (format "
        <li class=\"active\" data-category-title=\"%s\">
          <div class=\"collapsible-header\"><i class=\"material-symbols-outlined\">%s</i>%s</div>
          <div class=\"collapsible-body\"><div class=\"link-grid\">%s</div></div>
        </li>"
                                   (plist-get cat :title)
                                   (if (string-match-p "🛠️" (plist-get cat :title)) "construction" "folder")
                                   (plist-get cat :title)
                                   (mapconcat #'jme-org-dashboard--generate-link-html (plist-get cat :links) "\n")))
                         categories "\n"))
    ;; Non-collapsible version
    (mapconcat (lambda (cat)
                 (format "
      <section data-category-title=\"%s\">
        <h2 class=\"category-title\">%s</h2>
        <div class=\"link-grid\">%s</div>
      </section>"
                         (plist-get cat :title)
                         (plist-get cat :title)
                         (mapconcat #'jme-org-dashboard--generate-link-html (plist-get cat :links) "\n")))
               categories "\n")))

(defun jme-org-dashboard--generate-link-html (link)
  "Generate HTML for a single, redesigned LINK card."
  (let* ((label (plist-get link :label))
         (url (plist-get link :url))
         (icon (plist-get link :icon))
         (note (plist-get link :note))
         (domain (replace-regexp-in-string "^https?://\\([^/]+\\)/?.*" "\\1" url)))
    (format "<a href=\"%s\" target=\"_blank\" class=\"link-card tooltipped\" data-position=\"bottom\" data-tooltip=\"%s\" data-label=\"%s\">
    <div class=\"link-card-header\">
        <div class=\"icon\">
        %s
        </div>
        <span class=\"link-label\">%s</span>
    </div>
    %s
</a>"
            url
            url
            label
            (cond
             (icon (format "<i class=\"material-symbols-outlined\">%s</i>" icon))
             (jme-org-dashboard-icon-service-url
              (format "<img src=\"%s\" class=\"favicon\" alt=\"\" onerror=\"this.style.display='none'; this.nextElementSibling.style.display='flex';\">
<span class=\"letter-icon\" style=\"display: none;\">%s</span>"
                      (format jme-org-dashboard-icon-service-url domain)
                      (upcase (substring label 0 1))))
             (t (format "<span class=\"letter-icon\">%s</span>" (upcase (substring label 0 1)))))
            label
            (if note (format "<p class=\"link-note\">%s</p>" note) ""))))

(defun jme-org-dashboard--generate-javascript ()
  "Generate the inline JavaScript for enhanced interactivity."
  "
document.addEventListener('DOMContentLoaded', function() {
    const collapsibleElems = document.querySelectorAll('.collapsible');
    const collapsibleInstances = M.Collapsible.init(collapsibleElems, { accordion: false });

    const tooltipElems = document.querySelectorAll('.tooltipped');
    M.Tooltip.init(tooltipElems);

    // Theme Toggling
    const themeToggle = document.getElementById('theme-toggle');
    const body = document.body;
    const currentTheme = localStorage.getItem('dashboard-theme') || (body.classList.contains('dark-theme') ? 'dark' : 'light');

    function applyTheme(theme) {
        body.classList.remove('light-theme', 'dark-theme');
        body.classList.add(theme + '-theme');
        localStorage.setItem('dashboard-theme', theme);
    }
    applyTheme(currentTheme);

    themeToggle.addEventListener('click', (e) => {
        e.preventDefault();
        const newTheme = body.classList.contains('dark-theme') ? 'light' : 'dark';
        applyTheme(newTheme);
    });

    // Expand/Collapse Controls
    const expandBtn = document.getElementById('expand-all');
    const collapseBtn = document.getElementById('collapse-all');
    expandBtn.addEventListener('click', e => {
        e.preventDefault();
        collapsibleInstances.forEach(inst => inst.open());
    });
    collapseBtn.addEventListener('click', e => {
        e.preventDefault();
        collapsibleInstances.forEach(inst => inst.close());
    });

    // Search Filtering
    const searchInput = document.getElementById('search');
    const linkCards = Array.from(document.querySelectorAll('.link-card'));
    const categories = Array.from(document.querySelectorAll('[data-category-title]'));

    searchInput.addEventListener('input', (e) => {
        const query = e.target.value.toLowerCase().trim();

        linkCards.forEach(card => {
            const label = card.dataset.label.toLowerCase();
            const noteEl = card.querySelector('.link-note');
            const note = noteEl ? noteEl.textContent.toLowerCase() : '';
            const shouldShow = label.includes(query) || note.includes(query);
            card.style.display = shouldShow ? 'flex' : 'none';
        });

        categories.forEach(category => {
            const visibleLinks = category.querySelectorAll('.link-card[style*=\"display: flex\"], .link-card:not([style*=\"display: none\"])');
            category.style.display = visibleLinks.length > 0 ? '' : 'none';
        });
    });
});
")


;;----------------------------------------------------------------------
;;; Org Capture Integration
;;----------------------------------------------------------------------

(defun jme-org-dashboard--get-category-titles ()
  "Parse `jme-org-dashboard-org-file` and return a list of category titles."
  (unless (file-exists-p jme-org-dashboard-org-file)
    (error "Dashboard Org file not found at: %s" jme-org-dashboard-org-file))
  (with-temp-buffer
    (insert-file-contents jme-org-dashboard-org-file)
    (let (titles)
      (goto-char (point-min))
      (while (re-search-forward "^\\* \\(.*\\)" nil t)
        (push (match-string 1) titles))
      (nreverse titles))))

(defun jme-org-dashboard--prompt-for-category ()
  "Prompt the user to select a category from the existing dashboard file."
  (let ((categories (jme-org-dashboard--get-category-titles)))
    (unless categories
      (error "No level 1 headlines (categories) found in %s" jme-org-dashboard-org-file))
    (completing-read "Category: " categories nil t)))

(defun jme-org-dashboard--build-capture-template (url description icon tags note)
    "Build text for the capture template with URL, DESCRIPTION, ICON, TAGS, NOTE.
Handles optional ICON and TAGS properties gracefully."
  (let ((props '()))
    (when (and icon (not (string-empty-p icon)))
      (push (format ":ICON: %s" icon) props))
    (when (and tags (not (string-empty-p tags)))
      (push (format ":TAGS: %s" tags) props))
    (when (and note (not (string-empty-p note)))
      (push (format ":NOTE: %s" note) props))

    (if (null props)
        (format "- [[%s][%s]]" url description)
      (format "- [[%s][%s]]\n  :PROPERTIES:\n%s\n  :END:"
              url
              description
              (mapconcat (lambda (p) (format "  %s" p))
                         (nreverse props)
                         "\n")))))

(defcustom jme-org-dashboard-capture-template
  '(("b"
     "Bookmark to Dashboard"
     item
     (file+headline (lambda () jme-org-dashboard-org-file)
                    (lambda () (jme-org-dashboard--prompt-for-category)))
     "%(jme-org-dashboard--build-capture-template \"%c\" \"%^{Description}\" \"%^{Icon (opt)}\" \"%^{Tags (opt)}\" \"%^{Note (opt)}\")"
     :empty-lines-before 1
     :empty-lines-after 1))
  "The `org-capture' template definition for adding dashboard links."
  :type 'sexp
  :group 'jme-org-dashboard)

;;----------------------------------------------------------------------
;;; Main Interactive Command
;;----------------------------------------------------------------------

;;;###autoload
(defun jme-org-dashboard-export-to-html ()
  "Generate a personal HTML dashboard from the current Org-mode buffer.
This version uses a robust buffer-navigation parser."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (error "This command must be run in an Org-mode buffer"))

  (let ((source-buffer-content (buffer-string))
        (dashboard-data nil)
        (html-content "")
        (title "My Dashboard")
        (theme jme-org-dashboard-default-theme)
        (author nil)
        (categories nil))

    (with-temp-buffer
      (insert source-buffer-content)

      ;; 1. Parse Document Properties
      (goto-char (point-min))
      (while (re-search-forward "^#\\+\\([^:]+\\):[ \t]*\\(.*\\)" nil t)
        (let ((key (downcase (match-string 1)))
              (val (string-trim (match-string 2))))
          (cond
           ((string= key "title") (setq title val))
           ((string= key "author") (setq author val))
           ((string= key "dashboard-theme") (setq theme (read-from-string val))))))

      ;; 2. Parse Categories and Links
      (goto-char (point-min))
      (while (re-search-forward "^\\* \\(.*\\)" nil t)
        (let* ((category-title (match-string 1))
               (section-start (point))
               (section-end (save-excursion
                              (if (re-search-forward "^\\* " nil t)
                                  (match-beginning 0)
                                (point-max))))
               (found-links (jme-org-dashboard--parse-links-in-region section-start section-end)))
          (when found-links
            (push (list :title category-title :links found-links) categories)))))

    ;; 3. Assemble the final data structure
    (setq dashboard-data
          (list :title title
                :author author
                :theme theme
                :categories (nreverse categories)))

    ;; 4. Generate and save the HTML
    (setq html-content (jme-org-dashboard--generate-html dashboard-data))
    (with-temp-file jme-org-dashboard-output-file
      (insert html-content))
    (message "Dashboard exported successfully to %s" jme-org-dashboard-output-file)
    (when (y-or-n-p "Open the exported dashboard in a browser? ")
      (browse-url-of-file (expand-file-name jme-org-dashboard-output-file)))))

(provide 'jme-org-dashboard)
;;; jme-org-dashboard.el ends here
