;; jme-org-dashboard.el --- HTML Dashboard -*- lexical-binding: t; -*-

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
;; This package provides the functionality to transform a specially structured
;; Org-mode file into a beautiful, interactive HTML dashboard.

;;; Code:
(require 'org-element)
(require 'cl-lib)
(require 'custom)

(defgroup jme-org-dashboard nil
  "Customization group for the Org Mode Dashboard Generator."
  :group 'org)

(defcustom jme-org-dashboard-org-file (expand-file-name "~/Documents/dashboard.org")
  "The full path to the source Org file for the dashboard.
This is used by the `org-capture' template to find the target file."
  :type 'file
  :group 'jme-org-dashboard)

(defcustom jme-org-dashboard-output-file (expand-file-name "~/Documents/dashboard.html")
  "The full path where the generated HTML dashboard will be saved."
  :type 'string
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

(defcustom jme-org-dashboard-capture-template
  '(("b" ; The trigger key
     "Bookmark to Dashboard" ; Description
     item ; Type of entry (a list item)
     ;; Target: The file and headline to capture to. We use functions
     ;; to dynamically determine these at capture time.
     (file+headline (lambda () jme-org-dashboard-org-file)
                    (lambda () (jme-org-dashboard--prompt-for-category)))
     ;; Template Body: This is where the magic happens. We call our
     ;; helper function to build the final text.
     "%(jme-org-dashboard--build-capture-template \"%c\" \"%^{Description}\" \"%^{Icon (optional)}\" \"%^{Tags (optional, comma-separated)}\")"
     ;; Properties for the capture process
     :empty-lines-before 1
     :empty-lines-after 1))
  "The `org-capture' template definition for adding dashboard links.
See documentation on how to add this to your `org-capture-templates`."
  :type 'sexp
  :group 'jme-org-dashboard)

(defun jme-org-dashboard--parse-org-data ()
  "Parse the current Org buffer and return a plist of dashboard data."
  (let* ((org-tree (org-element-parse-buffer))
         (doc-props (jme-org-dashboard--get-document-properties org-tree))
         (categories (jme-org-dashboard--get-categories-and-links org-tree)))
    (list :title (or (plist-get doc-props :title) "Dashboard")
          :description (plist-get doc-props :description)
          :author (plist-get doc-props :author)
          :theme (or (plist-get doc-props :dashboard-theme) jme-org-dashboard-default-theme)
          :categories categories)))

(defun jme-org-dashboard--get-document-properties (org-tree)
  "Extract #+KEY: VALUE properties from the parsed ORG-TREE.
This version is robust against malformed keywords."
  (let (props)
    (org-element-map org-tree 'keyword
      (lambda (kw)
        ;; Guard against malformed keywords that might lack a :key.
        (when-let ((key-str (plist-get kw :key)))
          (let ((key (intern (concat ":" (downcase key-str))))
                (val (plist-get kw :value)))
            (setq props (plist-put props key val))))))
    props))

(defun jme-org-dashboard--get-categories-and-links (org-tree)
  "Extract categories (level 1 headlines) and their links from ORG-TREE.
This version iterates over top-level elements for robustness."
  (let (categories)
    ;; Iterate only over the direct children of the root document element.
    (dolist (element (org-element-contents org-tree))
      ;; We only care about elements that are headlines.
      (when (eq (org-element-type element) 'headline)
        ;; Check if it's a top-level (level 1) headline.
        ;; `org-element-property` is the canonical way to get properties.
        (when (= (org-element-property :level element) 1)
          (let* ((title (org-element-property :raw-value element))
                 (links (jme-org-dashboard--extract-links-from-headline element)))
            (when links
              (push (list :title title :links links) categories))))))

    (let ((sorted-categories (nreverse categories)))
      (if (eq jme-org-dashboard-sort-order 'alphabetical)
          (sort sorted-categories (lambda (a b) (string< (plist-get a :title) (plist-get b :title))))
        sorted-categories))))

(defun jme-org-dashboard--get-item-properties (item-element)
  "Extract the property plist from a property drawer within an ITEM-ELEMENT.
Returns a plist of properties, or nil if no property drawer is found."
  (let (properties)
    ;; A property drawer is a direct child of the item element.
    ;; We search for it within the item's contents.
    (org-element-map (org-element-contents item-element) 'property-drawer
      (lambda (drawer)
        ;; The properties are stored as the first (and only)
        ;; element in the drawer's contents. It's a plist.
        (setq properties (car (org-element-contents drawer)))))
    properties))

(defun jme-org-dashboard--extract-links-from-headline (headline)
  "Extract links from list items within a HEADLINE's section."
  (let (links)
    (org-element-map (org-element-contents headline) 'item
      (lambda (item)
        ;; An item might not contain a link, so we search within it.
        ;; This handles cases where a list item might just be text.
        (org-element-map item 'link
          (lambda (link)
            (let* ((props (jme-org-dashboard--get-item-properties item))
                   (url (org-element-property :raw-link link))
                   (label (or (car (org-element-contents link)) url))
                   (icon (plist-get props :ICON))
                   (note (plist-get props :NOTE))
                   (tags (when-let (tags-str (plist-get props :TAGS))
                           (split-string tags-str "," t " "))))
              (push (list :url url :label label :icon icon :note note :tags tags) links))))))
    (let ((sorted-links (nreverse links)))
       (if (eq jme-org-dashboard-sort-order 'alphabetical)
           (sort sorted-links (lambda (a b) (string< (plist-get a :label) (plist-get b :label))))
         sorted-links))))

;; HTML Generation functions

(defun jme-org-dashboard--generate-html (data)
  "Generate the full HTML document from parsed DATA."
  (concat
   "<!DOCTYPE html>\n"
   "<html lang=\"en\">\n"
   (jme-org-dashboard--generate-html-head data)
   (jme-org-dashboard--generate-html-body data)
   "</html>"))

(defun jme-org-dashboard--generate-html-head (data)
  "Generate the <head> section of the HTML from DATA.
This version is robust against format-string errors in static CSS."
  (concat
   "<head>\n"
   "    <meta charset=\"utf-8\">\n" ; <-- Typo also fixed here (was UTF-t-8)
   "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
   ;; Only format the line that actually needs a dynamic value.
   (format "    <title>%s</title>\n" (plist-get data :title))
   "    <!-- Materialize CSS and Icons -->\n"
   "    <link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/css/materialize.min.css\">\n"
   "    <link href=\"https://fonts.googleapis.com/icon?family=Material+Icons\" rel=\"stylesheet\">\n"
   "    <style>\n"
   "      :root {\n"
   "        --primary-color: #2196F3;\n"
   "        --primary-color-light: #64b5f6;\n"
   "        --primary-color-dark: #1976d2;\n"
   "      }\n"
   "      body { transition: background-color 0.3s, color 0.3s; }\n"
   "      body.light-theme {\n"
   "        --bg-color: #f4f4f4; --text-color: #333; --card-bg: #fff;\n"
   "        --header-bg: var(--primary-color); --header-text: #fff;\n"
   "        --search-bg: rgba(255,255,255,0.3);\n"
   "      }\n"
   "      body.dark-theme {\n"
   "        --bg-color: #121212; --text-color: #e0e0e0; --card-bg: #1e1e1e;\n"
   "        --header-bg: #212121; --header-text: #e0e0e0;\n"
   "        --search-bg: rgba(255,255,255,0.1);\n"
   "      }\n"
   "      body { background-color: var(--bg-color); color: var(--text-color); }\n"
   "      header { background-color: var(--header-bg); color: var(--header-text); box-shadow: 0 2px 5px rgba(0,0,0,0.2); }\n"
   "      .brand-logo { padding-left: 15px !important; }\n"
   "      .search-wrapper input { border-bottom: none !important; box-shadow: none !important; color: var(--header-text); }\n"
   "      .search-wrapper { background-color: var(--search-bg); border-radius: 4px; padding: 0 15px; }\n"
   "      main { padding: 20px 0; }\n"
   "      .category-title { color: var(--primary-color); border-bottom: 2px solid var(--primary-color-light); padding-bottom: 5px; margin-top: 2rem; }\n"
   "      .link-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(220px, 1fr)); gap: 1rem; margin-top: 1.5rem; }\n"
   "      .link-card { display: flex; align-items: center; background-color: var(--card-bg); padding: 12px 16px; border-radius: 8px; text-decoration: none; color: var(--text-color); transition: transform 0.2s, box-shadow 0.2s; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }\n"
   "      .link-card:hover { transform: translateY(-3px); box-shadow: 0 4px 10px rgba(0,0,0,0.15); }\n"
   "      .link-card .material-icons { margin-right: 16px; color: var(--primary-color); }\n"
   "      .link-card .favicon { width: 24px; height: 24px; margin-right: 16px; border-radius: 4px; }\n"
   "      .link-card .letter-icon { width: 24px; height: 24px; margin-right: 16px; border-radius: 50%; background-color: var(--primary-color); color: white; display: inline-flex; align-items: center; justify-content: center; font-weight: bold; font-size: 14px; }\n"
   "      .link-card .link-label { font-weight: 500; }\n"
   "      .collapsible-header { background-color: var(--card-bg) !important; font-size: 1.5em; }\n"
   "      .collapsible-body { background-color: var(--bg-color) !important; padding: 2rem 1rem 1rem 1rem !important; border-bottom: none !important; }\n"
   "      footer { text-align: center; padding: 20px; color: #9e9e9e; font-size: 0.9em; }\n"
   "    </style>\n"
   "</head>"))

(defun jme-org-dashboard--generate-html-body (data)
  "Generate the <body> section of the HTML based on DATA."
  (format
   "<body class=\"%s-theme\">
    <header>
      <nav class=\"transparent z-depth-0\">
        <div class=\"nav-wrapper container\">
          <a href=\"#\" class=\"brand-logo\">%s</a>
          <ul class=\"right\">
            <li class=\"hide-on-med-and-down search-wrapper\">
              <div class=\"input-field\">
                <input id=\"search\" type=\"search\" placeholder=\"Search links...\">
                <label class=\"label-icon\" for=\"search\"><i class=\"material-icons\">search</i></label>
                <i class=\"material-icons\">close</i>
              </div>
            </li>
            <li><a href=\"#\" id=\"theme-toggle\"><i class=\"material-icons\">brightness_6</i></a></li>
          </ul>
        </div>
      </nav>
    </header>

    <main class=\"container\">
      %s
    </main>

    <footer class=\"page-footer transparent\">
        <div class=\"footer-copyright grey-text text-darken-1\">
            <div class=\"container\">
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
  "Generate HTML for all CATEGORIES."
  (if jme-org-dashboard-collapsible-sections
      (format "<ul class=\"collapsible expandable z-depth-0\" style=\"border: none;\">%s</ul>"
              (mapconcat (lambda (cat)
                           (format "
        <li data-category-title=\"%s\">
          <div class=\"collapsible-header\"><i class=\"material-icons\">%s</i>%s</div>
          <div class=\"collapsible-body\"><div class=\"link-grid\">%s</div></div>
        </li>"
                                   (plist-get cat :title)
                                   (if (string-match "🛠️" (plist-get cat :title)) "build" "folder")
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
  "Generate HTML for a single LINK card."
  (let* ((label (plist-get link :label))
         (url (plist-get link :url))
         (icon (plist-get link :icon))
         (domain (replace-regexp-in-string "^https?://\\([^/]+\\)/?.*" "\\1" url)))
    (format "<a href=\"%s\" target=\"_blank\" class=\"link-card\" data-label=\"%s\">
    %s
    <span class=\"link-label\">%s</span>
</a>"
            url
            label
            (cond
             (icon (format "<i class=\"material-icons\">%s</i>" icon))
             (jme-org-dashboard-icon-service-url (format "<img src=\"%s\" class=\"favicon\" alt=\"\" onerror=\"this.style.display='none'; this.nextElementSibling.style.display='inline-flex';\">
<span class=\"letter-icon\" style=\"display: none;\">%s</span>" (format jme-org-dashboard-icon-service-url domain) (upcase (substring label 0 1))))
             (t (format "<span class=\"letter-icon\">%s</span>" (upcase (substring label 0 1)))))
            label)))

(defun jme-org-dashboard--generate-javascript ()
  "Generate the inline JavaScript for interactivity."
  "
document.addEventListener('DOMContentLoaded', function() {
    // Initialize Materialize components
    if (document.querySelector('.collapsible')) {
        M.Collapsible.init(document.querySelectorAll('.collapsible'), { accordion: false });
    }

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

    // Search Filtering
    const searchInput = document.getElementById('search');
    const linkCards = Array.from(document.querySelectorAll('.link-card'));
    const categories = Array.from(document.querySelectorAll('[data-category-title]'));

    searchInput.addEventListener('input', (e) => {
        const query = e.target.value.toLowerCase();

        linkCards.forEach(card => {
            const label = card.dataset.label.toLowerCase();
            const shouldShow = label.includes(query);
            card.style.display = shouldShow ? 'flex' : 'none';
        });

        // Hide empty categories
        categories.forEach(category => {
            const visibleLinks = category.querySelectorAll('.link-card[style*=\"display: flex\"], .link-card:not([style*=\"display: none\"])');
            category.style.display = visibleLinks.length > 0 ? '' : 'none';
        });
    });
});
")

;;;###autoload
(defun jme-org-dashboard-export-to-html ()
  "Generate a personal HTML dashboard from the current Org-mode buffer.

The command parses the buffer for level 1 headlines (categories)
and list items containing links.  It uses document properties like
#+TITLE and #+DASHBOARD-THEME for configuration.

The output is a single, self-contained HTML file configured via
`jme-org-dashboard-output-file`."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (error "This command must be run in an Org-mode buffer"))
  (let* ((dashboard-data (jme-org-dashboard--parse-org-data))
         (html-content (jme-org-dashboard--generate-html dashboard-data))
         (output-file jme-org-dashboard-output-file))
    (with-temp-file output-file
      (insert html-content))
    (message "Dashboard exported successfully to %s" output-file)
    (when (y-or-n-p "Open the exported dashboard in a browser? ")
      (browse-url-of-file (expand-file-name output-file)))))

;; --- Capture Helpers ---

(defun jme-org-dashboard--get-category-titles ()
  "Parse `jme-org-dashboard-org-file` and return a list of category titles."
  (unless (file-exists-p jme-org-dashboard-org-file)
    (error "Dashboard Org file not found at: %s" jme-org-dashboard-org-file))
  (with-temp-buffer
    (insert-file-contents jme-org-dashboard-org-file)
    (let ((org-tree (org-element-parse-buffer 'headline)))
      (let (titles)
        (dolist (element (org-element-contents org-tree))
          (when (and (eq (org-element-type element) 'headline)
                     (= (org-element-property :level element) 1))
            (push (org-element-property :raw-value element) titles)))
        (nreverse titles)))))

(defun jme-org-dashboard--prompt-for-category ()
  "Prompt the user to select a category from the existing dashboard file."
  (let ((categories (jme-org-dashboard--get-category-titles)))
    (unless categories
      (error "No level 1 headlines (categories) found in %s" jme-org-dashboard-org-file))
    (completing-read "Category: " categories nil t)))

(defun jme-org-dashboard--build-capture-template (url description icon tags)
  "Build the text for the capture template with URL, DESCRIPTION, ICON, and TAGS.
Handles optional ICON and TAGS properties gracefully."
  (let ((props '()))
    ;; Add properties to the list only if they are not empty strings.
    (when (and icon (not (string-empty-p icon)))
      (push (format ":ICON: %s" icon) props))
    (when (and tags (not (string-empty-p tags)))
      (push (format ":TAGS: %s" tags) props))

    (if (null props)
        ;; If no properties were provided, insert only the link.
        (format "- [[%s][%s]]" url description)
      ;; Otherwise, build the full list item with a properties drawer.
      (format "- [[%s][%s]]\n  :PROPERTIES:\n%s\n  :END:"
              url
              description
              (mapconcat (lambda (p) (format "  %s" p))
                         (nreverse props) ; Reverse to get original order
                         "\n")))))

(provide 'jme-org-dashboard)
;;; jme-org-dashboard.el ends here
