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
            (message "CATEGORY: %s" title)
            (message "LINKS: %s" links)
            (when links
              (push (list :title title :links links) categories))))))

    (let ((sorted-categories (nreverse categories)))
      (if (eq jme-org-dashboard-sort-order 'alphabetical)
          (sort sorted-categories (lambda (a b) (string< (plist-get a :title) (plist-get b :title))))
        sorted-categories))))

(defun jme-org-dashboard--get-properties-for-link (link-element)
  "Find the property plist for a LINK-ELEMENT by navigating up the AST.
This is the robust way to find properties associated with a link in a list."
  (let (properties-plist)
    ;; A link is inside a paragraph, which is inside an item.
    ;; We need to get the parent's parent.
    (when-let* ((parent (org-element-property :parent link-element))
                (item (org-element-property :parent parent)))
      ;; Ensure we have found the 'item' element.
      (when (eq (org-element-type item) 'item)
        ;; Now that we have the correct item, find its property drawer.
        (when-let ((drawer (org-element-map (org-element-contents item) 'property-drawer 'first-match)))
          (message "DRAWER: %s" drawer)
          (dolist (node-prop (org-element-contents drawer))
            (when (eq (org-element-type node-prop) 'node-property)
              (when-let ((key-str (org-element-property :key node-prop)))
                (setq properties-plist
                      (plist-put properties-plist
                                 (intern (concat ":" key-str))
                                 (org-element-property :value node-prop)))))))))
    properties-plist))

(defun jme-org-dashboard--get-item-properties-old (item-element)
  "Extract the property plist from a property drawer within an ITEM-ELEMENT.
This corrected version properly iterates over all node-properties."
  (message "--> 1 <--")
  (let (properties-plist)
    ;; Find the property drawer associated with the item.
    ;; 'first-match is efficient as there should only be one.
    (when-let ((drawer (org-element-map (org-element-contents item-element) 'property-drawer 'first-match)))
      (message "--> 2 <--")
      ;; Iterate over each property line inside the drawer.
      (dolist (node-prop (org-element-contents drawer))
        (message "--> 3 <--")
        ;; Ensure we are only looking at node-property elements.
        (when (eq (org-element-type node-prop) 'node-property)
          (message "--> 4 <--")
          (let* ((key-str (org-element-property :key node-prop))
                 (value   (org-element-property :value node-prop)))
            (message "--> 5 <--")
            ;; Check that we have a valid key to avoid errors.
            (when (and key-str (> (length key-str) 0))
              (message "--> 6 <--")
              ;; Convert the string "KEY" to a Lisp keyword :KEY
              (let ((key-symbol (intern (concat ":" key-str))))
                (message "--> 7 <--")
                ;; Add the key-value pair to our plist.
                (setq properties-plist (plist-put properties-plist key-symbol value))))))))
    properties-plist))

(defun jme-org-dashboard--get-item-properties-old-old (item-element)
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
  "Extract links and their associated properties from a HEADLINE's section."
  (let (links)
    ;; Find all links within the headline's contents. This is simpler and more direct.
    (org-element-map (org-element-contents headline) 'link
      (lambda (link)
        (let* (;; NEW: Call the robust function to get properties for THIS link.
               (props (jme-org-dashboard--get-properties-for-link link))
               (url (org-element-property :raw-link link))
               (label (or (car (org-element-contents link)) url))
               (icon (plist-get props :ICON))
               (note (plist-get props :NOTE))
               (tags (when-let (tags-str (plist-get props :TAGS))
                       (split-string tags-str "," t " "))))
          (push (list :url url :label label :icon icon :note note :tags tags) links))))
    ;; The sorting logic remains the same.
    (let ((sorted-links (nreverse links)))
       (if (eq jme-org-dashboard-sort-order 'alphabetical)
           (sort sorted-links (lambda (a b) (string< (plist-get a :label) (plist-get b :label))))
         sorted-links))))

(defun jme-org-dashboard--extract-links-from-headline-old (headline)
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
              (message "PROPS: %s" props)
              (message "ITEM: %s (%s) %s" label icon note)
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
  "Generate the <head> section with a major visual overhaul from DATA."
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
  "Generate the <body> section with an improved layout and controls from DATA."
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
    ;; Non-collapsible version (unchanged, but still here for completeness)
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
    (message "RAW: %s" link)
    (message "LINK: %s (%s) - %s" label icon note)
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
            url ; Tooltip shows the full URL
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
