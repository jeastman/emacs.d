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
  "Generate the <head> section for DATA, correctly loading the MWC library."
  (concat
   "<head>\n"
   "    <meta charset=\"utf-8\">\n"
   "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
   (format "    <title>%s</title>\n" (plist-get data :title))
   "    <!-- Google Fonts: Roboto and Material Symbols -->\n"
   "    <link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">\n"
   "    <link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>\n"
   "    <link href=\"https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700&display=swap\" rel=\"stylesheet\">\n"
   "    <link href=\"https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined\" rel=\"stylesheet\" />\n"
   "    <!-- Fuse.js for Fuzzy Searching -->\n"
   "    <script src=\"https://cdn.jsdelivr.net/npm/fuse.js/dist/fuse.min.js\"></script>\n"
   "    <!-- THE FIX: MWC library MUST be loaded in the <head> -->\n"
   "    <script type=\"module\" src=\"https://esm.run/@material/web/all.js\"></script>\n"
   "    <style>\n"
   "      /* MWC Theming and Custom Layouts */\n"
   "      :root {\n"
   "        --md-sys-typescale-body-large-font: 'Roboto', sans-serif;\n"
   "        --md-sys-typescale-title-large-font: 'Roboto', sans-serif;\n"
   "        --md-sys-typescale-label-large-font: 'Roboto', sans-serif;\n"
   "        --md-ref-typeface-brand: 'Roboto', sans-serif;\n"
   "        --padding: 1.5rem;\n"
   "      }\n"
   "      body.light-theme {\n"
   "        --md-sys-color-background: #f8f9fa; --md-sys-color-on-background: #1b1c1e;\n"
   "        --md-sys-color-surface: #f8f9fa; --md-sys-color-on-surface: #1b1c1e;\n"
   "        --md-sys-color-surface-container-highest: #e3e3e6; --md-sys-color-on-surface-variant: #44474b;\n"
   "        --md-sys-color-primary: #006399; --md-sys-color-on-primary: #ffffff;\n"
   "        --md-sys-color-outline: #74777b; --md-sys-color-shadow: #000000;\n"
   "        --hover-overlay: rgba(0, 0, 0, 0.04);\n"
   "        --chip-hover-bg: #d0d0d3;\n"
   "      }\n"
   "      body.dark-theme {\n"
   "        --md-sys-color-background: #121212; --md-sys-color-on-background: #e3e3e6;\n"
   "        --md-sys-color-surface: #121212; --md-sys-color-on-surface: #e3e3e6;\n"
   "        --md-sys-color-surface-container-highest: #333639; --md-sys-color-on-surface-variant: #c4c7cc;\n"
   "        --md-sys-color-primary: #8fcaff; --md-sys-color-on-primary: #003352;\n"
   "        --md-sys-color-outline: #8e9195; --md-sys-color-shadow: #000000;\n"
   "        --hover-overlay: rgba(255, 255, 255, 0.08);\n"
   "        --chip-hover-bg: #55585b;\n"
   "      }\n"
   "      body { background-color: var(--md-sys-color-background); color: var(--md-sys-color-on-background); }\n"
   "      header { display: flex; align-items: center; justify-content: space-between; padding: 0.5rem var(--padding); background-color: var(--md-sys-color-surface); border-bottom: 1px solid var(--md-sys-color-outline); }\n"
   "      header h1 { font-size: 1.5rem; font-weight: 500; margin: 0; }\n"
   "      .header-controls { display: flex; align-items: center; gap: 0.5rem; }\n"
   "      md-outlined-text-field { min-width: 250px; --md-outlined-text-field-container-shape: 20px; }\n"
   "      main { max-width: 1200px; margin: 0 auto; padding: var(--padding); }\n"
   "      .category { margin-bottom: 2rem; }\n"
   "      .category-header {\n"
   "        background: none; border: none; font: inherit; text-align: left; width: 100%;\n"
   "        display: flex; align-items: center; gap: 0.75rem; cursor: pointer;\n"
   "        padding: 0.75rem 0.5rem; border-radius: 8px;\n"
   "        font-size: 1.5rem; font-weight: 500; color: var(--md-sys-color-on-surface);\n"
   "        transition: background-color 0.2s ease-out;\n"
   "      }\n"
   "      .category-header:hover { background-color: var(--hover-overlay); }\n"
   "      .category-header md-icon { transition: transform 0.3s ease; color: var(--md-sys-color-primary); }\n"
   "      .category-header[aria-expanded='false'] md-icon { transform: rotate(-90deg); }\n"
   "      .category-body { padding-top: 1rem; }\n"
   "      .category-body[hidden] { display: none; }\n"
   "      .link-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(280px, 1fr)); gap: 1rem; }\n"
   "      .link-card {\n"
   "        display: flex; flex-direction: column; text-decoration: none; padding: 1rem;\n"
   "        background-color: var(--md-sys-color-surface-container-highest); color: var(--md-sys-color-on-surface);\n"
   "        border-radius: 12px; position: relative; overflow: hidden; --md-ripple-hover-color: var(--md-sys-color-primary);\n"
   "        border: 2px solid transparent;\n"
   "        transition: transform 0.2s ease-out, border-color 0.2s ease-out;\n"
   "      }\n"
   "      .link-card.is-focused {\n"
   "        border-color: var(--md-sys-color-primary);\n"
   "        transform: scale(1.03);\n"
   "      }\n"
   "      .link-card-header { display: flex; align-items: center; gap: 1rem; }\n"
   "      .link-card-header .favicon { width: 32px; height: 32px; object-fit: contain; border-radius: 6px; }\n"
   "      .link-card-header .letter-icon { width: 32px; height: 32px; border-radius: 6px; background-color: var(--md-sys-color-primary); color: var(--md-sys-color-on-primary); display: flex; align-items: center; justify-content: center; font-weight: bold; font-size: 18px; text-transform: uppercase; }\n"
   "      .link-label { font-size: 1.1rem; font-weight: 500; line-height: 1.3; }\n"
   "      .link-note { font-size: 0.9rem; color: var(--md-sys-color-on-surface-variant); margin-top: 0.75rem; flex-grow: 1; }\n"
   "      .link-tags { display: flex; flex-wrap: wrap; gap: 0.5rem; margin-top: 1rem; }\n"
   "      .tag-chip {\n"
   "        background: none; border: none; font: inherit;\n"
   "        font-size: 0.75rem; font-weight: 500; padding: 0.25rem 0.6rem; cursor: pointer;\n"
   "        border-radius: 8px; \n"
   "        background-color: var(--md-sys-color-surface-container-highest);\n"
   "        color: var(--md-sys-color-on-surface-variant);\n"
   "        transition: background-color 0.2s ease;\n"
   "      }\n"
   "      .tag-chip:hover { background-color: var(--chip-hover-bg); }\n"
   "      .main-controls { display: flex; justify-content: flex-end; gap: 1rem; margin-bottom: 2rem; }\n"
   "    </style>\n"
   "</head>"))

(defun jme-org-dashboard--generate-html-body (data)
  "Generate the <body> section for DATA using MWC custom elements."
  (format
   "<body class=\"%s-theme\">
    <header>
      <h1>%s</h1>
      <div class=\"header-controls\">
        <md-outlined-text-field id=\"search\" label=\"Search\" type=\"search\" autocomplete=\"off\">
          <md-icon slot=\"leading-icon\">search</md-icon>
        </md-outlined-text-field>
        <md-icon-button id=\"theme-toggle\" aria-label=\"Toggle Theme\">
          <md-icon>brightness_6</md-icon>
        </md-icon-button>
      </div>
    </header>

    <main>
      <div class=\"main-controls\">
        <md-filled-button id=\"expand-all\"><md-icon slot=\"icon\">unfold_more</md-icon>Expand All</md-filled-button>
        <md-filled-button id=\"collapse-all\"><md-icon slot=\"icon\">unfold_less</md-icon>Collapse All</md-filled-button>
      </div>
      %s
    </main>

    <!-- THE FIX: This script now only contains our application logic -->
    <script type=\"module\">%s</script>
</body>"
   (if (eq (plist-get data :theme) 'dark) "dark" "light")
   (plist-get data :title)
   (jme-org-dashboard--generate-categories-html (plist-get data :categories))
   (jme-org-dashboard--generate-javascript)))

(defun jme-org-dashboard--generate-categories-html (categories)
  "Generate CATEGORIES sections for MWC, using ARIA for collapsibility."
  (mapconcat
   (lambda (cat)
     (format
      "<section class=\"category\" data-category-title=\"%s\">
         <button class=\"category-header\" aria-expanded=\"true\" aria-controls=\"category-body-%s\">
           <md-icon>expand_more</md-icon>
           %s
         </button>
         <div class=\"category-body\" id=\"category-body-%s\">
           <div class=\"link-grid\">%s</div>
         </div>
       </section>"
      (plist-get cat :title)
      (jme-org-dashboard--slugify (plist-get cat :title))
      (plist-get cat :title)
      (jme-org-dashboard--slugify (plist-get cat :title))
      (mapconcat #'jme-org-dashboard--generate-link-html (plist-get cat :links) "\n")))
   categories "\n"))

;; NEW HELPER FUNCTION: Add this anywhere before the function above.
(defun jme-org-dashboard--slugify (text)
  "Convert TEXT into a URL-friendly slug."
  (let ((slug (downcase text)))
    (setq slug (replace-regexp-in-string "[^a-z0-9]+" "-" slug))
    (setq slug (replace-regexp-in-string "^-\\|-$" "" slug))
    slug))

(defun jme-org-dashboard--generate-link-html (link)
  "Generate HTML for a single LINK card, now with clickable tag buttons."
  (let* ((label (plist-get link :label))
         (url (plist-get link :url))
         (icon (plist-get link :icon))
         (note (plist-get link :note))
         (tags (plist-get link :tags))
         (domain (replace-regexp-in-string "^https?://\\([^/]+\\)/?.*" "\\1" url))
         (unique-id (md5 (concat url label))))
    (format "<a id=\"card-%s\" href=\"%s\" target=\"_blank\" class=\"link-card\" data-label=\"%s\" data-tags=\"%s\">
    <md-ripple></md-ripple>
    <div class=\"link-card-header\">
        %s
        <span class=\"link-label\">%s</span>
    </div>
    %s
    %s
</a>"
            unique-id
            url
            label
            (if tags (string-join tags " ") "")
            (cond
             (icon (format "<md-icon>%s</md-icon>" icon))
             (jme-org-dashboard-icon-service-url
              (format "<img src=\"%s\" class=\"favicon\" alt=\"\" onerror=\"this.style.display='none'; this.nextElementSibling.style.display='flex';\">
<span class=\"letter-icon\" style=\"display: none;\">%s</span>"
                      (format jme-org-dashboard-icon-service-url domain)
                      (upcase (substring label 0 1))))
             (t (format "<span class=\"letter-icon\">%s</span>" (upcase (substring label 0 1)))))
            label
            (if note (format "<p class=\"link-note\">%s</p>" note) "")
            ;; NEW: Tags are now <button> elements.
            (if tags
                (format "<div class=\"link-tags\">%s</div>"
                        (mapconcat (lambda (tag)
                                     (format "<button class=\"tag-chip\" data-tag=\"%s\">%s</button>" tag tag))
                                   tags
                                   "\n"))
              ""))))

(defun jme-org-dashboard--generate-javascript ()
  "Generate the inline JavaScript, with Emacs-style keyboard navigation."
  "
// --- Theme Toggling ---
const themeToggle = document.getElementById('theme-toggle');
const body = document.body;
const currentTheme = localStorage.getItem('dashboard-theme') || (body.classList.contains('dark-theme') ? 'dark' : 'light');
function applyTheme(theme) {
    body.classList.remove('light-theme', 'dark-theme');
    body.classList.add(theme + '-theme');
    localStorage.setItem('dashboard-theme', theme);
}
applyTheme(currentTheme);
themeToggle.addEventListener('click', () => {
    applyTheme(body.classList.contains('dark-theme') ? 'light' : 'dark');
});

// --- Collapsible Sections ---
const categoryHeaders = document.querySelectorAll('.category-header');
categoryHeaders.forEach(header => {
    const body = document.getElementById(header.getAttribute('aria-controls'));
    header.addEventListener('click', () => {
        const isExpanded = header.getAttribute('aria-expanded') === 'true';
        header.setAttribute('aria-expanded', !isExpanded);
        body.hidden = isExpanded;
    });
});
document.getElementById('expand-all').addEventListener('click', () => {
    categoryHeaders.forEach(h => {
        h.setAttribute('aria-expanded', 'true');
        document.getElementById(h.getAttribute('aria-controls')).hidden = false;
    });
});
document.getElementById('collapse-all').addEventListener('click', () => {
    categoryHeaders.forEach(h => {
        h.setAttribute('aria-expanded', 'false');
        document.getElementById(h.getAttribute('aria-controls')).hidden = true;
    });
});

// --- Search Filtering ---
const searchInput = document.getElementById('search');
const linkCards = Array.from(document.querySelectorAll('.link-card'));
const categories = Array.from(document.querySelectorAll('.category'));

const searchableData = linkCards.map(card => ({
    id: card.id,
    label: card.dataset.label,
    tags: card.dataset.tags,
    note: card.querySelector('.link-note')?.textContent || ''
}));

const fuseOptions = {
    includeScore: true,
    threshold: 0.4,
    keys: ['label', 'tags', 'note']
};
const fuse = new Fuse(searchableData, fuseOptions);

function performSearch(query) {
    if (query === '') {
        linkCards.forEach(card => card.style.display = 'flex');
    } else {
        linkCards.forEach(card => card.style.display = 'none');
        const results = fuse.search(query);
        results.forEach(result => {
            const cardToShow = document.getElementById(result.item.id);
            if (cardToShow) {
                cardToShow.style.display = 'flex';
            }
        });
    }

    categories.forEach(category => {
        const visibleLinks = category.querySelectorAll('.link-card[style*=\"display: flex\"]');
        category.style.display = visibleLinks.length > 0 ? 'block' : 'none';
    });
}

searchInput.addEventListener('input', (e) => {
    performSearch(e.target.value.trim());
});

// --- NEW: Tag Filtering Logic ---
document.querySelector('main').addEventListener('click', (e) => {
    // Check if a tag-chip was clicked
    if (e.target.classList.contains('tag-chip')) {
        e.preventDefault(); // Prevent the parent <a> tag from navigating
        e.stopPropagation(); // Stop the event from bubbling up further

        const tag = e.target.dataset.tag;
        // If the tag is already the search query, clear it. Otherwise, search for the tag.
        if (searchInput.value === tag) {
            searchInput.value = '';
        } else {
            searchInput.value = tag;
        }

        // Manually trigger the input event to run the search
        searchInput.dispatchEvent(new Event('input', { bubbles: true }));
    }
});

// --- Keyboard Navigation (with Emacs-style keys) ---
let focusedCard = null;

function setFocus(card) {
    if (focusedCard) {
        focusedCard.classList.remove('is-focused');
    }
    focusedCard = card;
    if (focusedCard) {
        focusedCard.classList.add('is-focused');
        focusedCard.scrollIntoView({ block: 'nearest', behavior: 'smooth' });
    }
}

document.addEventListener('keydown', (e) => {
    // Don't interfere when typing in the search bar
    if (document.activeElement === searchInput) {
        if (e.key === 'Escape') {
            searchInput.blur();
        }
        return;
    }

    const visibleCards = Array.from(document.querySelectorAll('.link-card:not([style*=\"display: none\"])'));
    if (visibleCards.length === 0 && !['s', '/'].includes(e.key)) return;

    let currentIndex = focusedCard ? visibleCards.indexOf(focusedCard) : -1;

    switch (e.key) {
        // UPDATED: 's' now focuses the search bar ('/' is kept as an alias)
        case 's':
        case '/':
            e.preventDefault();
            searchInput.focus();
            setFocus(null);
            break;

        case 'Escape':
            setFocus(null);
            break;

        // UPDATED: 'n' now moves down
        case 'n':
        case 'ArrowDown':
            e.preventDefault();
            const nextIndex = currentIndex < visibleCards.length - 1 ? currentIndex + 1 : 0;
            setFocus(visibleCards[nextIndex]);
            break;

        // UPDATED: 'p' now moves up
        case 'p':
        case 'ArrowUp':
            e.preventDefault();
            const prevIndex = currentIndex > 0 ? currentIndex - 1 : visibleCards.length - 1;
            setFocus(visibleCards[prevIndex]);
            break;

        case 'Enter':
            if (focusedCard) {
                e.preventDefault();
                focusedCard.click();
            }
            break;
    }
});
")

(defun jme-org-dashboard--generate-javascript-old ()
  "Generate the inline JavaScript, with tag filtering."
  "
// --- Theme Toggling ---
const themeToggle = document.getElementById('theme-toggle');
const body = document.body;
const currentTheme = localStorage.getItem('dashboard-theme') || (body.classList.contains('dark-theme') ? 'dark' : 'light');
function applyTheme(theme) {
    body.classList.remove('light-theme', 'dark-theme');
    body.classList.add(theme + '-theme');
    localStorage.setItem('dashboard-theme', theme);
}
applyTheme(currentTheme);
themeToggle.addEventListener('click', () => {
    applyTheme(body.classList.contains('dark-theme') ? 'light' : 'dark');
});

// --- Collapsible Sections ---
const categoryHeaders = document.querySelectorAll('.category-header');
categoryHeaders.forEach(header => {
    const body = document.getElementById(header.getAttribute('aria-controls'));
    header.addEventListener('click', () => {
        const isExpanded = header.getAttribute('aria-expanded') === 'true';
        header.setAttribute('aria-expanded', !isExpanded);
        body.hidden = isExpanded;
    });
});
document.getElementById('expand-all').addEventListener('click', () => {
    categoryHeaders.forEach(h => {
        h.setAttribute('aria-expanded', 'true');
        document.getElementById(h.getAttribute('aria-controls')).hidden = false;
    });
});
document.getElementById('collapse-all').addEventListener('click', () => {
    categoryHeaders.forEach(h => {
        h.setAttribute('aria-expanded', 'false');
        document.getElementById(h.getAttribute('aria-controls')).hidden = true;
    });
});

// --- Search Filtering ---
const searchInput = document.getElementById('search');
const linkCards = Array.from(document.querySelectorAll('.link-card'));
const categories = Array.from(document.querySelectorAll('.category'));

const searchableData = linkCards.map(card => ({
    id: card.id,
    label: card.dataset.label,
    tags: card.dataset.tags,
    note: card.querySelector('.link-note')?.textContent || ''
}));

const fuseOptions = {
    includeScore: true,
    threshold: 0.4,
    keys: ['label', 'tags', 'note']
};
const fuse = new Fuse(searchableData, fuseOptions);

function performSearch(query) {
    if (query === '') {
        linkCards.forEach(card => card.style.display = 'flex');
    } else {
        linkCards.forEach(card => card.style.display = 'none');
        const results = fuse.search(query);
        results.forEach(result => {
            const cardToShow = document.getElementById(result.item.id);
            if (cardToShow) {
                cardToShow.style.display = 'flex';
            }
        });
    }

    categories.forEach(category => {
        const visibleLinks = category.querySelectorAll('.link-card[style*=\"display: flex\"]');
        category.style.display = visibleLinks.length > 0 ? 'block' : 'none';
    });
}

searchInput.addEventListener('input', (e) => {
    performSearch(e.target.value.trim());
});

// --- NEW: Tag Filtering Logic ---
document.querySelector('main').addEventListener('click', (e) => {
    // Check if a tag-chip was clicked
    if (e.target.classList.contains('tag-chip')) {
        e.preventDefault(); // Prevent the parent <a> tag from navigating
        e.stopPropagation(); // Stop the event from bubbling up further

        const tag = e.target.dataset.tag;
        // If the tag is already the search query, clear it. Otherwise, search for the tag.
        if (searchInput.value === tag) {
            searchInput.value = '';
        } else {
            searchInput.value = tag;
        }

        // Manually trigger the input event to run the search
        searchInput.dispatchEvent(new Event('input', { bubbles: true }));
    }
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
