;;; ox-blog.el --- Blog Back-End for Org Export Engine

;; Copyright (C) 2016, Victor Santos

;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; Keywords: Blog, HTML

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements an blog back-end for the Org
;; generic exporter.

;; To test it, run:
;;
;;   M-x org-blog-export-as-html
;;
;; in an Org mode buffer.  See ox.el and ox-html.el for more details
;; on how this exporter works.

;;; Code:

;;; Requirements

(require 'ox-html)
(eval-when-compile
  (require 'ox)
  (require 's)
  (require 'f)
  (use-package htmlize :ensure t)
  (require 'ox-extra))

(defgroup org-export-blog nil
  "Options specific to blog backend."
  :tag "Org Blog"
  :group 'org-export
  :version "24.5"
  :package-version '(Org . "8.0"))

(defcustom org-blog-extension "html"
  "The extension for exported Blog files."
  :group 'org-export-blog
  :type 'string)

(defcustom org-blog-coding-system 'utf-8
  "Coding system for Blog export.
Defaults to 'utf-8."
  :group 'org-export-blog
  :version "24.5"
  :package-version '(Org . "8.0")
  :type 'coding-system)

(defcustom org-blog-template nil
  "Non-nil means insert body contents inside a template in Blog export.

 When t, insert a string as defined by the formatting string in
 `org-blog-template-format'.

 When set to a string, verifies if the string refers to a file path. If yes, use
 the file as an HTML template for the preamble; otherwise, use the string itself
 as the preamble. In both cases the returned string accepts lisp tags
 `<lisp></lisp>' to enclose lisp code to be evaluated during exporting.

 When set to a function, apply this function and insert the returned string. The
 function takes the property list of export options as its only argument."

  :group 'org-export-blog
  :type '(choice (const :tag "No template" nil)
                 (const :tag "Default formatting string" t)
                 (const :tag "Custom formatting string")
                 (function :tag "Function (must return a string)")))

(defcustom org-blog-template-format
  "
<!doctype html>
<html language=\"{{ language }}\">
<head>
<meta charset=\"{{ charset }}\">
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<title>{{ title }}</title>
<meta name=\"generator\" content=\"Org-mode\">
<meta name=\"author\" content=\"{{ author }}\">
{{ head }}
</head>
<body>
{{ preamble }}
{{ contents }}
{{ postamble }}
</body>
</html>"
  "Format string for Blog contents.

The format string can contain these elements:

  {{head}}            will be replace by custom head.
  {{preamble}}        will be replaced by the preamble
  {{postamble}}       will be replaced by the postamble
  {{title}}           stands for title.
  {{author}}          stands for author's name.
  {{email}}           stands for author's e-mail.
  {{date}}            stands for date.
  {{validation-link}} will be replaced by `org-blog-validation-link'.
  {{modified}}       will be replaced by the last modification time.
  {{contents}}        stands for the file contents."
  :group 'org-export-blog
  :type 'string)

(defcustom org-blog-preamble t
  "Non-nil means insert a preamble in Blog export.

When t, insert a string as defined by the formatting string in
`org-blog-preamble-format'.

When set to a string, verifies if the string refers to a file path. If yes, use
the file as an HTML template for the preamble; otherwise, use the string itself
as the preamble. In both cases the returned string accepts lisp tags
`<lisp></lisp>' to enclose lisp code to be evaluated during exporting.

When set to a function, apply this function and insert the returned string. The
function takes the property list of export options as its only argument."
  :group 'org-export-blog
  :type '(choice (const :tag "No preamble" nil)
                 (const :tag "Default formatting string" t)
                 (const :tag "Custom formatting string or path for template")
                 (function :tag "Function (must return a string)")))

(defcustom org-blog-preamble-format ""
  "Format template for preamble"
  :group 'org-export-blog
  :type 'string)

(defcustom org-blog-postamble t
  "Non-nil means insert a postamble in Blog export.

 When t, insert a string as defined by the formatting string in
 `org-blog-postamble-format'.

 When set to a string, verifies if the string refers to a file path. If yes, use
 the file as an HTML template for the postamble; otherwise, use the string itself
 as the postamble. In both cases the returned string accepts lisp tags
 `<lisp></lisp>' to enclose lisp code to be evaluated during exporting.

 When set to a function, apply this function and insert the returned string. The
 function takes the property list of export options as its only argument."
  :group 'org-export-blog
  :type '(choice (const :tag "No postamble" nil)
                 (const :tag "Default formatting string" t)
                 (const :tag "Custom formatting string or path for template")
                 (function :tag "Function (must return a string)")))

(defcustom org-blog-postamble-format
  "
<footer id=\"postamble\" class=\"status\">
<p class=\"author\">Author: {{ author }}</p>
<p class=\"date\">Created: {{ created }}</p>
<p class=\"validation\">{{ validation-link }}</p>
</footer>"
  "Format template for postamble"
  :group 'org-export-blog
  :type 'string)

(defcustom org-blog-validation-link
  "<a href=\"http://validator.w3.org/check?uri=referer\">Validate</a>"
  "Link to HTML validation service."
  :group 'org-export-blog
  :type 'string)

(defcustom org-blog-lang "en"
  "Set page language"
  :group 'org-export-blog
  :version "24.5"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-blog-flush-lines nil
  "Non-nil means to remove empty lines from the generated HTML.
Warning: non-nil may break indentation of source code blocks,
and change file structure"
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)


;;; Define Back-End
(org-export-define-derived-backend 'blog 'html
  :menu-entry
  '(?b "Export to custom html"
       ((?B "To temporary buffer" org-blog-export-as-blog)
        (?b "To file" org-blog-export-to-blog)))
  :options-alist
  '(
    (:blog-template nil nil org-blog-template-format)
    (:blog-preamble nil nil org-blog-preamble-format)
    (:blog-postamble nil nil org-blog-postamble-format)
    (:blog-language "BLOG_LANGUAGE" nil org-blog-lang)
    (:blog-flush-lines nil nil org-blog-flush-lines)
    (:blog-root-directory nil "blog-root-directory" t)
    (:description "DESCRIPTION" nil nil newline)
    (:keywords "KEYWORDS" nil nil space)
    (:created "CREATED" nil nil newline)
    (:modified "MODIFIED" nil nil newline)
    )
  :translate-alist
  '((template . org-blog--build-template))
  :filters-alist
  '((:filter-final-output . org-blog--final-function)))

;;; Internal functions

(defun org-blog--eval-lisp ()
  "Eval embeded lisp code defined by <lisp> tags in html fragment
when publishing a page."
  ;; Stolen from
  ;; https://github.com/renard/o-blog/blob/master/lisp/o-blog-utils.el
  (save-excursion
    (save-restriction
      (save-match-data
        ;; needed for thing-at-point
        (html-mode)
        (beginning-of-buffer)
        (let ((open-tag "<lisp>\\|{lisp}\\|\\[lisp\\]")
              (close-tag "</lisp>\\|{/lisp}\\|\\[/lisp\\]")
              beg end sexp)
          (while (search-forward-regexp open-tag nil t)
            (setq beg (- (point) (length  (match-string 0))))
            (when (search-forward-regexp close-tag nil t)
              (setq end (point))
              (backward-char (length (match-string 0)))
              (backward-sexp)
              (setq sexp (substring-no-properties (thing-at-point 'sexp)))
              ;; In some exporters (pandoc) " are replaced with &quot; which
              ;; breaks lisp interpolation.
              (with-temp-buffer
                (insert sexp)
                (goto-char (point-min))
                (while (search-forward "&quot;" nil t)
                  (replace-match "\"" nil t))
                (setq sexp (buffer-string)))
              (narrow-to-region beg end)
              (delete-region (point-min) (point-max))
              (insert
               (save-match-data
                 (condition-case err
                     (let ((object (eval (read sexp))))
                       (cond
                        ;; result is a string
                        ((stringp object) object)
                        ;; a list
                        ((and (listp object)
                              (not (eq object nil)))
                         (let ((string (pp-to-string object)))
                           (substring string 0 (1- (length string)))))
                        ;; a number
                        ((numberp object)
                         (number-to-string object))
                        ;; nil
                        ((eq object nil) "")
                        ;; otherwise
                        (t (pp-to-string object))))
                   ;; error handler
                   (error
                    (format "Lisp error in %s: %s" (buffer-file-name) err)))))
              (goto-char (point-min))
              (widen))))))))

(defun org-blog--prepend-level (str root-dir)
  "Prepend the appropriate relative level for a file path:
level 0 -> ./
level 1 -> ../
level 2 -> ../../"
  (setq level (cdr (s-slice-at "../" (f-relative (f-slash root-dir) str))))
  (if (eq level nil)
      (setq level "./")
    (setq level (mapconcat 'identity level "")))
  (format "%s" level))

;;; Transcode functions

(defun org-blog--unmustachefy (info)
  "Return format specification for elements that can be
used in the preamble, postamble and body."
  (save-excursion
    (save-restriction
      (save-match-data
        (let (
              (title (org-export-data (plist-get info :title) info))
              (author (org-export-data (plist-get info :author) info))
              (email (org-export-data (plist-get info :email) info))
              (lang (plist-get info :blog-language))
              (title (org-export-data (plist-get info :title) info))
              (created (org-export-data (plist-get info :created) info))
              (modified (org-export-data (plist-get info :modified) info))
              (keywords (org-export-data (plist-get info :keywords) info))
              )
          (beginning-of-buffer)
          (while (re-search-forward "\{\{ title \}\}" nil t)
            (replace-match title))
          (beginning-of-buffer)
          (while (re-search-forward "\{\{ author \}\}" nil t)
            (replace-match author))
          (beginning-of-buffer)
          (while (re-search-forward "\{\{ email \}\}" nil t)
            (replace-match email))
          (beginning-of-buffer)
          (while (re-search-forward "\{\{ date \}\}" nil t)
            (replace-match date))
          (beginning-of-buffer)
          (while (re-search-forward "\{\{ validation-link \}\}" nil t)
            (replace-match org-blog-validation-link))
          (beginning-of-buffer)
          (while (re-search-forward "\{\{ modified \}\}" nil t)
            (replace-match modified))
          (beginning-of-buffer)
          (while (re-search-forward "\{\{ created \}\}" nil t)
            (replace-match created))
          (beginning-of-buffer)
          (while (re-search-forward "\{\{ language \}\}" nil t)
            (replace-match lang))
          (beginning-of-buffer)
          (while (re-search-forward "\{\{ charset \}\}" nil t)
            (replace-match (format "%s" org-blog-coding-system)))
          (beginning-of-buffer)
          (while (re-search-forward "\{\{ keywords \}\}" nil t)
            (replace-match keywords))
          )))))

(defun org-blog--get-pre/postamble (type info)
  "Return pre/postamble string.
CONTENTS is the transcoded contents string. INFO is a plist holding export options"
  (let ((section (plist-get info (intern (format ":blog-%s" type)))))
    (let ((section-contents ""))
      (when section
        (cond
         ((stringp section)
          (setq section-contents section contents))
         (t
          (setq section-contents (eval (intern (format "org-blog-%s-format" type)))))))
      (format "%s" section-contents))))

;;;; Template

(defun org-blog--build-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string. INFO is a plist holding export options"
  (let ((doc-contents "")) ;; Document string
    (let ((given-doc-template (plist-get info :blog-template))
          (root-directory (org-export-data (plist-get info :blog-root-directory) info))
          (head (concat (org-element-normalize-string (plist-get info :html-head))
                        (org-element-normalize-string (plist-get info :html-head-extra))))
          )
      (let ((doc-template ""))
        ;; Set correct template for page
        (when given-doc-template
          (cond
           ((stringp given-doc-template)
            (setq doc-template given-doc-template))
           (t
            (setq doc-template org-blog-template-format))))
        (setq doc-contents
              (replace-regexp-in-string "\{\{ contents \}\}" contents doc-template))
        (setq doc-contents
              (replace-regexp-in-string "\{\{ head \}\}" head doc-contents))
        (setq doc-contents
              (replace-regexp-in-string "\{\{ preamble \}\}" (org-blog--get-pre/postamble 'preamble info) doc-contents))
        (setq doc-contents
              (replace-regexp-in-string "\{\{ postamble \}\}" (org-blog--get-pre/postamble 'postamble info) doc-contents))
        (setq doc-contents
              (replace-regexp-in-string "\{\{ level \}\}" (org-blog--prepend-level (buffer-file-name) root-directory) doc-contents))
        (format "%s" doc-contents)))))

;;; Filter Functions

(defun org-blog--final-function (contents _backend info)
  "Filter to indent the HTML and convert HTML entities."
  (with-temp-buffer
    (insert contents)
    (set-auto-mode t)
    ;; Unmustachefy text
    (org-blog--unmustachefy info)
    ;; Eval lisp code in tags <lisp></lisp>
    (org-blog--eval-lisp)
    (if (plist-get info :blog-flush-lines)
        (flush-lines "^$" (point-min) (point-max)))
    (if (plist-get info :html-indent)
        (indent-region (point-min) (point-max)))

    (buffer-substring-no-properties (point-min) (point-max))))

;;; End-user functions

;;;###autoload
(defun org-blog-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a blog HTML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let* ((extension (concat "." org-blog-extension))
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system org-blog-coding-system))
    (org-export-to-file 'blog file
                        async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-blog-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.
Return output file name."
  (org-publish-org-to 'blog filename
                      (concat "." (or (plist-get plist :html-extension)
                                      org-blog-extension
                                      "html"))
                      plist pub-dir))

(provide 'ox-blog)
