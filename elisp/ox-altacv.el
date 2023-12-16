;;; ox-altacv.el --- AltaCV Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; Keywords: org, wp, tex

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library implements an AltaCV LaTeX back-end, derived from the
;; LaTeX one.

;;; Code:
(require 'ox-latex)

;; Install a default set-up for altacv export.
(unless (assoc "altacv" org-latex-classes)
  (add-to-list 'org-latex-classes
	       '("altacv"
		     "\\documentclass[10pt,a4paper,ragged2e,withhyper]{altacv}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]

\\geometry{left=1.25cm,right=1.25cm,top=1.25cm,bottom=1.25cm,columnsep=1cm}

\\usepackage{paracol}

\\ifxetexorluatex
  % If using xelatex or lualatex:
  \\setmainfont{Lato}
\\else
  % If using pdflatex:
  \\usepackage[default]{lato}
\\fi

\\definecolor{VividPurple}{HTML}{3E0097}
\\definecolor{SlateGrey}{HTML}{2E2E2E}
\\definecolor{LightGrey}{HTML}{666666}
\\colorlet{heading}{VividPurple}
\\colorlet{headingrule}{VividPurple}
\\colorlet{accent}{VividPurple}
\\colorlet{emphasis}{SlateGrey}
\\colorlet{body}{LightGrey}

\\renewcommand{\\cvItemMarker}{{\\small\\textbullet}}
\\renewcommand{\\cvRatingMarker}{\\faCircle}

\\usepackage{newfields}

[EXTRA]

"
		 ("\\cvevent{%s}" . "\\cvevent*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

;;; User-Configurable Variables

(defgroup org-export-altacv nil
  "Options specific for using the altacv class in LaTeX export."
  :tag "Org AltaCV"
  :group 'org-export
  :version "24.2")

;;; Define Back-End
(org-export-define-derived-backend 'altacv 'latex
  :menu-entry
  '(?l 1
       ((?A "As LaTeX buffer (AltaCV)" org-altacv-export-as-latex)
	(?a "As LaTeX file (AltaCV)" org-altacv-export-to-latex)
	(?P "As PDF file (AltaCV)" org-altacv-export-to-pdf)
	(?O "As PDF file and open (AltaCV)"
	    (lambda (a s v b)
	      (if a (org-altacv-export-to-pdf t s v b)
		(org-open-file (org-altacv-export-to-pdf nil s v b)))))))
  :options-alist
  '(
    (:latex-class "LATEX_CLASS" nil "altacv" t)
    (:tagline "TAGLINE" nil "" t)
    (:email "EMAIL" nil "" t)
    (:address "ADDRESS" nil nil t)
    (:github "GITHUB" nil "" t)
    (:gitlab "GITLAB" nil "" t)
    (:location "LOCATION" nil "" t)
    (:homepage "HOMEPAGE" nil "" t)
    (:linkedin "LINKEDIN" nil "" t)
    (:orcid "ORCID" nil "" t)
    (:phone "PHONE" nil "" t)
    (:photo "PHOTO" nil "" t)
    (:column_ratio "COLUMN_RATIO" nil "0.5" t)
    )
  :translate-alist '(
		             (headline . org-altacv-headline)
                     (template . org-altacv-template)
                     ))

(dolist (backend org-export-registered-backends)
  (if (eq 'altacv (org-export-backend-name backend))
    (setf (car (org-export-backend-menu backend))
          ?w)))

;;; Transcode Functions
(defun org-altacv--format-cvevent (headline contents info)
  (let* ((headline-title (org-element-property :title headline))
         (title (org-export-data headline-title info))
         (company (org-element-property :COMPANY headline))
         (from (org-element-property :FROM headline))
         (to (org-element-property :TO headline))
         (location (org-element-property :LOCATION headline)))
    (format "\\cvevent{%s}{%s}{%s -- %s}{%s}

%s

"
            title
            company 
            from 
            to 
            location
            contents)))

(defun org-altacv--format-cvachievement (headline contents info)
  (let* ((headline-title (org-element-property :title headline))
         (title (org-export-data headline-title info))
         (icon (org-element-property :ICON headline))
         (new-contents (replace-regexp-in-string
                        "\n$" "" 
                        contents)))
    (format "\\cvachievement{\\%s}{%s}{%s}"
          icon
          title
          new-contents)))

(defun org-altacv--format-cvskill (headline contents info)
  (let* ((headline-title (org-element-property :title headline))
         (title (org-export-data headline-title info))
         (skill-level (org-element-property :SKILL_LEVEL headline)))
         (format "\\cvskill{%s}{%s}\n"                  
                 title
                 skill-level)))

(defun org-altacv--format-cvref (headline contents info)
  (let* ((headline-title (org-element-property :title headline))
         (title (org-export-data headline-title info))
         (ref-institution (org-element-property :REF_INST headline))
         (ref-address (org-element-property :REF_ADDRESS headline))
         (ref-email (org-element-property :REF_EMAIL headline))
         (new-contents (if (org-string-nw-p contents)
                           (format "%s" contents) "")))
    (format "\\cvref{%s}{%s}{%s}{%s}

%s"                  
            title
            ref-institution
            ref-email
            ref-address
            new-contents)))

;;;; Headline
(defun org-altacv-headline (headline contents info)
  "Transcode HEADLINE element into AltaCV code.
CONTENTS is the contents of the headline.  INFO is a plist used
as a communication channel.

Depending on the tag of the headline it is considered a section, an event...."
  (let* ((level (org-export-get-relative-level headline info))
         (tags (org-element-property :tags headline))
         (headline-title (org-element-property :title headline))
         (title (org-export-data headline-title info))
         (entry-type (org-element-property :CVENTRY headline)))    
    (cond
     ((string= entry-type "cvsection")
      (concat (format "\n\\cvsection{%s}\n\n" title)
              contents))
     ((string= entry-type "cvevent")
      (org-altacv--format-cvevent headline contents info))
     ((string= entry-type "cvachievement")
      (org-altacv--format-cvachievement headline contents info))
     ((string= entry-type "cvskill")
      (org-altacv--format-cvskill headline contents info))
     ((string= entry-type "cvref")
      (org-altacv--format-cvref headline contents info))
     ((string= entry-type "cvtag")
      (format "\\cvtag{%s}%s" title (if contents contents "" )))
     (t (format "\\section{%s}\n" title)))))

(defun org-altacv-template (contents info)
  "Return complete document string after AltaCV conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Time-stamp   
   (format-time-string "%% Created %Y-%m-%d %a %H:%M\n")
   ;; Document class and packages.
   (org-latex-make-preamble info)
   "\n\\begin{document}\n"
   ;; Author
   (let ((author (and (plist-get info :with-author)
			(let ((auth (plist-get info :author)))
			  (and auth (org-export-data auth info))))))
     (format "\\name{%s}\n" author))
   ;; Tagline
   (let ((tagline (plist-get info :tagline)))
     (when tagline
       (format "  \\tagline{%s}\n" tagline)))
   ;; Photo
   (let ((photo (plist-get info :photo)))
     (when photo
       (format "\\photoR{2.5cm}{%s}\n" photo)))
   "\\personalinfo{%\n"
   ;; Email
   (let ((email (plist-get info :email)))
     (when email
       (format "  \\email{%s}\n" email)))
   ;; Address
   (let ((address (plist-get info :address)))
     (when address
       (format "  \\mailaddress{%s}\n" address)))
   ;; Location
   (let ((location (plist-get info :location)))
     (when location
       (format "  \\location{%s}\n" location)))
   ;; LinkedIn
   (let ((linkedin (plist-get info :linkedin)))
     (when linkedin
       (format "  \\linkedin{%s}\n" linkedin)))
   ;; Homepage   
   (let ((homepage (plist-get info :homepage)))
     (when homepage
       (format "  \\homepage{%s}\n" homepage)))
   ;; GitHub
   (let ((gh-uname (plist-get info :github)))
     (when gh-uname
       (format "  \\github{github.com/%s}\n" gh-uname)))
   ;; GitLab
   (let ((gl-uname (plist-get info :gitlab)))
     (when gl-uname
       (format "  \\gitlab{gitlab.com/%s}\n" gl-uname)))
   ;; OrcID
   (let ((orcid (plist-get info :orcid)))
     (when orcid
       (format "  \\orcid{%s}\n" orcid)))
   ;; Phone
   (let ((phone (plist-get info :phone)))
     (when phone
       (format "  \\phone{%s}\n" phone)))
   "}\n"
   "\\makecvheader\n"
   "\\AtBeginEnvironment{itemize}{\\small}"
   ;; Column ratio
   (let ((column-ratio (plist-get info :column_ratio)))
     (format "\\columnratio{%s}" column-ratio))
   contents
   "\\end{document}"))

;;; Commands

;;;###autoload
(defun org-altacv-export-as-latex
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a AltaCV buffer.
If narrowing is active in the current buffer, only export its
narrowed part.
If a region is active, export that region.
A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.
When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.
When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.
When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".
EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.
Export is done in a buffer named \"*Org AltaCV Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'altacv "*Org AltaCV Export*"
    async subtreep visible-only body-only ext-plist (lambda () (LaTeX-mode))))

;;;###autoload
(defun org-altacv-export-to-latex
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a AltaCV document (tex).
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
between \"\\begin{document}\" and \"\\end{document}\".
EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.
Return output file's name."
  (interactive)
  (let ((file (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'altacv file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-altacv-export-to-pdf
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a AltaCV document (PDF).
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
between \"\\begin{document}\" and \"\\end{document}\".
EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.
Return PDF file's name."
  (interactive)
  (let ((file (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'altacv file
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-latex-compile file)))))

;;;###autoload
(defun org-altacv-publish-to-latex (plist filename pub-dir)
  "Publish an Org file to a AltaCV document (LaTeX).
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.
Return output file name."
  (org-publish-org-to 'altacv filename ".tex" plist pub-dir))

;;;###autoload
(defun org-altacv-publish-to-pdf (plist filename pub-dir)
  "Publish an Org file to a AltaCV document (PDF, via LaTeX).
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.
Return output file name."
  ;; Unlike to `org-altacv-publish-to-latex', PDF file is generated in
  ;; working directory and then moved to publishing directory.
  (org-publish-attachment
   plist
   ;; Default directory could be anywhere when this function is
   ;; called.  We ensure it is set to source file directory during
   ;; compilation so as to not break links to external documents.
   (let ((default-directory (file-name-directory filename)))
     (org-latex-compile
      (org-publish-org-to
       'altacv filename ".tex" plist (file-name-directory filename))))
   pub-dir))

(provide 'ox-altacv)
;;; ox-altacv.el ends here
