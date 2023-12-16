(require 'ox)
(require 'ox-altacv)

(add-to-list 'org-latex-logfiles-extensions "tex")
(add-to-list 'org-latex-logfiles-extensions "bbl")
(add-to-list 'org-latex-logfiles-extensions "xmpi")

(setq this-directory (file-name-directory (or load-file-name buffer-file-name)))
(setq project-directory (file-name-directory (directory-file-name this-directory)))
(setq pub-directory (concat project-directory "public/"))

(setenv "TEXMFHOME" (concat project-directory "texmf"))

(unless (boundp 'org-publish-project-alist)
  (setq org-publish-project-alist nil))

(setq org-publish-timestamp-directory "/tmp/org-timestamps/")

(add-to-list
 'org-publish-project-alist
 `("pdf"
   :base-directory ,project-directory
   :base-extension "org"
   :publishing-directory ,this-directory
   :publishing-function org-altacv-publish-to-pdf
   :completion-function ,(lambda (plist) (delete-file (concat this-directory "pdfa.xmpi")))))

(defun org-publish-pdf ()
  (org-publish-project "pdf" t))
