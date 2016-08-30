;; publish.mo.el --- Publish website using ox-blog
;; Author: Victor Santos (victor_santos@fisica.ufc.br)

;; Set relevant PATH's. You can change {{root-dir}} by the base folder of the
;; website; otherwise it is set by an external bash command
(setq blog-base-dir "{{root-dir}}")
(setq blog-pub-dir (concat blog-base-dir "public_html/"))
(setq blog-lisp-dir (concat blog-base-dir "lisp/"))
(setq blog-template-dir (concat blog-base-dir "templates/"))

;; Load packages
;(require 'package)
(package-initialize)
(use-package s)
(use-package f)
(use-package ox-blog
  :load-path blog-lisp-dir)

;; Main configuration
(setq org-publish-project-alist
      `(("www-index"
         :blog-root-directory ,(concat blog-base-dir)
         :base-directory ,(concat blog-base-dir)
         :blog-template ,(f-read-text (s-concat blog-template-dir "default.mo.html"))
         :blog-preamble ,(f-read-text (s-concat blog-template-dir "preamble.mo.html"))
         :blog-postamble ,(f-read-text (s-concat blog-template-dir "postamble.mo.html"))
         :base-extension "org"
         :html-head ,(s-concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"{{ level }}static/css/style.css\">\n")
         :html-indent t
         :blog-flush-lines t
         :exclude ".*"
         :include ["index.org"]
         :publishing-directory ,(concat blog-pub-dir)
         :publishing-function org-blog-publish-to-html)
        ("www-static"
         :blog-root-directory ,(concat blog-base-dir)
         :base-directory ,(concat blog-base-dir "static/")
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|map\\|eot\\|svg\\|ttf\\|woff\\|woff2"
         :publishing-directory ,(concat blog-pub-dir "static/")
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("www" :components ("www-static" "www-index"))))

;; Export project
(defun export (&optional force)
  "Publish the project."
  (let ((force (or force t)))
    (org-publish-project "www" force)))

(defun export-pdf (&optional force)
  "Publish the project."
  (let ((force (or force t)))
    (org-latex-export-to-pdf)))
