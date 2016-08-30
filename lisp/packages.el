(require 'package)
(package-initialize)

(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
  '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
  '("org" . "http://orgmode.org/elpa/") t)

;; Fix HTTP1/1.1 problems
(setq url-http-attempt-keepalives nil)

(package-refresh-contents)

(package-install 's)
(package-install 'f)
(package-install 'dash)
(package-install 'mustache)
(package-install 'org)
(package-install 'org-plus-contrib)
(package-install 'use-package)
(package-install 'org-ref)
(package-install 'htmlize)
