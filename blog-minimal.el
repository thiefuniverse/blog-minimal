;;; blog-minimal.el --- a very simple static site generator based on org mode

;; Package-Requires: ((ht "1.5") (simple-httpd "1.4.6") (mustache "0.22") (s "1.11.0"))

;;     This file is part of blog-minimal.

;;     Blog-minimal is free software: you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation, either version 3 of the License, or
;;     (at your option) any later version.

;;     Blog-minimal is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.

;;     You should have received a copy of the GNU General Public License
;;     along with Foobar.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Blog-minimal is a simple static site generator based on org mode.
;; It's so simple that others can hack it quickly and then create and
;; modify their own sites.

;; you need add (require 'blog-minimal) to you .emacs file
;; then config your blog-minimal-vars, execute (blog-minimal-init)


(require 'mustache)
(require 'ht)
(require 's)
(require 'simple-httpd)
(require 'blog-minimal-vars)
(require 'blog-minimal-utils)

;;; set templates dir
;;; Code:

(push blog-minimal-mustache-templates-dir mustache-partial-paths)


(defun blog-minimal-init ()
  "Init directory structure for blog minimal."
  (interactive)
  (blog-minimal-set-package-dir)
  
  (if (string= blog-minimal-blog-main-dir "")
      (message-box "blog-minimal-blog-main-dir is nil\n Please set a dir for your blog.")
    (progn
      (make-directory (concat blog-minimal-blog-main-dir "org") t)
      (copy-directory (concat blog-minimal-package-dir "/templates") blog-minimal-blog-main-dir)
      (copy-directory (concat blog-minimal-package-dir "/media") blog-minimal-blog-main-dir)
      (copy-file (concat blog-minimal-package-dir "/about.org") blog-minimal-blog-main-dir t)
      (blog-minimal-render-all-org-files))))


(defun blog-minimal-get-org-article-property (org-file-name)
  "ORG-FILE-NAME: org-file.
Get property (with title,uri,date) from org-file."
  (let ((pt-list (ht)))
    (with-temp-buffer
      (insert-file-contents org-file-name)
      (ht-set pt-list "title" (blog-minimal-read-org-option "title"))
      (ht-set pt-list "date" (blog-minimal-read-org-option "date"))

;;;    you can add other property for your org file.
      (ht-set pt-list "title-uri" (concat (blog-minimal-read-org-option "uri") ".html"))
;;;      (ht-set pt-list "tags" (split-string  (blog-minimal-read-org-option "tags") ","))
      (ht-set pt-list "keywords" (split-string  (blog-minimal-read-org-option "keywords") ",")))
    pt-list))


(defun blog-minimal-get--articles-property-for-index ()
  "Get all articles properties for index."
  (let* ((articles-property ())
	 (org-prefix-dir (concat blog-minimal-blog-main-dir "org/"))
	(real-org-files (blog-minimal-real-org-files)))

    (dolist (real-org real-org-files)
      (push (blog-minimal-get-org-article-property (concat org-prefix-dir real-org))
	    articles-property))
    
    ;;; sort org index by date
    (sort articles-property 'blog-minimal-comparator-org-by-date)))


(defun blog-minimal-real-org-files ()
  "Return real org files."
  (let ((org-files (directory-files (concat blog-minimal-blog-main-dir "org/")))
	(real-org-files ()))
    (dolist (org-file org-files)
      
      ;;; filter files whose name begin with . or # and end with ~
      (when (not (or (s-starts-with? "." org-file)
		     (s-starts-with? "#" org-file)
		     (s-ends-with? "~" org-file)))
	(push org-file real-org-files)))
    real-org-files))


(defun blog-minimal-render-all-org-files ()
  "Render all org files."
  (interactive)
  (blog-minimal-update-vars)
  (blog-minimal-render-main-index)
  (blog-minimal-render-about)
  (blog-minimal-render-404)
  
  (let ((real-org-files (blog-minimal-real-org-files)))
    (dolist (real-orgf real-org-files)
      (find-file  (concat blog-minimal-blog-main-dir "org/" real-orgf))
      (blog-minimal-render-current-article)
      (save-buffer)
      (kill-buffer))))


(defun blog-minimal-update-index-vars (index-or-tags)
  "If INDEX-OR-TAGS is t, return article index ; nil return tags index."
  (ht-set  blog-minimal-blog-index-vars "blog-info" blog-minimal-blog-info)

  (ht-remove blog-minimal-blog-index-vars "blog-index")
;;;  (ht-remove blog-minimal-blog-index-vars "blog-tags")
  
  (if index-or-tags
      (ht-set blog-minimal-blog-index-vars "blog-index" (blog-minimal-get--articles-property-for-index))
;;;    (ht-set blog-minimal-blog-index-vars "blog-tags" (blog-minimal-get--tags-property-for-index))
    ))



(defun blog-minimal-render-main-index ()
  "Render main index for blog minimal."
  (ht-remove blog-minimal-header-vars "high-dir")
  (blog-minimal-update-index-vars t)

  (let ((main-vars
	  (ht-merge blog-minimal-header-vars blog-minimal-person-zone-vars blog-minimal-nav-vars blog-minimal-blog-index-vars blog-minimal-footer-vars)))
      (blog-minimal-string-to-file (mustache-render
		   "{{> main-index}}"
		   main-vars)
		      (concat blog-minimal-blog-main-dir "index.html"))))


(defun blog-minimal-update-self-info ()
  "Transfer about.org to self-info.mustache."
  (let ((buffer-name (find-file (concat blog-minimal-blog-main-dir "about.org")))
	(current-html (blog-minimal-org-content-to-html-file)))
    (copy-file current-html (concat blog-minimal-mustache-templates-dir "self-info.mustache") t)
    (delete-file current-html)
    (save-buffer)
    (kill-buffer buffer-name)))


(defun blog-minimal-render-about ()
  "Render about me for blog minimal."
  (ht-remove blog-minimal-header-vars "high-dir")
  (blog-minimal-update-self-info)
  (let ((about-vars
	  (ht-merge blog-minimal-header-vars blog-minimal-person-zone-vars blog-minimal-nav-vars blog-minimal-footer-vars)))
      (blog-minimal-string-to-file (mustache-render
		   "{{> about}}"
		   about-vars)
		      (concat blog-minimal-blog-main-dir "about.html"))))

(defun blog-minimal-render-404 ()
  "Render 404 for blog minimal."
  (ht-remove blog-minimal-header-vars "high-dir")
  
  (let ((main-vars
	  (ht-merge blog-minimal-header-vars blog-minimal-person-zone-vars blog-minimal-nav-vars blog-minimal-footer-vars)))
      (blog-minimal-string-to-file (mustache-render
		   "{{> 404}}"
		   main-vars)
		      (concat blog-minimal-blog-main-dir "404.html"))))

;; TODO add tags index?  if you need it?
;; (defun blog-minimal-render-main-tags ()
;;   "render tags index for blog minimal"
;;   (ht-remove blog-minimal-header-vars "high-dir")
;;   (blog-minimal-update-index-vars nil)
  
;;   (let ((main-vars
;; 	  (ht-merge blog-minimal-header-vars blog-minimal-person-zone-vars blog-minimal-nav-vars blog-minimal-blog-index-vars)))
;;       (blog-minimal-string-to-file (mustache-render
;; 		   "{{> main-index}}"
;; 		   main-vars)
;; 			 (concat blog-minimal-blog-main-dir "about.html"))))


(defun blog-minimal-render-current-article ()
  "Render blog content for blog minimal."
  (interactive)
  (blog-minimal-post-current-article)
  (ht-set blog-minimal-header-vars "high-dir" "../../../")
  
  (ht-set blog-minimal-content-title-vars "current-title" (blog-minimal-read-org-option "title"))
  (ht-set blog-minimal-content-title-vars "uri" (blog-minimal-read-org-option "uri"))
  
  (ht-set blog-minimal-content-title-vars "article-keywords"
	  (let ((all-keywords ()))
	    (dolist (keyword (split-string (blog-minimal-read-org-option "keywords") ","))
	      (push (ht ("keyword" keyword)) all-keywords))
	    all-keywords))

  (let ((blog-vars
	 (ht-merge blog-minimal-header-vars blog-minimal-content-title-vars blog-minimal-footer-vars blog-minimal-blog-comment-vars))
	(date-list (split-string (blog-minimal-read-org-option "date") "-") ))

    ;;; create date dirs for articles
    (blog-minimal-create-date-dir date-list)

    (blog-minimal-string-to-file (mustache-render
		     "{{> blog-article}}"
		     blog-vars)
		      ;;; write html to date dir
		    (concat blog-minimal-blog-main-dir (ht-get blog-vars "uri") ".html")))
  ;;; update main index
  (blog-minimal-render-main-index))


(defun blog-minimal-post-current-article ()
  "Post current org file to blog minimal."
  (let ((current-html (blog-minimal-org-content-to-html-file)))
    (copy-file current-html (concat blog-minimal-mustache-templates-dir "blog-content.mustache") t)
    (delete-file current-html)))


(defun blog-minimal-create-new-article ()
  "Create new article for blog minimal."
  (interactive)
  (let* ((title (read-string "Article Title: "))
	 (uri (read-string "Uri: ")) ;;; automatically add date
	 (real-uri (concat  (format-time-string "articles/%Y/%m/") uri))
;;;	 (tags (read-string "Tags(separated by comma and space) [,]): "))
	 (keywords (read-string "Keywords(separated by comma and space) [,]: "))
	 (file-name (concat (format-time-string "%Y-%m-%d-") uri ".org"))
	 (time-list (split-string (format-time-string "%Y-%m-%d") "-"))
	 (buffer-name (concat blog-minimal-blog-main-dir "org/" file-name)))

    (blog-minimal-create-date-dir time-list)
    (switch-to-buffer (find-file buffer-name))
    (insert (format
	     "#+TITLE:       %s
#+AUTHOR:      %s
#+EMAIL:       %s
#+DATE:        %s
#+URI:         %s
#+KEYWORDS:    %s
#+LANGUAGE:    %s
#+OPTIONS:     toc:%s  num:nil \n\n\n
"
;;;#+TAGS:        %s
	     
	     title
	     blog-minimal-author
	     blog-minimal-blog-email
	     (format-time-string "%Y-%m-%d")
	     real-uri
	     keywords
;;;	     tags
	     org-export-default-language
	     blog-minimal-export-with-toc))))


(defun blog-minimal-preview-blog ()
  "Preivew blog minimal."
  (interactive)
  (httpd-serve-directory blog-minimal-blog-main-dir)
  (browse-url (format "http://%s:%d" "127.0.0.1" httpd-port)))

(provide 'blog-minimal)
;;; blog-minimal.el ends here
