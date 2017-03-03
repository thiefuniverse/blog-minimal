;;; blog-minimal.el --- a very simple static site generator based on org mode

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

;; see documentation at https://github.com/thiefunvierse/blog-minimal

;; you need add (require 'blog-minimal) to you .emacs file
;; then config your blog vars, execute (bm/init)


(require 'mustache)
(require 'ht)
(require 'simple-httpd)
(require 'bm-vars)
(require 'bm-utils)

;;; set templates dir
(push bm/mustache-templates-dir mustache-partial-paths)


(defun bm/init ()
  "init directory structure for blog minimal"
  (interactive)
  (bm/set-package-dir)
  
  (if (string= bm/blog-main-dir "")
      (message-box "bm/blog-main-dir is nil\n Please set a dir for your blog.")
    (progn
      (make-directory (concat bm/blog-main-dir "org") t)
      (copy-directory (concat bm/package-dir "/templates") bm/blog-main-dir)
      (copy-directory (concat bm/package-dir "/media") bm/blog-main-dir)
      (copy-file (concat bm/package-dir "/about.org") bm/blog-main-dir t)
      (bm/render-all-org-files))))


(defun bm/get-org-article-property (org-file-name)
  "get property (with title,uri,date) from org-file"
  (let ((pt-list (ht)))
    (with-temp-buffer
      (insert-file-contents org-file-name)
      (ht-set pt-list "title" (bm/read-org-option "title"))
      (ht-set pt-list "date" (bm/read-org-option "date"))

;;;    you can add other property for your org file      
      (ht-set pt-list "title-uri" (concat (bm/read-org-option "uri") ".html"))
;;;      (ht-set pt-list "tags" (split-string  (bm/read-org-option "tags") ","))
      (ht-set pt-list "keywords" (split-string  (bm/read-org-option "keywords") ",")))
    pt-list))


(defun bm/get--articles-property-for-index ()
  "get all articles properties for index"
  (let* ((articles-property ())
	 (org-prefix-dir (concat bm/blog-main-dir "org/"))
	(real-org-files (bm/real-org-files)))

    (dolist (real-org real-org-files)
      (push (bm/get-org-article-property (concat org-prefix-dir real-org))
	    articles-property))
    
    ;;; sort org index by date
    (sort articles-property 'bm/comparator-org-by-date)))


(defun bm/real-org-files ()
  "return real org files"
  (let ((org-files (directory-files (concat bm/blog-main-dir "org/")))
	(real-org-files ()))
    (dolist (org-file org-files)
      
      ;;; filter files whose name begin with . or # and end with ~
      (when (not (or (s-starts-with? "." org-file)
		     (s-starts-with? "#" org-file)
		     (s-ends-with? "~" org-file)))
	(push org-file real-org-files)))
    real-org-files))


(defun bm/render-all-org-files ()
  "render all org files"
  (interactive)
  (bm/update-vars)
  (bm/render-main-index)
  (bm/render-about)
  (bm/render-404)
  
  (let ((real-org-files (bm/real-org-files)))
    (dolist (real-orgf real-org-files)
      (find-file  (concat bm/blog-main-dir "org/" real-orgf))
      (bm/render-current-article)
      (save-buffer)
      (kill-buffer))))


(defun bm/update-index-vars (index-or-tags)
  "if INDEX-OR-TAGS is t, return article index \n
nil return tags index"
  (ht-set  bm/blog-index-vars "blog-info" bm/blog-info)

  (ht-remove bm/blog-index-vars "blog-index")
;;;  (ht-remove bm/blog-index-vars "blog-tags")
  
  (if index-or-tags
      (ht-set bm/blog-index-vars "blog-index" (bm/get--articles-property-for-index))
;;;    (ht-set bm/blog-index-vars "blog-tags" (bm/get--tags-property-for-index))
    ))



(defun bm/render-main-index ()
  "render main index for blog minimal"
  (ht-remove bm/header-vars "high-dir")
  (bm/update-index-vars t)

  (let ((main-vars
	  (ht-merge bm/header-vars bm/person-zone-vars bm/nav-vars bm/blog-index-vars bm/footer-vars)))
      (bm/string-to-file (mustache-render
		   "{{> main-index}}"
		   main-vars)
		      (concat bm/blog-main-dir "index.html"))))


(defun bm/update-self-info ()
  "transfer about.org to self-info.mustache"
  (let ((buffer-name (find-file (concat bm/blog-main-dir "about.org")))
	(current-html (bm/org-content-to-html-file)))
    (copy-file current-html (concat bm/mustache-templates-dir "self-info.mustache") t)
    (delete-file current-html)
    (save-buffer)
    (kill-buffer buffer-name)))


(defun bm/render-about ()
  "render about me for blog minimal"
  (ht-remove bm/header-vars "high-dir")
  (bm/update-self-info)
  (let ((about-vars
	  (ht-merge bm/header-vars bm/person-zone-vars bm/nav-vars bm/footer-vars)))
      (bm/string-to-file (mustache-render
		   "{{> about}}"
		   about-vars)
		      (concat bm/blog-main-dir "about.html"))))

(defun bm/render-404 ()
  "render 404 for blog minimal"
  (ht-remove bm/header-vars "high-dir")
  
  (let ((main-vars
	  (ht-merge bm/header-vars bm/person-zone-vars bm/nav-vars bm/footer-vars)))
      (bm/string-to-file (mustache-render
		   "{{> 404}}"
		   main-vars)
		      (concat bm/blog-main-dir "404.html"))))

;; TODO add tags index?  if you need it?
;; (defun bm/render-main-tags ()
;;   "render tags index for blog minimal"
;;   (ht-remove bm/header-vars "high-dir")
;;   (bm/update-index-vars nil)
  
;;   (let ((main-vars
;; 	  (ht-merge bm/header-vars bm/person-zone-vars bm/nav-vars bm/blog-index-vars)))
;;       (bm/string-to-file (mustache-render
;; 		   "{{> main-index}}"
;; 		   main-vars)
;; 			 (concat bm/blog-main-dir "about.html"))))


(defun bm/render-current-article ()
  "render blog content for blog minimal"
  (interactive)
  (bm/post-current-article)
  (ht-set bm/header-vars "high-dir" "../../../")
  
  (ht-set bm/content-title-vars "current-title" (bm/read-org-option "title"))
  (ht-set bm/content-title-vars "uri" (bm/read-org-option "uri"))
  
  (ht-set bm/content-title-vars "article-keywords"
	  (let ((all-keywords ()))
	    (dolist (keyword (split-string (bm/read-org-option "keywords") ","))
	      (push (ht ("keyword" keyword)) all-keywords))
	    all-keywords))

  (let ((blog-vars
	 (ht-merge bm/header-vars bm/content-title-vars bm/footer-vars bm/blog-comment-vars))
	(date-list (split-string (bm/read-org-option "date") "-") ))

    ;;; create date dirs for articles
    (bm/create-date-dir date-list)

    (bm/string-to-file (mustache-render
		     "{{> blog-article}}"
		     blog-vars)
		      ;;; write html to date dir
		    (concat bm/blog-main-dir (ht-get blog-vars "uri") ".html")))
  ;;; update main index
  (bm/render-main-index))


(defun bm/post-current-article ()
  "post current org file to blog minimal"
  (let ((current-html (bm/org-content-to-html-file)))
    (copy-file current-html (concat bm/mustache-templates-dir "blog-content.mustache") t)
    (delete-file current-html)))


(defun bm/create-new-article ()
  "create new article for blog minimal"
  (interactive)
  (let* ((title (read-string "Article Title: "))
	 (uri (read-string "Uri: ")) ;;; automatically add date
	 (real-uri (concat  (format-time-string "articles/%Y/%m/") uri))
;;;	 (tags (read-string "Tags(separated by comma and space) [,]): "))
	 (keywords (read-string "Keywords(separated by comma and space) [,]: "))
	 (file-name (concat (format-time-string "%Y-%m-%d-") uri ".org"))
	 (time-list (split-string (format-time-string "%Y-%m-%d") "-"))
	 (buffer-name (concat bm/blog-main-dir "org/" file-name)))

    (bm/create-date-dir time-list)
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
	     bm/author
	     bm/blog-email
	     (format-time-string "%Y-%m-%d")
	     real-uri
	     keywords
;;;	     tags
	     org-export-default-language
	     bm/export-with-toc))))


(defun bm/preview-blog ()
  "preivew blog minimal"
  (interactive)
  (httpd-serve-directory bm/blog-main-dir)
  (browse-url (format "http://%s:%d" "127.0.0.1" httpd-port)))

(provide 'blog-minimal)
;;; blog-minimal.el ends here
