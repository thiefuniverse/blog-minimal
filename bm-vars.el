;;; bm-vars.el -- Variable configurations required by blog-minimal

(require 'ht)

(defgroup blog-minimal nil
  "Easily create new blog post, with minimal theme."
  :tag "blog-minimal" :group 'org)


;;; easily config line start--------------------------------------------------
(defcustom bm/blog-main-dir "/home/thief/practice/org/test_blog/"
  "Your blog main dir"
  :type 'string
  :group 'blog-minimal)

(defcustom bm/package-dir  ""
   "Your package dir, like ***/blog-minimal"
  :type 'string
  :group 'blog-minimal)

(defcustom bm/mustache-templates-dir (concat bm/blog-main-dir "templates/")
  "a dir for mustache templates"
  :type 'string
  :group 'blog-minimal)

(defcustom bm/master-name "ThankFly"
  "It's your nickname! Please change it~"
  :type 'string
  :group 'blog-minimal)

(defcustom bm/avatar-path  "media/img/avatar4.png"
  "specify path of avatar image."
  :type 'string
  :group 'blog-minimal)

(defcustom bm/github-link "https://github.com/thiefuniverse" "It's your github-link! Please change it~"
  :type 'string
  :group 'blog-minimal)

(defcustom bm/page-title "Thief's Valley"
  "your page title"
  :type 'string
  :group 'blog-minimal)

(defcustom bm/author "thief"
  "your name"
  :type 'string
  :group 'blog-minimal)

(defcustom bm/blog-email "thiefuniverses@gmail.com"
  "your email"
  :type 'string
  :group 'blog-minimal)

(defcustom bm/description "Welcome to Thief's Valley!"
  "your blog description"
  :type 'string
  :group 'blog-minimal)

(defcustom bm/blog-keywords "thiefuniverse flythief"
  "your blog keywords"
  :type 'string
  :group 'blog-minimal)

(defcustom bm/blog-name "Thiefuniverse"
  "your blog name"
  :type 'string
  :group 'blog-minimal)

(defcustom bm/one-cool-sentance "Do what you want to do, love what you love."
  "one cool sentance"
  :type 'string
  :group 'blog-minimal)

(defcustom bm/blog-info "Blog -- for My Beautiful Life!"
  "Your blog title"
  :type 'string
  :group 'blog-minimal)

(defvar bm/export-with-toc nil
  "config for exporting with toc or not")

;;; if you want to change to disqus, you can change this var and blog-comment.mustache
(defcustom bm/duoshuoID "thiefuniverse"
  "your duoshuo ID for your duoshuo comment"
  :type 'string
  :group 'blog-minimal)

;;; easily config line end --------------------------------------------------


;;; define vars for render mustache
(defconst bm/header-vars
  (ht
   ;;; for header
    ("page-title" bm/page-title)
    ("author" bm/author)
    ("description" bm/description)
    ("keywords" bm/blog-keywords)))

(defconst bm/person-zone-vars
  (ht
   ;;; for person-zone
   ("blog-name" bm/blog-name)
   ("master-name" bm/master-name)
   ("avatar" bm/avatar-path)    ; for avatar
   ("one-cool-sentance" bm/one-cool-sentance)))


(defconst bm/footer-vars
  (ht
   ;;;for footer
   ("master-name" bm/master-name)))

(defconst bm/nav-vars
  (ht
  ;;;for nav
   ("github-link" bm/github-link)
   ("personal-email" bm/blog-email)))

(defconst bm/content-title-vars
  (ht
   ;;; for content-title
   ))


(defconst bm/blog-index-vars
  (ht
    ;;;for blog-index
   ))

(defconst bm/blog-comment-vars
  (ht
   ;;;for blog-comment
   ("bm_duoshuoID" bm/duoshuoID)
   ))

(defun bm/update-vars ()
  "update all vars for template."

  (setq bm/header-vars
	(ht
   ;;; for header
	 ("page-title" bm/page-title)
	 ("author" bm/author)
	 ("description" bm/description)
	 ("keywords" bm/blog-keywords)))

  (setq bm/person-zone-vars
	(ht
   ;;; for person-zone
	 ("blog-name" bm/blog-name)
	 ("master-name" bm/master-name)
	 ("avatar" bm/avatar-path)    ; for avatar
	 ("one-cool-sentance" bm/one-cool-sentance)))


  (setq bm/footer-vars
	(ht
   ;;;for footer
	 ("master-name" bm/master-name)))

  (setq bm/nav-vars
	(ht
  ;;;for nav
	 ("github-link" bm/github-link)
	 ("personal-email" bm/blog-email)))
  (setq bm/blog-comment-vars
	(ht
   ;;;for blog-comment
	 ("bm_duoshuoID" bm/duoshuoID)
	 )))


(defun bm/set-package-dir ()
  (setq bm/package-dir (file-name-directory (symbol-file 'bm/read-org-option))))

(defun bm/config-vars ()
  "jump to blog-minimal vars files for configuration"
  (interactive)
  (find-file (symbol-file 'bm/set-package-dir)))

(provide 'bm-vars)
;;; bm-vars.el ends here
