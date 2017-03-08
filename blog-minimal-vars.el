;;; bm-vars.el -- Variable configurations required by blog-minimal

(require 'ht)

(defgroup blog-minimal nil
  "Easily create new blog post, with minimal theme."
  :tag "blog-minimal" :group 'org)


;;; easily config line start--------------------------------------------------
(defcustom blog-minimal-blog-main-dir "/home/thief/awe-website/thief-personal-blog/"
  "Your blog main dir"
  :type 'string
  :group 'blog-minimal)

(defcustom blog-minimal-package-dir  ""
   "Your package dir, like ***/blog-minimal"
  :type 'string
  :group 'blog-minimal)

(defcustom blog-minimal-mustache-templates-dir (concat blog-minimal-blog-main-dir "templates/")
  "A dir for mustache templates"
  :type 'string
  :group 'blog-minimal)

(defcustom blog-minimal-master-name "ThankFly"
  "It's your nickname! Please change it~"
  :type 'string
  :group 'blog-minimal)

(defcustom blog-minimal-avatar-path  "media/img/avatar4.png"
  "Specify path of avatar image."
  :type 'string
  :group 'blog-minimal)

(defcustom blog-minimal-github-link "https://github.com/thiefuniverse" "It's your github-link! Please change it~"
  :type 'string
  :group 'blog-minimal)

(defcustom blog-minimal-page-title "Thief's Valley"
  "Your page title"
  :type 'string
  :group 'blog-minimal)

(defcustom blog-minimal-author "thief"
  "Your name"
  :type 'string
  :group 'blog-minimal)

(defcustom blog-minimal-blog-email "thiefuniverses@gmail.com"
  "Your email"
  :type 'string
  :group 'blog-minimal)

(defcustom blog-minimal-description "Welcome to Thief's Valley!"
  "Your blog description"
  :type 'string
  :group 'blog-minimal)

(defcustom blog-minimal-blog-keywords "thiefuniverse flythief"
  "Your blog keywords"
  :type 'string
  :group 'blog-minimal)

(defcustom blog-minimal-blog-name "Thiefuniverse"
  "Your blog name"
  :type 'string
  :group 'blog-minimal)

(defcustom blog-minimal-one-cool-sentance "Do what you want to do, love what you love."
  "One cool sentance"
  :type 'string
  :group 'blog-minimal)

(defcustom blog-minimal-blog-info "Blog -- for My Beautiful Life!"
  "Your blog title"
  :type 'string
  :group 'blog-minimal)

(defvar blog-minimal-export-with-toc nil
  "Config for exporting with toc or not")

;;; if you want to change to disqus, you can change this var and blog-comment.mustache
(defcustom blog-minimal-duoshuoID "thiefuniverse"
  "Your duoshuo ID for your duoshuo comment"
  :type 'string
  :group 'blog-minimal)

;;; easily config line end --------------------------------------------------


;;; define vars for render mustache
(defconst blog-minimal-header-vars
  (ht
   ;;; for header
    ("page-title" blog-minimal-page-title)
    ("author" blog-minimal-author)
    ("description" blog-minimal-description)
    ("keywords" blog-minimal-blog-keywords)))

(defconst blog-minimal-person-zone-vars
  (ht
   ;;; for person-zone
   ("blog-name" blog-minimal-blog-name)
   ("master-name" blog-minimal-master-name)
   ("avatar" blog-minimal-avatar-path)    ; for avatar
   ("one-cool-sentance" blog-minimal-one-cool-sentance)))


(defconst blog-minimal-footer-vars
  (ht
   ;;;for footer
   ("master-name" blog-minimal-master-name)))

(defconst blog-minimal-nav-vars
  (ht
  ;;;for nav
   ("github-link" blog-minimal-github-link)
   ("personal-email" blog-minimal-blog-email)))

(defconst blog-minimal-content-title-vars
  (ht
   ;;; for content-title
   ))


(defconst blog-minimal-blog-index-vars
  (ht
    ;;;for blog-index
   ("blog-info" blog-minimal-blog-info)
   ))

(defconst blog-minimal-blog-comment-vars
  (ht
   ;;;for blog-comment
   ("bm_duoshuoID" blog-minimal-duoshuoID)
   ))

(defun blog-minimal-update-vars ()
  "update all vars for template."

  (setq blog-minimal-header-vars
	(ht
   ;;; for header
	 ("page-title" blog-minimal-page-title)
	 ("author" blog-minimal-author)
	 ("description" blog-minimal-description)
	 ("keywords" blog-minimal-blog-keywords)))

  (setq blog-minimal-person-zone-vars
	(ht
   ;;; for person-zone
	 ("blog-name" blog-minimal-blog-name)
	 ("master-name" blog-minimal-master-name)
	 ("avatar" blog-minimal-avatar-path)    ; for avatar
	 ("one-cool-sentance" blog-minimal-one-cool-sentance)))


  (setq blog-minimal-footer-vars
	(ht
   ;;;for footer
	 ("master-name" blog-minimal-master-name)))

  (setq blog-minimal-nav-vars
	(ht
  ;;;for nav
	 ("github-link" blog-minimal-github-link)
	 ("personal-email" blog-minimal-blog-email)))
  (setq blog-minimal-blog-comment-vars
	(ht
   ;;;for blog-comment
	 ("bm_duoshuoID" blog-minimal-duoshuoID)
	 ))
  (setq blog-minimal-blog-index-vars
	(ht
    ;;;for blog-index
	 ("blog-info" blog-minimal-blog-info)
	 )))


(defun blog-minimal-set-package-dir ()
  (setq blog-minimal-package-dir (file-name-directory (symbol-file 'blog-minimal-read-org-option))))

(defun blog-minimal-config-vars ()
  "Jump to blog-minimal vars files for configuration"
  (interactive)
  (find-file (symbol-file 'blog-minimal-set-package-dir)))

(provide 'blog-minimal-vars)
;;; bm-vars.el ends here
