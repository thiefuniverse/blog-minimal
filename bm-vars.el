;;; bm-vars.el -- Variable configurations required by blog-minimal

(require 'ht)

(defgroup blog-minimal nil
  "Easily create new blog post, with minimal theme."
  :tag "Org to html for blog with minimal theme." :group 'org)


;;; easily config line start--------------------------------------------------
(defconst bm/blog-main-dir "/home/thief/practice/website/")

(defconst bm/mustache-templates-dir (concat bm/blog-main-dir "templates/")
  "a dir for mustache templates")

(defconst bm/master-name "ThankFly"
  "It's your nickname! Please change it~")

(defconst bm/avatar-path  "media/img/avatar4.png"
  "specifify path of avatar image.")

(defconst bm/github-link "https://github.com/thiefuniverse" "It's your github-link! Please change it~")

(defconst bm/page-title "Thief's Valley")

(defconst bm/author "thief")

(defconst bm/blog-email "thiefuniverses@gmail.com")

(defconst bm/description "Welcome to Thief's Valley!")

(defconst bm/blog-keywords "thiefuniverse flythief")

(defconst bm/blog-name "Thiefuniverse")

(defconst bm/one-cool-sentance "Do what you want to do, love what you love.")

(defconst bm/blog-info "Blog -- for My Beautiful Life!")

(defvar bm/export-with-toc nil
  "config for exporting with toc or not")

;;; if you want to change to disqus, you can change this var and blog-comment.mustache
(defvar bm/duoshuoID "thiefuniverse"
  "your duoshuo ID")

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

(provide 'bm-vars)
;;; bm-vars.el ends here
