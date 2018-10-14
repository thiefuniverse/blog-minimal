;;; blog-minimal-vars.el -- Variable configurations required by blog-minimal


;;; Commentary:
;; define vars for blog-minimal

;;; Code:

(require 'ht)
(require 'json)

(defgroup blog-minimal nil
  "Easily create new blog post, with minimal theme."
  :tag "blog-minimal" :group 'org)

;;;------------------------------------------------------------
;;; load config file
;;;------------------------------------------------------------
(defcustom  blog-minimal-config-file (concat user-emacs-directory ".blog_minimal.config")
  "Your config file."
  :type 'string
  :group 'blog-minimal)

(defvar blog-minimal-config-vars (let ((json-object-type 'hash-table))
				   (json-read-file blog-minimal-config-file))
  "Load all config options.")



;;; easily config line start--------------------------------------------------
(defcustom blog-minimal-blog-main-dir (ht-get blog-minimal-config-vars "main_dir")
  "Your blog main dir."
  :type 'string
  :group 'blog-minimal
  :get  '(ht-get blog-minimal-config-vars "main_dir"))

(defcustom blog-minimal-package-dir (ht-get blog-minimal-config-vars "package_dir")
   "Your package dir, like ***/blog-minimal."
  :type 'string
  :group 'blog-minimal)

(defcustom blog-minimal-mustache-templates-dir (concat blog-minimal-blog-main-dir "templates/")
  "A dir for mustache templates."
  :type 'string
  :group 'blog-minimal)

(defcustom blog-minimal-master-name (ht-get blog-minimal-config-vars "master_name")
  "It's your nickname! Please change it~."
  :type 'string
  :group 'blog-minimal)

(defcustom blog-minimal-avatar-path (ht-get blog-minimal-config-vars "avatar_path")
  "Specify path of avatar image."
  :type 'string
  :group 'blog-minimal)

(defcustom blog-minimal-github-link (ht-get blog-minimal-config-vars "github_link")
  "It's your github-link! Please change it~."
  :type 'string
  :group 'blog-minimal)

(defcustom blog-minimal-page-title (ht-get blog-minimal-config-vars "page_title")
  "Your page title."
  :type 'string
  :group 'blog-minimal)

(defcustom blog-minimal-author (ht-get blog-minimal-config-vars "author")
  "Your name."
  :type 'string
  :group 'blog-minimal)

(defcustom blog-minimal-blog-email (ht-get blog-minimal-config-vars "blog_email")
  "Your email."
  :type 'string
  :group 'blog-minimal)

(defcustom blog-minimal-description (ht-get blog-minimal-config-vars "description")
  "Your blog description."
  :type 'string
  :group 'blog-minimal)

(defcustom blog-minimal-blog-keywords (ht-get blog-minimal-config-vars "blog_keywords")
  "Your blog keywords."
  :type 'string
  :group 'blog-minimal)

(defcustom blog-minimal-blog-name (ht-get blog-minimal-config-vars "blog_name")
  "Your blog name."
  :type 'string
  :group 'blog-minimal)

(defcustom blog-minimal-one-cool-sentance (ht-get blog-minimal-config-vars "cool_sentance")
  "One cool sentance."
  :type 'string
  :group 'blog-minimal)

(defcustom blog-minimal-blog-info (ht-get blog-minimal-config-vars "blog_info")
  "Your blog title."
  :type 'string
  :group 'blog-minimal)

(defun update-config-info ()
  "Update config info with user's config file."
  (setq blog-minimal-config-vars (let ((json-object-type 'hash-table))
				   (json-read-file blog-minimal-config-file))
	)
  (setq blog-minimal-blog-info (ht-get blog-minimal-config-vars "blog_info"))
  (setq blog-minimal-one-cool-sentance (ht-get blog-minimal-config-vars "cool_sentance"))
  (setq blog-minimal-blog-name (ht-get blog-minimal-config-vars "blog_name"))
  (setq blog-minimal-blog-keywords (ht-get blog-minimal-config-vars "blog_keywords"))
  (setq blog-minimal-description (ht-get blog-minimal-config-vars "description"))
  (setq blog-minimal-blog-email (ht-get blog-minimal-config-vars "blog_email"))
  (setq blog-minimal-author (ht-get blog-minimal-config-vars "author"))
  (setq blog-minimal-page-title (ht-get blog-minimal-config-vars "page_title"))
  (setq blog-minimal-github-link (ht-get blog-minimal-config-vars "github_link"))
  (setq blog-minimal-avatar-path (ht-get blog-minimal-config-vars "avatar_path"))
  (setq blog-minimal-master-name (ht-get blog-minimal-config-vars "master_name"))
  (setq blog-minimal-package-dir (ht-get blog-minimal-config-vars "package_dir"))
  
  (setq blog-minimal-blog-main-dir (ht-get blog-minimal-config-vars "main_dir"))
  (if (not (string-suffix-p "/" blog-minimal-blog-main-dir))
      (setq blog-minimal-blog-main-dir (concat blog-minimal-blog-main-dir "/"))))
(defvar blog-minimal-export-with-toc nil
  "Config for exporting with toc or not.")

;;; if you want to change to disqus, you can change this var and blog-comment.mustache
(defcustom blog-minimal-disqusID (ht-get blog-minimal-config-vars "disqus_ID")
  "Your disqus ID for your disqus comment."
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
   ("bm_disqusID" blog-minimal-disqusID)
   ))

(defun blog-minimal-update-vars ()
  "Update all vars for template."

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
	 ("bm_disqusID" blog-minimal-disqusID)
	 ))
  (setq blog-minimal-blog-index-vars
	(ht
	 ;;; for blog-index
	 ("blog-info" blog-minimal-blog-info)
	 )))


(defun blog-minimal-set-package-dir ()
  "Set package dir."
  (setq blog-minimal-package-dir (file-name-directory (symbol-file 'blog-minimal-read-org-option))))

(defun blog-minimal-config-vars ()
  "Jump to blog-minimal vars files for configuration."
  (interactive)
  (let ((edit-buffer (current-buffer))
	(config-buffer (generate-new-buffer "config-blog"))
	(mini-map (make-sparse-keymap)))
    ;; init config buffer
    (save-current-buffer
      (set-buffer config-buffer)
      (if (not (file-exists-p blog-minimal-config-file))
	  (copy-file "./.blog_minimal.config" blog-minimal-config-file))
      (insert-file-contents blog-minimal-config-file)
      )
    ;; switch config buffer and edit
    (switch-to-buffer config-buffer)
    (json-mode)
    (message-box "please config your website info! Happy Writing!")
    (define-key mini-map (kbd "C-c '") 'blog-minimal-commit-config)
    (use-local-map mini-map)))

(defun blog-minimal-commit-config ()
  "Commit user's config after editing."
  (interactive)
  (write-region (point-min) (point-max) blog-minimal-config-file)
  (update-config-info)
  (kill-buffer)
  (message-box "Congratulations ! Config your website done!"))
(provide 'blog-minimal-vars)
;;; blog-minimal-vars.el ends here
