;;; bm-utils.el --- Utillity functions required by blog-minimal

(require 'ht)
(require 'bm-vars)


(defun bm/read-org-option (option)
  "Read option value of org file opened in current buffer.
e.g:
#+TITLE: this is title
will return \"this is title\" if OPTION is \"TITLE\""
  (let ((match-regexp (org-make-options-regexp `(,option))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward match-regexp nil t)
        (match-string-no-properties 2 nil)))))


;;; copy from org-page:: op-util.el
;;; with small changes
(defun bm/string-to-file (string file &optional mode)
  "Write STRING into FILE, only when FILE is writable. If MODE is a valid major
mode, format the string with MODE's format settings."
  (with-temp-buffer
    (insert string)
    (save-excursion
      ;;;    (set-buffer-file-coding-system 'utf-8-unix)
      (when (and mode (functionp mode))
	(funcall mode)
	(flush-lines "^[ \\t]*$" (point-min) (point-max))
	(delete-trailing-whitespace (point-min) (point-max))
	(indent-region (point-min) (point-max)))
      (setq buffer-file-coding-system 'raw-text)
      (when (file-writable-p file)
	(write-region (point-min) (point-max) file)))))



(defun bm/comparator-date-string (Adate Bdate)
  "comparator of two date list"
  (cond ((< (string-to-number (first Adate))
	    (string-to-number (first Bdate)))
	 nil)
	((>  (string-to-number (first Adate))
	     (string-to-number (first Bdate)))
	 t)
	((=  (string-to-number (first Adate))
	     (string-to-number (first Bdate)))
	 (cond ((< (string-to-number (second Adate))
		   (string-to-number (second Bdate)))
		nil)
	       ((>  (string-to-number (second Adate))
		    (string-to-number (second Bdate)))
		t)
	       ((=  (string-to-number (second Adate))
		    (string-to-number (second Bdate)))
		(cond ((< (string-to-number (third Adate))
			  (string-to-number (third Bdate)))
		       nil)
		      ((>=  (string-to-number (third Adate))
			    (string-to-number (third Bdate)))
		       t)))))))


(defun bm/comparator-org-by-date (Aorg Borg)
  "comparator for org files' sort"
  (let ((Adate  (split-string (ht-get Aorg "date") "-"))
	(Bdate (split-string (ht-get Borg "date") "-")))
    (bm/comparator-date-string Adate Bdate)))

(defun bm/process-html-by-regexp ()
  "process html file for dirty output by org-html-export-to-html"
  (beginning-of-buffer)
  (replace-regexp "div id=\"outline-container-[a-z0-9]*\"" "div id=\"outline-container-thief\"")
  (beginning-of-buffer)
  (replace-regexp " id=\"org[a-z0-9]*\"" " id=\"org-thief\"")
  (beginning-of-buffer)
  (replace-regexp " id=\"text-org[a-z0-9]*\">" " id=\"text-org-thief\">")  )

(defun bm/org-content-to-html-file ()
  ;;; TODO make it can config file name
  "transfer org file into html file, with only body."
  (let ((export-file-name (org-html-export-to-html nil nil nil t )))
    (with-temp-buffer
      (insert-file-contents export-file-name)
      (bm/process-html-by-regexp)
      (write-file export-file-name)
      export-file-name
      )))

(defun bm/create-date-dir (time-list)
  "create dir for blog articles by date
   for example, date: 2018 05, then create dir 2018/05/ within dir /articles
   time-list is (\"2018\" \"05\")
   "
  (make-directory (concat bm/blog-main-dir "articles/" (first time-list) "/" (second time-list)) t))


(provide 'bm-utils)
;;; bm-utils.el ends here
