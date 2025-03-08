;;; org-roam-gatsby --- Org Roam to Gatsby


;;; Commentary:


;;; Code:

(defvar e/org-roam-gatsby-tags-alist
  '(("~/.org-roam-public" "public" "/roam")
    ("~/.org-roam-public-nv" "public-nv" "/roam")))
(defvar e/org-export-gatsby-header nil)


(defun e/org-roam-gatsby-sites ()
  (let* (( file-name (buffer-file-name (buffer-base-buffer (current-buffer))))
         ( file-nondir (file-name-nondirectory file-name))
         ( filetags (split-string (or (cadar (org-collect-keywords '("FILETAGS"))) "")
                                  ":" 'OMIT-NULLS))
         link-name)
    (dolist ( tag e/org-roam-gatsby-tags-alist)
      (setq link-name (concat (expand-file-name (file-name-as-directory (car tag)))
                              file-nondir))
      (if (member (cadr tag) filetags)
          (make-symbolic-link file-name link-name t)
        (when (file-exists-p link-name)
          (delete-file link-name))))))


(defun e/org-gatsby-title-slug (title)
  (replace-regexp-in-string
   "-+" "-"
   (replace-regexp-in-string
    "[^a-zA-Z0-9\\-$_.+!*'(),]" "-"
    (downcase title))))


(defun e/org-roam-gatsby-site ()
  (let (( sites e/org-roam-gatsby-tags-alist)
        site)
    (while sites
      (setq site (pop sites))
      (if (string= (expand-file-name
                    (file-name-as-directory (car site)))
                   e/org-publish-base-directory)
          (setq sites nil)
        (setq site nil)))
    site))


(defun e/org-translate-link (link contents info)
  (let (( type (org-element-property :type link)))
    (when (string= type "id")
      (let* (( id (org-element-property :path link))
             ( description (substring-no-properties (nth 2 link)))
             ( node (car (org-roam-db-query
                          [:select [nodes:title nodes:file (funcall group-concat tags:tag nil)]
                                   :from nodes
                                   :left-join tags
                                   :on (= nodes:id tags:node-id)
                                   :where (= nodes:id $s1)]
                          id)))
             ( title (e/org-gatsby-title-slug (car node)))
             ( file (file-name-base (cadr node)))
             ( tags (cddr node))
             ( site (e/org-roam-gatsby-site))
             ( tag (cadr site))
             ( link-dir (file-name-as-directory (caddr site))))
        (if (member tag tags)
            (concat "<a href=\"" link-dir file "/" title "\">" description "</a>")
          description)))))


(defun e/org-translate-keyword (keyword contents info)
  (setq e/org-export-gatsby-header
        (cons `(,(downcase (org-element-property :key keyword)) .
                ,(org-element-property :value keyword))
              e/org-export-gatsby-header))
  nil)


(defun e/org-translate-inner-template (text info)
  (let* (( title (assoc "title" e/org-export-gatsby-header))
         ( slug (e/org-gatsby-title-slug (cdr title))))
    (setq e/org-export-gatsby-header
          (cons `("slug" . ,slug) e/org-export-gatsby-header)))
  (let (( header "<!-- GATSBY BEGIN\n"))
    (dolist ( line e/org-export-gatsby-header)
      (setq header (concat header "#+" (car line) ": " (cdr line) "\n")))
    (setq e/org-export-gatsby-header nil)
    (concat header "GATSBY END -->\n" text)))


(defun e/org-translate-property-drawer (property-drawer contents info)
  (message "%s" property-drawer))
 

(org-export-define-derived-backend 'html-gatsby 'html
  :translate-alist '((property-drawer . e/org-translate-property-drawer)
                     (link . e/org-translate-link)
                     (keyword . e/org-translate-keyword)
                     (inner-template . e/org-translate-inner-template)))

  
(defun e/org-html-publish-to-gatsby ( plist filename pub-dir)
  (setq e/org-publish-base-directory
        (expand-file-name (file-name-as-directory (plist-get plist :base-directory))))
  (org-publish-org-to 'html-gatsby
                      filename
                      (concat "." (or (plist-get plist :html-extension)
                                      org-html-extension
                                      "html"))
                      plist
                      pub-dir))



(provide 'org-roam-gatsby)


;;; org-roam-gatsby.el ends here
