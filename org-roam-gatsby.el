;;; org-roam-gatsby --- Org Roam to Gatsby  -*- lexical-binding: t; -*-


;;; Commentary:


;;; Code:
(require 'org)
(require 'org-roam)
(require 'ox)
(require 'org-roam-notation)

(defvar e/org-roam-gatsby-plist nil)

(defvar e/org-export-gatsby-header nil)

(defvar e/org-roam-gatsby-hook nil)

(defvar e/org-roam-gatsby-projects-info nil)

(defun e/org-roam-gatsby-tag-nodes ( tag)
  (mapcar (lambda (l) (org-roam-node-from-id (car l)))
          (org-roam-db-query
           [:select [node-id]
                    :from tags
                    :where (like tag $r1)]
           (concat "%\"" tag "\"%"))))

(defun e/org-roam-gatsby-plist-find ( tag)
  (let (( plists (mapcar 'cdr org-publish-project-alist))
        plist found)
    (while plists
      (setq plist (pop plists))
      (when (string= (plist-get plist :tag) tag)
        (setq plists nil found t)))
    (if found plist nil)))

(defun org-roam-gatsby-backlink-nodes ( node tag)
  "Return all backlinks from project parent nodes (goals).

NODE is a valid org-roam node.  Returned superior node list has a
depth of 1."
  (let ( nodes ids)
    (dolist ( backlink (org-roam-backlinks-get node))
      (let* (( bl-node (org-roam-backlink-source-node backlink))
             ( bl-id (org-roam-node-id bl-node)))
        (when (and (not (member bl-id ids))
                   (member tag (org-roam-node-tags bl-node)))
          (setq ids (cons bl-id ids)
                nodes (cons bl-node nodes)))))
    nodes))

(defun e/org-roam-gatsby-symlink-after-save ()
  "Maintain symbolic links from roam directory to project base-directory.

Works with the current buffer."
  (let* (( file-name (buffer-file-name (buffer-base-buffer (current-buffer))))
         ( file-nondir (file-name-nondirectory file-name))
         ( filetags (split-string (or (cadar (org-collect-keywords '("FILETAGS"))) "")
                                  ":" 'OMIT-NULLS)))
    (dolist ( project org-publish-project-alist)
      (let* (( project-name (car project))
             ( plist (cdr project))
             ( tag (plist-get plist :tag)))
        (when tag
          (let* (( base-directory (file-name-as-directory
                                  (plist-get plist :base-directory)))
                 ( symbolic-link-name (expand-file-name
                                       file-nondir base-directory)))
            (cond ((member tag filetags)
                   (setq e/org-roam-gatsby-projects-info
                         (cons (list project-name tag base-directory)
                               e/org-roam-gatsby-projects-info))
                   (make-symbolic-link file-name symbolic-link-name t))
                  ((file-exists-p symbolic-link-name)
                   (setq e/org-roam-gatsby-projects-info
                         (cons (list project-name tag base-directory)
                               e/org-roam-gatsby-projects-info))
                   (delete-file symbolic-link-name)))))))))

(defun e/org-roam-gatsby-backlinks-after-save ()
  (let (( node (org-roam-node-at-point)))
    (dolist ( project e/org-roam-gatsby-projects-info)
      (let (( tag (cadr project))
            ( base-directory (caddr project)))
        (dolist ( bl-node (org-roam-gatsby-backlink-nodes node tag))
          (let* (( file-name (org-roam-node-file bl-node))
                 ( file-nondir (file-name-nondirectory file-name))
                 ( symbolic-link-name (expand-file-name
                                       file-nondir base-directory)))
            (set-file-times symbolic-link-name)))))))

(defun e/org-roam-gatsby-publish-all ()
  (interactive)
  (dolist ( project org-publish-project-alist)
    (let* (( project-name (car project))
           ( plist (cdr project))
           ( tag (plist-get plist :tag)))
      (when tag
        (let (( base-directory (expand-file-name
                                (file-name-as-directory
                                (plist-get plist :base-directory)))))
          (dolist ( file (directory-files base-directory 'full-name))
            (set-file-times file)))
        (org-publish-project project-name)))))

(defun e/org-roam-gatsby-publish-after-save ()
  (let (( projects e/org-roam-gatsby-projects-info))
    (setq e/org-roam-gatsby-projects-info nil)
    (dolist ( project projects)
      (message "--- Org-Roam-Gatsby: Publishing project \"%s\" ---" (car project))
      (org-publish-project (car project)))))

(defun e/org-roam-gatsby-hook-functions ()
  "Gatsby hook functions."
  (add-hook 'after-save-hook #'e/org-roam-gatsby-symlink-after-save 0 'local)
  (add-hook 'after-save-hook #'e/org-roam-gatsby-backlinks-after-save 5 'local)
  (add-hook 'after-save-hook #'e/org-roam-gatsby-publish-after-save 10 'local))

(add-hook 'e/org-roam-gatsby-hook 'e/org-roam-gatsby-hook-functions)

(defun e/org-roam-gatsby-run-hook ()
  (when (org-roam-file-p)
    (run-hooks 'e/org-roam-gatsby-hook)))

(add-hook 'org-mode-hook 'e/org-roam-gatsby-run-hook)

(defun e/org-gatsby-title-slug ( title)
  (replace-regexp-in-string
   "-+" "-"
   (replace-regexp-in-string
    "[^a-zA-Z0-9\\-$_.+!*'(),]" "-"
    (downcase title))))

(defun e/org-translate-link ( link _ _)
  (let (( type (org-element-property :type link))
        ( path (org-element-property :path link))
        ( plist e/org-roam-gatsby-plist))
    (cond ((string= type "id")
           (let* (( id path)
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
                  ( tags (cddr node)))
             (if (member (plist-get plist :tag) tags)
                 (concat "<a href=\""
                         (file-name-as-directory (plist-get plist :rel-url))
                         file "/" title "\">" description "</a>")
               description)))
          ((and (string= type "file") (string-match "images" path))
           (let* (( abs-path (expand-file-name path org-roam-directory))
                  ( name (file-name-nondirectory abs-path))
                  ( target-path (concat (file-name-as-directory
                                         (plist-get plist :image-directory))
                                        name))
                  ( rel-path (concat "/"
                                     (cadr (string-split
                                            target-path
                                            (file-name-as-directory "static"))))))
             (copy-file abs-path target-path 'OK-IF-ALREADY-EXISTS)
             (concat "<img src=\"" rel-path "\" />"))))))

(defun e/org-translate-keyword ( keyword _ _)
  (let (( key (downcase (org-element-property :key keyword))))
    (when (member key '("title" "id" "roots" "filetags" "atime" "mtime" "created"))
      (setq e/org-export-gatsby-header
            (cons `(,key . ,(org-element-property :value keyword))
                  e/org-export-gatsby-header))))
  nil)

(defun e/org-roam-gatsby-mathjax-macros ()
  (setq e/org-export-gatsby-header
        (cons `("mathjax_macros" .
                ,(string-join
                  (mapcar (lambda (m) (concat (car m) ": \"" (cadr m) "\""))
                          org-roam-notation-mathjax-macros)
                  ", "))
              e/org-export-gatsby-header)))
 
(defun e/org-translate-inner-template ( text _)
  (e/org-roam-gatsby-mathjax-macros)
  (let* (( title (cdr (assoc "title" e/org-export-gatsby-header)))
         ( slug (e/org-gatsby-title-slug title))
         ( header "<!-- GATSBY BEGIN\n"))
    (setq e/org-export-gatsby-header
          (cons `("slug" . ,slug) e/org-export-gatsby-header))
    (dolist ( line e/org-export-gatsby-header)
      (setq header (concat header "#+" (car line) ": " (cdr line) "\n")))
    (setq e/org-export-gatsby-header nil)
    (concat header "GATSBY END -->\n" text)))

(defun e/org-translate-drawer (drawer contents info)
  (let (( drawer-name (org-element-property :drawer-name drawer)))
    (cond ((string= drawer-name "thumbs")
           (format "<div class=\"%s\" id=\"%s\">\n%s</div>\n
<div class=\"container\">
  <span>&times;</span>
  <img>
  <div class=\"imgtext\"></div>
</div>\n"
                   drawer-name
                   (org-html--reference drawer info)
                   (org-html-drawer drawer contents info)))
          (t
           (org-html-drawer drawer contents info)))))

(defun e/org-translate-special-block ( special-block contents info)
  (let (( type (org-element-property :type special-block)))
    (cond ((string= type "thumbs")
           (concat (org-html-special-block special-block contents info)
                   "\n<div class=\"container\">
  <span>&times;</span>
  <img>
  <div class=\"imgtext\"></div>
</div>\n"))
          (t
           (org-html-special-block special-block contents info)))))

(defun e/org-translate-latex-environment ( latex-environment contents info)
  (let* (( environment (org-element-property :value latex-environment))
         ( class (when (string-match "\\\\begin{\\([^{}]+\\)}" environment)
                   (capitalize (match-string 1 environment))))
         ( id (org-html--reference latex-environment info)))
    (format "<div class=\"latexEnvironment%s\" id=\"%s\">\n%s</div>"
            class id
            (org-html-latex-environment latex-environment contents info))))

(defun e/org-roam-gatsby-date-string-to-time ( string)
  (if (not (string-match "^[0-9]\\{8,\\}" string))
      nil
    (setq string (match-string 0 string))
    (let (( year (string-to-number (substring string 0 4)))
          ( month (string-to-number (substring string 4 6)))
          ( day (string-to-number (substring string 6 8)))
          ( hour (and (>= (length string) 10)
                      (string-to-number (substring string 8 10))))
          ( minute (and (>= (length string) 12)
                        (string-to-number (substring string 10 12))))
          ( second (and (>= (length string) 14)
                        (string-to-number (substring string 12 14)))))
      (encode-time (list second minute hour day month year)))))

(defun e/org-roam-gatsby-before-parsing ( backend)
  (when (eq backend 'html-gatsby)
    (let (( node (org-roam-node-at-point))
          ( date-format "%Y-%m-%dT%H:%M:%S")
          ( created (file-name-base (buffer-name)))
          roots)
      (when (re-search-forward "#\\+title:" nil t)
        (beginning-of-line)
        (insert "#+id: " (org-roam-node-id node) "\n"
                "#+created: " (format-time-string
                               date-format
                               (e/org-roam-gatsby-date-string-to-time created))
                "\n"
                "#+atime: " (format-time-string date-format (org-roam-node-file-atime node)) "\n"
                "#+mtime: " (format-time-string date-format (org-roam-node-file-mtime node)) "\n"))
      (dolist ( tag (org-roam-node-tags node))
        (when (e/org-roam-gatsby-plist-find tag)
          (unless (org-roam-gatsby-backlink-nodes node tag)
            (setq roots (cons tag roots)))))
      (when roots
        (beginning-of-line)
        (insert "#+roots: " (string-join roots " ") "\n")))))

(add-hook 'org-export-before-parsing-functions 'e/org-roam-gatsby-before-parsing)

(org-export-define-derived-backend 'html-gatsby 'html
  :translate-alist '((property-drawer . e/org-translate-property-drawer)
                     (drawer . e/org-translate-drawer)
                     (link . e/org-translate-link)
                     (keyword . e/org-translate-keyword)
                     (latex-environment . e/org-translate-latex-environment)
                     (special-block . e/org-translate-special-block)
                     (inner-template . e/org-translate-inner-template)))

(defun e/org-html-publish-to-gatsby ( plist filename pub-dir)
  "Publishing function for \"roam-to-gatsby\" projects.

The extension of the exported file is determined by the project
plist value for html-extension, the value of the variable
`org-html-extension' or \"html\". The backend is html-gatsby,
which is defined by `org-export-define-derived-backend'.

PLIST is the property list for the given project,
i.e. \"roam-to-gatsby\" projects.  FILENAME is the filename of
the Org file to be published.  PUB-DIR, when non-nil is the
publishing directory, i.e. the target directory where the html
files will end up in."
  (setq e/org-roam-gatsby-plist plist)
  (org-publish-org-to 'html-gatsby
                      filename
                      (concat "." (or (plist-get plist :html-extension)
                                      org-html-extension
                                      "html"))
                      plist
                      pub-dir))

(provide 'org-roam-gatsby)


;;; org-roam-gatsby.el ends here
