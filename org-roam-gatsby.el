;;; org-roam-gatsby --- Org Roam to Gatsby  -*- lexical-binding: t; -*-


;;; Commentary:


;;; Code:
(require 'org)
(require 'org-roam)
(require 'ox)

(defvar e/org-roam-gatsby-plist nil)

(defvar e/org-export-gatsby-header nil)

(defvar e/org-roam-gatsby-hook nil)

(defun e/org-roam-gatsby-plist-find ( tag)
  (let (( plists (mapcar 'cdr org-publish-project-alist))
        plist)
    (while plists
      (setq plist (pop plists))
      (when (string= (plist-get plist :tag) tag)
        (setq plists nil)))
    plist))

(defun e/org-roam-gatsby-symlink-after-save ()
  "Maintain symbolic links to publishing directory.

Works with the current buffer."
  (let* (( file-name (buffer-file-name (buffer-base-buffer (current-buffer))))
         ( file-nondir (file-name-nondirectory file-name))
         ( filetags (split-string (or (cadar (org-collect-keywords '("FILETAGS"))) "")
                                  ":" 'OMIT-NULLS)))
    (dolist ( project org-publish-project-alist)
      (let* (( plist (cdr project))
             ( tag (plist-get plist :tag)))
        (when tag
          (let (( symbolic-link-name (expand-file-name
                                      file-nondir
                                      (file-name-as-directory
                                       (plist-get plist :base-directory)))))
            (if (member tag filetags)
                (make-symbolic-link file-name symbolic-link-name t)
              (when (file-exists-p symbolic-link-name)
                (delete-file symbolic-link-name)))))))))

(defun e/org-roam-gatsby-publish-after-save ()
  (let (( filetags (split-string (or (cadar (org-collect-keywords '("FILETAGS"))) "")
                                 ":" 'OMIT-NULLS)))
    (when filetags
      (dolist ( project org-publish-project-alist)
        (let* (( plist (cdr project))
               ( tag (plist-get plist :tag)))
          (when (and tag (member tag filetags))
            (message "Org-Roam-Gatsby: Publishing project \"%s\"" (car project))
            (org-publish-project (car project))))))))

(defun e/org-roam-gatsby-hook-functions ()
  "Gatsby hook functions."
  (add-hook 'after-save-hook #'e/org-roam-gatsby-symlink-after-save 0 'local)
  (add-hook 'after-save-hook #'e/org-roam-gatsby-publish-after-save 10 'local))

(add-hook 'e/org-roam-gatsby-hook 'e/org-roam-gatsby-hook-functions)

(defun e/org-roam-gatsby-run-hook ()
  (when (org-roam-file-p)
    (run-hooks 'e/org-roam-gatsby-hook)))

(add-hook 'org-mode-hook 'e/org-roam-gatsby-run-hook)

(defun e/org-gatsby-title-slug (title)
  (replace-regexp-in-string
   "-+" "-"
   (replace-regexp-in-string
    "[^a-zA-Z0-9\\-$_.+!*'(),]" "-"
    (downcase title))))

(defun e/org-translate-link (link _ _)
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
             ( plist e/org-roam-gatsby-plist))
        (if (member (plist-get plist :tag) tags)
            (concat "<a href=\""
                    (file-name-as-directory (plist-get plist :rel-url))
                    file "/" title "\">" description "</a>")
          description)))))

(defun e/org-translate-keyword (keyword _ _)
  (setq e/org-export-gatsby-header
        (cons `(,(downcase (org-element-property :key keyword)) .
                ,(org-element-property :value keyword))
              e/org-export-gatsby-header))
  nil)

(defun e/org-translate-inner-template (text _)
  (let* (( title (assoc "title" e/org-export-gatsby-header))
         ( slug (e/org-gatsby-title-slug (cdr title))))
    (setq e/org-export-gatsby-header
          (cons `("slug" . ,slug) e/org-export-gatsby-header)))
  (let (( header "<!-- GATSBY BEGIN\n"))
    (dolist ( line e/org-export-gatsby-header)
      (setq header (concat header "#+" (car line) ": " (cdr line) "\n")))
    (setq e/org-export-gatsby-header nil)
    (concat header "GATSBY END -->\n" text)))

(defun e/org-translate-property-drawer (property-drawer _ _)
  (message "%s" property-drawer))

(org-export-define-derived-backend 'html-gatsby 'html
  :translate-alist '((property-drawer . e/org-translate-property-drawer)
                     (link . e/org-translate-link)
                     (keyword . e/org-translate-keyword)
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
