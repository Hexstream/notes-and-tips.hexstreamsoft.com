;;;; -*- lexical-binding: t -*-

(defvar-local hexstream-xref-db nil)
(defvar-local hexstream-binding-type-to-namespaces nil)

(defun hexstream-search-for-match-at-column (search-function string &optional column bound noerror)
  (interactive "i\nMString to search for at current column: ")
  (check-type bound (or null integer marker))
  (unless search-function
    (setf search-function 'search-forward))
  (unless column
    (setf column (current-column)))
  (let ((original-point (point)))
    (or (block nil
          (while (funcall search-function string bound t)
            (when (= (save-excursion
                       (goto-char (match-beginning 0))
                       (current-column))
                     column)
              (return (match-end 0)))))
        (progn
          (goto-char (if (member noerror '(t nil))
                         original-point
                       (or bound (point-max))))
          (if noerror
              nil
            (signal 'search-failed (list string column)))))))

(defun hexstream-cross-reference-categories ()
  (interactive)
  (goto-char 0)
  (search-forward "<table")
  (let* ((table-end (copy-marker
                     (save-excursion
                       (hexstream-search-for-match-at-column
                        'search-forward
                        "</table>"
                        (progn
                          (goto-char (match-beginning 0))
                          (current-column))))))
         (tbody-column (progn
                         (search-forward "<tbody id=" table-end)
                         (goto-char (match-beginning 0))
                         (current-column))))
    (unless hexstream-xref-db
      (message "Creating DB...")
      (hexstream-init-xref-db))
    (message "Updating...")
    (save-restriction
      (while (hexstream-search-for-match-at-column
              're-search-forward "<tbody id=\"\\(.*\\)\"" tbody-column table-end t)
        (let ((id (hexstream-stupid-expand-entities (match-string 1))))
          (narrow-to-region (match-beginning 0)
                            (hexstream-search-for-match-at-column
                             'search-forward "</tbody>" tbody-column table-end))
          (goto-char (point-min))
          (hexstream-update-categories id)
          (goto-char (point-max))
          (widen))))))

(defun hexstream-stupid-expand-entities (string)
  (if (find ?& string)
      (replace-regexp-in-string
       "&.*?;"
       (lambda (entity)
         (let ((replacement
                (assoc entity '(("&amp;" . "&")
                                ("&lt;" . "<")
                                ("&gt;" . ">")))))
           (if replacement
               (cdr replacement)
             (error "Don't know how to replace entity \"%s\"." entity))))
       string
       nil
       t)
      string))

(defun hexstream-update-categories (symbol-name)
  (search-forward "</tr>")
  (goto-char (match-beginning 0))
  (let ((column (current-column))
        (categories-for-symbol (hexstream-query-xref-db symbol-name))
        (used-categories nil))
    (while (hexstream-search-for-match-at-column
            'search-forward "<tr" column nil t)
      (re-search-forward "<th.*?>\\([ \t\n]*\\)<a.*?>\\(.*?\\)</a>\\([^\0]*?\\)</th>")
      (let ((binding-type (match-string-no-properties 2))
            (first-gap (copy-marker (match-beginning 1)))
            (second-gap (copy-marker (match-beginning 3) t)))
        (replace-match "" t t nil 1)
        (replace-match "" t t nil 3)
        (goto-char first-gap)
        (newline-and-indent)
        (goto-char second-gap)
        (let ((applicable-namespaces
               (gethash binding-type
                        hexstream-binding-type-to-namespaces
                        :unrecognized-binding-type)))
          (if (and (eq applicable-namespaces :unrecognized-binding-type)
                   (not (member binding-type
                                '("Lambda list keyword"
                                  "Type specifier wildcard"
                                  "Method combination type"
                                  "Restart"
                                  "Glossary entry"
                                  "Optimize quality"
                                  "Documentation type"
                                  "Declaration"
                                  "Symbol"
                                  "Parameter specializer"))))
              (error "Unrecognized binding type: %S" binding-type)
            (dolist (category categories-for-symbol)
              (when (member (car (car category)) applicable-namespaces)
                (push category used-categories)
                (hexstream-render-category category)
                (goto-char second-gap)))))
        (set-marker first-gap nil)
        (set-marker second-gap nil)))
    (dolist (category categories-for-symbol)
      (unless (member category used-categories)
        (error "Category %S was not used!" category)))))

(defun hexstream-render-category (category)
  (labels ((maybe-add-class-for (name)
                                (let ((class (assoc name
                                                    '(("Obsolete" . "watch-out")
                                                      ("Discouraged" . "caution")
                                                      ("Not deprecated" . "good-news")))))
                                  (when class
                                    (forward-char)
                                    (insert (format " class=\"%s\"" (cdr class))))))
           (recurse (tail)
                    (newline-and-indent) ; Todo: Fix indentation bug.
                    (hexstream-html-doc-tag (point) (point) "ul" :style :block)
                    (destructuring-bind (human-readable-name url) (car tail)
                      (cond ((cdr tail)
                             (hexstream-html-doc-tag (point) (point) "li" :style :block)
                             (hexstream-html-doc-link (point) (point))
                             (insert url)
                             (maybe-add-class-for human-readable-name)
                             (goto-char hexstream-html-doc-inner-start-marker)
                             (insert human-readable-name)
                             (goto-char hexstream-html-doc-outer-end-marker)
                             (recurse (cdr tail)))
                            (t
                             (hexstream-html-doc-tag (point) (point) "li")
                             (hexstream-html-doc-link (point) (point))
                             (insert url)
                             (maybe-add-class-for human-readable-name)
                             (goto-char hexstream-html-doc-inner-start-marker)
                             (insert human-readable-name))))))
    (recurse category)))

(defun hexstream-init-xref-db ()
  (interactive)
  (setf hexstream-binding-type-to-namespaces (make-hash-table :test 'equal))
  (setf hexstream-xref-db
        (let ((db (make-hash-table :test 'equal)))
          (dolist (dir (with-temp-buffer
                         (insert-file-contents "index.txt")
                         (split-string (buffer-string) "\n" t)))
            (let* ((directory (expand-file-name dir))
                   (category-xref-sexp-path (concat directory "category-xref.sexp")))
              (when (file-exists-p category-xref-sexp-path)
                (dolist (tree (read (with-temp-buffer
                                      (insert "(")
                                      (insert-file-contents category-xref-sexp-path)
                                      (goto-char (point-max))
                                      (insert ")")
                                      (buffer-string))))
                  (destructuring-bind (namespace-name binding-types) (car tree)
                    (dolist (binding-type binding-types)
                      (push namespace-name
                            (gethash binding-type hexstream-binding-type-to-namespaces)))
                    (setf (car tree) namespace-name))
                  (hexstream-map-category-xref
                   (lambda (info symbol-names)
                     (dolist (symbol-name symbol-names)
                       (push info (gethash symbol-name db))))
                   tree
                   directory
                   dir)))))
          (maphash (lambda (key value)
                     (setf (gethash key db) (reverse value)))
                   db)
          (maphash (lambda (key value)
                     (setf (gethash key hexstream-binding-type-to-namespaces)
                           (reverse value)))
                   hexstream-binding-type-to-namespaces)
          db)))

(defun hexstream-map-category-xref (function tree directory namespace-group-path)
  (labels ((R (tree directory info path state)
              (destructuring-bind (human-readable-name &rest children)
                  (etypecase tree
                    (list tree)
                    (string (list tree)))
                (let ((identifier (hexstream-identifierize human-readable-name)))
                  (setf directory
                        (file-name-as-directory (concat directory identifier))
                        path
                        (ecase state
                          (:top-level (concat path identifier "/"))
                          (:namespace (concat path "#" identifier))
                          (:subcategory (concat path "_" identifier))))
                  (push (list human-readable-name path) info)
                  (if children
                      (dolist (child children)
                        (R child directory info path (ecase state
                                                       (:top-level :namespace)
                                                       ((:namespace :subcategory)
                                                        :subcategory))))
                    (funcall function
                             (reverse info)
                             (with-temp-buffer
                               (insert-file-contents (concat directory "symbols.txt"))
                               (split-string (buffer-string) "\n" t))))))))
    (R tree directory nil namespace-group-path :top-level)))

(defun hexstream-query-xref-db (symbol-name)
  (interactive "MQuery xref DB for symbol named:")
  (let ((value (gethash symbol-name
                        (or hexstream-xref-db (hexstream-init-xref-db)))))
    (when (called-interactively-p 'interactive)
      (message "%S" value))
    value))

(defun hexstream-identifierize (human-readable-name)
  (replace-regexp-in-string " " "-" (downcase human-readable-name) t t))
