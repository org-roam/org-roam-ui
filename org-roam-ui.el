(require 'f)
(require 'json)
(require 'simple-httpd)

(defvar org-roam-ui/root-dir
  (concat
   (file-name-directory
    (f-full (or
             load-file-name
             buffer-file-name))) "."))

(setq org-roam-ui/app-build-dir (expand-file-name "./web-build/" org-roam-ui/root-dir))

(define-minor-mode
  org-roam-ui-mode
  "Start the http API for org-roam-ui."
  :lighter ""
  :global t
  :group 'org-roam-ui
  :init-value nil
  (cond
   (org-roam-ui-mode
    (setq org-export-with-broken-links t)
    (setq-local httpd-port 35901)
    (setq httpd-root org-roam-ui/app-build-dir)
    (httpd-start)
    (org-roam-with-temp-buffer nil
    (let ((nodes (org-roam-db-query
                  `[:select [id file] :from nodes])))
      (dotimes (idx (length nodes))
        (let ((id (car (elt nodes idx)))
                (file (elt (elt nodes idx) 1)))
                (eval (org-roam-ui-html-servlet id file)))))))
   (t
    (setq org-export-with-broken-links nil)
    (httpd-stop))))


(defservlet* graph application/json ()
  (let* (
    (nodes-db-rows (org-roam-db-query `[:select [*] :from nodes]))
    (links-db-rows (org-roam-db-query `[:select [*] :from links :where (= type "id")]))
    (response (json-encode (list
      (cons 'nodes (mapcar 'nodes-row-to-cons nodes-db-rows))
      (cons 'links (mapcar 'links-row-to-cons links-db-rows))))))
      (progn
        (insert response)
        (httpd-send-header t "text/plain" 200 :Access-Control-Allow-Origin "*"))))

(defun nodes-row-to-cons (row)
  (list
   (cons 'id (elt row 0))
   (cons 'file (elt row 1))
   (cons 'title (elt row 8))))

(defun links-row-to-cons (row)
  (list
   (cons 'source (elt row 1))
   (cons 'target (elt row 2))))

(defservlet* theme application/json ()
  (when 'doom-themes--colors
      (let*
            ((colors (butlast doom-themes--colors (- (length doom-themes--colors) 25)))
             ui-theme (list nil))
      (progn
        (dolist (color colors)
          (push (cons (car color) (car (cdr color))) ui-theme))
        (insert (json-encode  ui-theme))
        (httpd-send-header t "text/plain" 200 :Access-Control-Allow-Origin "*")))))

;; Taken from org-roam-server
(defun org-roam-ui-html-servlet (id file)
  "Export the FILE to HTML and create a servlet for it."
  `(defservlet* ,(intern (concat "files/" (concat id ".html"))) text/html ()
     (let ((html-string))
       (org-roam-with-temp-buffer ,file
         (setq-local org-export-with-sub-superscripts nil)
         (setq html-string (org-export-as 'html)))
       (insert html-string))))

(provide 'org-roam-ui)
