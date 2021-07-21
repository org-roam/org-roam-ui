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
                (eval (org-roam-ui-html-servlet id file))))))
   (add-hook 'post-command-hook #'org-roam-ui-check-buffer))
   (t
    (remove-hook 'post-command-hook #'org-roam-ui-check-buffer)
    (dolist (buf (org-roam-buffer-list))
      (with-current-buffer buf
        (remove-hook 'post-command-hook #'org-roam-ui-node-p t)))
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

(defservlet* current-node-data text/event-stream ()
  (insert (format  "data: %s\n\n"
                       org-roam-ui-current-node))
  (httpd-send-header t "text/plain" 200 :Access-Control-Allow-Headers "*" :Access-Control-Allow-Origin "*"))

(defun org-roam-ui-update-current-buffer ()
  "Set the org-roam-ui-current-buffer to the current buffer"
          (setq org-roam-ui-current-buffer (current-buffer))
          )

(defun org-roam-ui-check-buffer ()
  (when (org-roam-buffer-p (current-buffer))
        (add-hook 'post-command-hook #'org-roam-ui-node-p nil t)
        (org-roam-ui-update-current-buffer)
    ))

(defun org-roam-ui-node-id ()
  (if-let (current-node-id (org-id-get))
      (setq org-roam-ui-current-node current-node-id)
      ))


(defvar org-roam-ui-current-buffer)
(defvar org-roam-ui-current-node nil)
(provide 'org-roam-ui)
