(require 'json)
(require 'simple-httpd)

(setq org-roam-ui/root-dir (file-name-directory load-file-name))
(setq org-roam-ui/app-build-dir (expand-file-name "./" org-roam-ui/root-dir))

(define-minor-mode
  org-roam-ui-mode
  "Start the http API for org-roam-ui."
  :lighter ""
  :global t
  :group 'org-roam-ui
  :init-value nil
  (cond
   (org-roam-ui-mode
    (setq-local httpd-port 35901)
    (httpd-start))
   (t
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

(httpd-def-file-servlet app org-roam-ui/app-build-dir)


(defun nodes-row-to-cons (row)
  (list
   (cons 'id (elt row 0))
   (cons 'file (elt row 1))
   (cons 'title (elt row 8))))

(defun links-row-to-cons (row)
  (list
   (cons 'source (elt row 1))
   (cons 'target (elt row 2))))

(provide 'org-roam-ui)
