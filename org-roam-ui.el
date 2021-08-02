;;; org-roam-ui.el --- User Interface for Org-roam -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2021 Kirill Rogovoy, Thomas F. K. Jorna

;; author: Kirill Rogovoy, Thomas Jorna
;; URL: https://github.com/org-roam/org-roam-ui
;; Keywords: org-mode, roam
;; Version: 0
;; Package-Requires: ((emacs "26.1") (f "0.17.2") (org-roam "2.0.0") (simple-httpd "20191103.1446") (websocket "20210110.17") (json "1.2"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;;  Org-roam-ui provides a web interface for navigating around notes created
;;  within Org-roam.
;;
;;; Code:
;;;; Dependencies
(require 'f)
(require 'json)
(require 'simple-httpd)
(require 'org-roam)
(require 'websocket)

(defgroup org-roam-ui nil
  "UI in Org-roam."
  :group 'org-roam
  :prefix "org-roam-ui-"
  :link '(url-link :tag "Github" "https://github.com/org-roam/org-roam-ui"))

(defvar org-roam-ui/root-dir
  (concat (file-name-directory
           (f-full (or
                    load-file-name
                    buffer-file-name)))
          ".")
  "Root directory of the org-roam-ui project.")

(defvar org-roam-ui/app-build-dir (expand-file-name "./out/" org-roam-ui/root-dir)
  "Directory containing org-roam-ui's web build.")

;; TODO: make into defcustom
(defvar org-roam-ui-port
  35901
  "Port to serve the org-roam-ui interface.")

(defcustom org-roam-ui-sync-theme t
  "If true, sync your current Emacs theme with org-roam-ui.
Works best with doom-themes.
Ignored if a custom theme is provied for 'org-roam-ui-custom-theme'."
  :group 'org-roam-ui
  :type 'boolean)

(defcustom org-roam-ui-custom-theme nil
  "Custom theme for org-roam-ui. Blocks 'org-roam-ui-sync-theme.
Provide a list of cons with the following values:
bg, bg-alt, fg, fg-alt, red, orange, yellow, green, cyan, blue, violet, magenta.
E.g. '((bg . \"#1E2029\")
\(bg-alt . \"#282a36\")
\(fg . \"#f8f8f2\")
\(fg-alt . \"#6272a\")
\(red . \"#ff5555\")
\(orange . \"#f1fa8c\")
\(yellow .\"#ffb86c\")
\(green . \"#50fa7b\")
\(cyan . \"#8be9fd\")
\(blue . \"#ff79c6\")
\(violet . \"#8be9fd\")
\(magenta . \"#bd93f9\"))."
  :group 'org-roam-ui
  :type 'list)

(defcustom org-roam-ui-follow t
  "If true, org-roam-ui will follow you around in the graph."
  :group 'org-roam-ui
  :type 'boolean)

(defcustom org-roam-ui-update-on-save t
  "If true, org-roam-ui will send new data when you save an org-roam-buffer.
This can lead to some jank."
  :group 'org-roam-ui
  :type 'boolean)

(defcustom org-roam-ui-open-on-start t
  "Whether to open your default browser when org-roam-ui-mode launces."
  :group 'org-roam-ui
  :type 'boolean)

(defvar org-roam-ui--ws-current-node nil
  "Var to keep track of which node you are looking at.")
(defvar oru-ws nil
  "The websocket for org-roam-ui.")

;;;###autoload
(define-minor-mode
  org-roam-ui-mode
  "Enable org-roam-ui.
This serves the web-build and API over HTTP."
  :lighter "org-roam-ui "
  :global t
  :group 'org-roam-ui
  :init-value nil
  (cond
   (org-roam-ui-mode
    (setq-local httpd-port org-roam-ui-port)
    (setq httpd-root org-roam-ui/app-build-dir)
    (httpd-start)
    (when org-roam-ui-open-on-start
      (browse-url "http://localhost:35901"))
    (setq org-roam-ui-ws
        (websocket-server
         35903
         :host 'local
         :on-open (lambda (ws) (progn
            (setq oru-ws ws)
            (org-roam-ui--send-graphdata)
            (when org-roam-ui-update-on-save
            (add-hook 'after-save-hook #'org-roam-ui--on-save))
            (message "Connection established with org-roam-ui")
            (when org-roam-ui-follow
              (org-roam-ui-follow-mode 1))))
        :on-message (lambda (_websocket frame)
                    (org-roam-node-visit
                        (org-roam-populate (org-roam-node-create
                        :id (websocket-frame-text frame)))))
         :on-close (lambda (_websocket)
            (remove-hook 'after-save-hook #'org-roam-ui--on-save)
            (org-roam-ui-follow-mode -1)
            (message "Connection with org-roam-ui closed.")))))
   (t
    (progn
    (websocket-server-close org-roam-ui-ws)
    (httpd-stop)
    (remove-hook 'after-save-hook #'org-roam-ui--on-save)
    (org-roam-ui-follow-mode -1)
    ))))

(defun org-roam-ui--on-save ()
  "Send graphdata on saving an org-roam buffer."
  (when (org-roam-buffer-p)
    (org-roam-ui--send-graphdata))
  )

(defun org-roam-ui--send-graphdata ()
  "Get roam data, make JSON, send through websocket to org-roam-ui."
  (let* ((nodes-columns [id file title level])
         (links-columns [links:source links:dest links:type refs:node-id])
         (nodes-db-rows (org-roam-db-query `[:select ,nodes-columns :from nodes]))
         ;; Left outer join on refs means any id link (or cite link without a
         ;; corresponding node) will have 'nil for the `refs:node-id' value. Any
         ;; cite link where a node has that `:ROAM_REFS:' will have a value.
         (links-db-rows (org-roam-db-query `[:select ,links-columns
                                             :from links
                                             :left :outer :join refs :on (= links:dest refs:ref)
                                             :where (or (= links:type "id") (= links:type "cite"))]))
         ;; Convert any cite links that have nodes with associated refs to a
         ;; standard id link while removing the 'nil `refs:node-id' from all
         ;; other links
         (links-db-rows (seq-map (lambda (l)
                                   (pcase-let ((`(,source ,dest ,type ,node-id) l))
                                     (if node-id
                                         (list source node-id "id")
                                       (list source dest type)))) links-db-rows))
         (response `((nodes . ,(mapcar (apply-partially #'org-roam-ui-sql-to-alist (append nodes-columns nil)) nodes-db-rows))
                                  (links . ,(mapcar (apply-partially #'org-roam-ui-sql-to-alist '(source target type)) links-db-rows)))))
    (websocket-send-text oru-ws (json-encode `((type . "graphdata") (data . ,response))))))

(defun org-roam-ui--update-current-node ()
  "Send the current node data to the web-socket."
  (when (and (websocket-openp oru-ws) (org-roam-buffer-p))
  (let* ((node (org-roam-id-at-point)))
    (unless (string= org-roam-ui--ws-current-node node)
    (setq org-roam-ui--ws-current-node node)
      (websocket-send-text oru-ws (json-encode `((type . "command") (data
. ((commandName . "follow") (id . ,node))))))))))


;; (defun org-roam-ui-sync-theme--advice ()
;;   "Function which is called after load-theme to sync your current theme with org-roam-ui."
;;   (message "Syncing theme")
;;   (websocket-send-text oru-ws (json-encode `((type . "theme") (data . ,(org-roam-ui--update-theme))))))


(defun org-roam-ui--update-theme ()
  "Send the current theme data to the websocket."
  (let  ((ui-theme (list nil)))
  (if org-roam-ui-sync-theme
    (if (boundp 'doom-themes--colors)
      (let*
        ((colors (butlast doom-themes--colors (- (length doom-themes--colors) 25))) doom-theme (list-nil))
        (progn
          (dolist (color colors) (push (cons (car color) (car (cdr color))) doom-theme)))
        (setq ui-theme doom-theme))
      (setq ui-theme (org-roam-ui-get-theme)))
    (when org-roam-ui-custom-theme
     org-roam-ui-custom-theme))
  ui-theme))

;; (defservlet* graph application/json ()
;;   (let* ((nodes-columns [id file title level])
;;          (links-columns [source dest type])
;;          (nodes-db-rows (org-roam-db-query `[:select ,nodes-columns :from nodes]))
;;          (links-db-rows (org-roam-db-query `[:select ,links-columns :from links :where (or (= type "id") (= type "cite"))]))
;;          (response (json-encode `((nodes . ,(mapcar (apply-partially #'org-roam-ui-sql-to-alist (append nodes-columns nil)) nodes-db-rows))
;;                                   (links . ,(mapcar (apply-partially #'org-roam-ui-sql-to-alist '(source target type)) links-db-rows))))))
;;     (insert response)
;;     (httpd-send-header t "application/json" 200 :Access-Control-Allow-Origin "*")))

(defun org-roam-ui-sql-to-alist (column-names rows)
  "Convert sql result to alist for json encoding.
ROWS is the sql result, while COLUMN-NAMES is the columns to use."
  (let (res)
    (while rows
      (push (cons (pop column-names) (pop rows)) res))
    res))


;; (defservlet* id/:id text/html ()
;;   (let ((node (org-roam-populate (org-roam-node-create :id id)))
;;         html-string)
;;     (org-roam-with-temp-buffer (org-roam-node-file node)
;;       (progn
;;       (setq-local org-export-with-toc nil)
;;       (setq-local org-export-with-broken-links t)
;;       (setq-local org-export-with-sub-superscripts nil)
;;       (replace-string "[[id:" "[[./")
;;              (let* ((file-string (buffer-string))
;;                     (matches (s-match-strings-all "\\[\\[\\(file:\\|\\.\\/\\)\\(.*\\.\\(png\\|jpg\\|jpeg\\|gif\\|svg\\)\\)\\]\\(\\[.*\\]\\)?\\]" file-string)))
;;                (dolist (match matches)
;;                  (let ((path (elt match 2))
;;                        (link (elt match 0)))
;;                    (unless (file-name-absolute-p path)
;;                      (setq path (concat (file-name-directory (org-roam-node-file-node)) path)))
;;                    (setq path (f-full path))
;;                    (if (file-exists-p path)
;;                        (setq file-string
;;                              (s-replace link (format "[[image:%s]]" path) file-string)))))
;;                (erase-buffer)
;;              (insert file-string))
;;       (setq html-string (org-export-as 'html))))
;;     (insert html-string)
;;     (httpd-send-header t "text/html" 200 :Access-Control-Allow-Origin "*")))

(defun org-roam-ui-get-theme ()
  "Attempt to bring the current theme into a standardized format."
(list `(bg . ,(face-background hl-line-face))
        `(bg-alt . ,(face-background 'default))
        `(fg . ,(face-foreground 'default))
        `(fg-alt . ,(face-foreground font-lock-comment-face))
        `(red . ,(face-foreground 'error))
        `(orange . ,(face-foreground 'warning))
        `(yellow . ,(face-foreground font-lock-builtin-face))
        `(green . ,(face-foreground 'success))
        `(cyan . ,(face-foreground font-lock-constant-face))
        `(blue . ,(face-foreground font-lock-keyword-face))
        `(violet . ,(face-foreground font-lock-constant-face))
        `(magenta . ,(face-foreground font-lock-preprocessor-face))
        ))


;;;; commands
;;;###autoload
(defun orui-node-zoom (&optional id speed padding)
  "Move the view of the graph to current node.
or optionally a node of your choosing.
Optionally takes three arguments:
The ID of the node you want to travel to.
The SPEED in ms it takes to make the transition.
The PADDING around the nodes in the viewport."
  (interactive)
  (if-let ((node (or id (org-roam-id-at-point))))
  (websocket-send-text oru-ws (json-encode `((type . "command") (data .
      ((commandName . "zoom") (id . ,node) (speed . ,speed) (padding . ,padding)))))))
  (message "No node found."))


;;;###autoload
(defun orui-node-local (&optional id speed padding)
  "Open the local graph view of the current node.
Optionally with ID (string), SPEED (number, ms) and PADDING (number, px)."
  (interactive)
  (if-let ((node (or id (org-roam-id-at-point))))
  (websocket-send-text oru-ws (json-encode `((type . "command") (data .
      ((commandName . "local") (id . ,node)))))))
  (message "No node found."))


;;;###autoload
(define-minor-mode org-roam-ui-follow-mode
  "Set whether ORUI should follow your every move in Emacs."
  :lighter "org-roam-ui "
  :global t
  :group 'org-roam-ui
  :init-value nil
  (if org-roam-ui-follow-mode
      (progn
    (add-hook 'post-command-hook #'org-roam-ui--update-current-node)
    (message "Org-Roam-UI will now follow you around."))
      (remove-hook 'post-command-hook #'org-roam-ui--update-current-node)
      (message "Org-Roam-UI will now leave you alone.")
  ))


(defun orui-sync-theme ()
  "Sync your current Emacs theme with org-roam-ui."
  (interactive)
  (websocket-send-text oru-ws (json-encode `((type . "theme") (data . ,(org-roam-ui--update-theme))))))

(provide 'org-roam-ui)
;;; org-roam-ui.el ends here
