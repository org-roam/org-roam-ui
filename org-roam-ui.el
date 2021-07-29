 ;;; org-roam-ui.el --- User Interface for Org-roam -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2021 ...

;; Author: ...
;; URL: https://github.com/org-roam/org-roam-ui
;; Keywords: org-mode, roam
;; Version: ...
;; Package-Requires: ((emacs "26.1") (f "0.17.2") (org-roam "2.0.0"))

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

(defvar org-roam-ui/root-dir default-directory
  "Root directory of the org-roam-ui project.")

(defvar org-roam-ui/app-build-dir (expand-file-name "./out/")
  "Directory containing org-roam-ui's web build.")

;;; Dynamic variables
(defvar org-roam-ui-current-node-id nil
  "The current node id Org-roam is tracking.")

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
E.g. '((bg . '#1E2029')
(bg-alt . '#282a36')
(fg . '#f8f8f2')
(fg-alt . '#6272a4')
(red . '#ff5555')
(orange . '#f1fa8c')
(yellow .'#ffb86c')
(green . '#50fa7b')
(cyan . '#8be9fd')
(blue . '#ff79c6')
(violet . '#8be9fd')
(magenta . '#bd93f9'))."
  :group 'org-roam-ui
  :type 'list)

(defvar org-roam-ui--ws-current-node nil)
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
    (setq httpd-port org-roam-ui-port
          httpd-root org-roam-ui/app-build-dir)
    (httpd-start)
    (setq org-roam-ui-ws
        (websocket-server
         35903
         :host 'local
         :on-open (lambda (ws) (progn (setq oru-ws
         ws) (org-roam-ui--send-graphdata) (org-roam-ui-sync-theme--advice) (message "Connection
         established with org-roam-ui")
    (add-hook 'post-command-hook
                                 #'org-roam-ui--update-current-node)
                                 ))
         :on-close (lambda (_websocket) (setq oru-ws
         nil) (message "Connection with org-roam-ui closed
         succesfully."))))
    (if (boundp 'counsel-load-theme)
(advice-add 'counsel-load-theme :around #'org-roam-ui-sync-theme--advice)
            (advice-add 'load-theme :around #'org-roam-ui-sync-theme--advice))
    (add-hook 'post-command-hook #'org-roam-ui--update-current-node))
    ;(add-hook 'post-command-hook #'org-roam-ui-update))
   (t
    (progn
    (remove-hook 'post-command-hook #'org-roam-ui-update)
    (remove-hook 'post-command-hook #'org-roam-ui--update-current-node)
    (if (boundp 'counsel-load-theme)
(advice-remove 'counsel-load-theme #'org-roam-ui-sync-theme--advice)
            (advice-remove 'load-theme #'org-roam-ui-sync-theme--advice))
    (websocket-server-close org-roam-ui-ws)
    (delete-process org-roam-ui-ws)
    (httpd-stop)))))


(defun org-roam-ui--send-graphdata ()
  "Get roam data, make JSON, send through websocket to org-roam-ui."
  (let* ((nodes-columns [id file title level])
         (links-columns [source dest type])
         (nodes-db-rows (org-roam-db-query `[:select ,nodes-columns :from nodes]))
         (links-db-rows (org-roam-db-query `[:select ,links-columns :from links :where (or (= type "id") (= type "cite"))]))
         (response `((nodes . ,(mapcar (apply-partially #'org-roam-ui-sql-to-alist (append nodes-columns nil)) nodes-db-rows))
                                  (links . ,(mapcar (apply-partially #'org-roam-ui-sql-to-alist '(source target type)) links-db-rows)))))
    (websocket-send-text oru-ws (json-encode `((type . "graphdata") (data . ,response))))))

(defun org-roam-ui--update-current-node ()
  (when (and (boundp 'org-roam-ui-websocket) (websocket-openp org-roam-ui-websocket))
  (let* ((node (org-roam-id-at-point)))
    (unless (string-match-p (regexp-quote "Minibuf") (buffer-name (current-buffer)))
    (unless (string= org-roam-ui--ws-current-node node)
    (setq org-roam-ui--ws-current-node node)
      (websocket-send-text org-roam-ui-websocket (json-encode `((type . "command") (data . ((commandName . "follow") (id . ,node))))))))))
)

(defun org-roam-ui-show-node ()
  "Open the current org-roam node in org-roam-ui."
  (interactive)
      (websocket-send-text oru-ws (json-encode `((type . "command") (data . ((commandName . "follow") (id . ,(org-roam-id-at-point))))))))

(defun org-roam-ui-sync-theme--advice ()
  "Function which is called after load-theme to sync your current theme with org-roam-ui."
  (message "Syncing theme")
  (websocket-send-text oru-ws (json-encode `((type . "theme") (data . ,(org-roam-ui--update-theme))))))

(defun org-roam-ui-sync-theme-manually ()
  "Sync your current Emacs theme with org-roam-ui."
  (interactive)
  (websocket-send-text oru-ws (json-encode `((type . "theme") (data . ,(org-roam-ui--update-theme))))))

(defun org-roam-ui--update-theme ()
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

(defservlet* graph application/json ()
  (let* ((nodes-columns [id file title level])
         (links-columns [source dest type])
         (nodes-db-rows (org-roam-db-query `[:select ,nodes-columns :from nodes]))
         (links-db-rows (org-roam-db-query `[:select ,links-columns :from links :where (or (= type "id") (= type "cite"))]))
         (response (json-encode `((nodes . ,(mapcar (apply-partially #'org-roam-ui-sql-to-alist (append nodes-columns nil)) nodes-db-rows))
                                  (links . ,(mapcar (apply-partially #'org-roam-ui-sql-to-alist '(source target type)) links-db-rows))))))
    (insert response)
    (httpd-send-header t "application/json" 200 :Access-Control-Allow-Origin "*")))

(defun org-roam-ui-sql-to-alist (column-names rows)
  "Convert sql result to alist for json encoding.
ROWS is the sql result, while COLUMN-NAMES is the columns to use."
  (let (res)
    (while rows
      (push (cons (pop column-names) (pop rows)) res))
    res))


(defservlet* id/:id text/html ()
  (let ((node (org-roam-populate (org-roam-node-create :id id)))
        html-string)
    (org-roam-with-temp-buffer (org-roam-node-file node)
      (progn
      (setq-local org-export-with-toc nil)
      (setq-local org-export-with-broken-links t)
      (setq-local org-export-with-sub-superscripts nil)
      (replace-string "[[id:" "[[./")
             (let* ((file-string (buffer-string))
                    (matches (s-match-strings-all "\\[\\[\\(file:\\|\\.\\/\\)\\(.*\\.\\(png\\|jpg\\|jpeg\\|gif\\|svg\\)\\)\\]\\(\\[.*\\]\\)?\\]" file-string)))
               (dolist (match matches)
                 (let ((path (elt match 2))
                       (link (elt match 0)))
                   (unless (file-name-absolute-p path)
                     (setq path (concat (file-name-directory (org-roam-node-file-node)) path)))
                   (setq path (f-full path))
                   (if (file-exists-p path)
                       (setq file-string
                             (s-replace link (format "[[image:%s]]" path) file-string)))))
               (erase-buffer)
             (insert file-string))
      (setq html-string (org-export-as 'html))))
    (insert html-string)
    (httpd-send-header t "text/html" 200 :Access-Control-Allow-Origin "*")))


(defservlet* current-node-id text/event-stream ()
  (insert (format "data: %s\n\n"
                       org-roam-ui-current-node-id))
  (httpd-send-header t "text/event-stream" 200 :Access-Control-Allow-Origin "*"))

(defun org-roam-ui-update ()
  "Track changes within Emacs to update Org-roam UI.
This function is added to `post-command-hook'."
  (setq org-roam-ui-current-node-id
        (or
         (condition-case nil (org-roam-id-at-point) (error nil))
         org-roam-ui-current-node-id)))

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


;; (defservlet* theme text/stream ()
;;   (progn)
;;   (if org-roam-ui-sync-theme
;;     (if (boundp 'doom-themes--colors)
;;       (let*
;;         ((colors (butlast doom-themes--colors (- (length doom-themes--colors) 25))) ui-theme (list nil))
;;         (progn
;;           (dolist (color colors) (push (cons (car color) (car (cdr color))) ui-theme))
;;           ui-theme))
;;       (insert (format "data: %s\n\n" (json-encode (org-roam-ui-get-theme)))))
;;     (when org-roam-ui-custom-theme
;;       (insert (format "data %s\n\n" (json-encode org-roam-ui-custom-theme)))))
;;     (httpd-send-header t "text/event-stream" 200 :Access-Control-Allow-Origin "*"))

(provide 'org-roam-ui)
;;; org-roam-ui.el ends here
