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
    (add-hook 'post-command-hook #'org-roam-ui-update))
   (t
    (remove-hook 'post-command-hook #'org-roam-ui-update)
    (httpd-stop))))

(defservlet* graph application/json ()
  (let* ((nodes-columns [id file title])
         (links-columns [source dest])
         (nodes-db-rows (org-roam-db-query `[:select ,nodes-columns :from nodes]))
         (links-db-rows (org-roam-db-query `[:select ,links-columns :from links :where (= type "id")]))
         (response (json-encode `((nodes . ,(mapcar (apply-partially #'org-roam-ui-sql-to-alist (append nodes-columns nil)) nodes-db-rows))
                                  (links . ,(mapcar (apply-partially #'org-roam-ui-sql-to-alist '(source target)) links-db-rows))))))
    (insert response)
    (httpd-send-header t "application/json" 200 :Access-Control-Allow-Origin "*")))

(defun org-roam-ui-sql-to-alist (column-names rows)
  "Convert sql result to alist for json encoding.
ROWS is the sql result, while COLUMN-NAMES is the columns to use."
  (let (res)
    (while rows
      (push (cons (pop column-names) (pop rows)) res))
    res))

(defservlet* theme text/stream ()
  (progn
    (when (boundp 'doom-themes--colors)
      (let*
        ((colors (butlast doom-themes--colors (- (length doom-themes--colors) 25))) ui-theme (list nil))
        (progn
          (dolist (color colors) (push (cons (car color) (car (cdr color))) ui-theme))
          (insert (format "data: %s\n\n" (json-encode  ui-theme))))))
    (httpd-send-header t "text/event-stream" 200 :Access-Control-Allow-Origin "*")))


(defservlet* id/:id text/html ()
  (let ((node (org-roam-populate (org-roam-node-create :id id)))
        html-string)
    (org-roam-with-temp-buffer (org-roam-node-file node)
      (setq-local org-export-with-toc nil)
      (setq-local org-export-with-broken-links t)
      (setq-local org-export-with-sub-superscripts nil)
      (setq html-string (org-export-as 'html)))
    (insert html-string)))

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

(provide 'org-roam-ui)
;;; org-roam-ui.el ends here
