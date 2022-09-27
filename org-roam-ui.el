;;; org-roam-ui.el --- User Interface for Org-roam -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2021 Kirill Rogovoy, Thomas F. K. Jorna

;; author: Kirill Rogovoy, Thomas Jorna
;; URL: https://github.com/org-roam/org-roam-ui
;; Keywords: files outlines
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (org-roam "2.0.0") (simple-httpd "20191103.1446") (websocket "1.13"))

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
(require 'json)
(require 'simple-httpd)
(require 'org-roam)
(require 'websocket)
(require 'org-roam-dailies)

(defgroup org-roam-ui nil
  "UI in Org-roam."
  :group 'org-roam
  :prefix "org-roam-ui-"
  :link '(url-link :tag "Github" "https://github.com/org-roam/org-roam-ui"))

(defvar org-roam-ui-root-dir
  (concat (file-name-directory
           (expand-file-name (or
                    load-file-name
                    buffer-file-name)))
          ".")
  "Root directory of the org-roam-ui project.")

(defvar org-roam-ui-app-build-dir
  (expand-file-name "./out/" org-roam-ui-root-dir)
  "Directory containing org-roam-ui's web build.")

;; TODO: make into defcustom
(defvar org-roam-ui-port
  35901
  "Port to serve the org-roam-ui interface.")

(defcustom org-roam-ui-sync-theme t
  "If true, sync your current Emacs theme with `org-roam-ui'.
Works best with doom-themes.
Ignored if a custom theme is provied for variable 'org-roam-ui-custom-theme'."
  :group 'org-roam-ui
  :type 'boolean)

(defcustom org-roam-ui-custom-theme nil
  "Custom theme for `org-roam-ui'.
Blocks 'org-roam-ui-sync-theme from syncing your current theme,
instead sync this theme.
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
  "If true, `org-roam-ui' will follow you around in the graph."
  :group 'org-roam-ui
  :type 'boolean)

(defcustom org-roam-ui-update-on-save t
  "If true, `org-roam-ui' will send new data when you save an `org-roam' buffer.
This can lead to some jank."
  :group 'org-roam-ui
  :type 'boolean)

(defcustom org-roam-ui-open-on-start t
  "Whether to open your default browser when `org-roam-ui' launces."
  :group 'org-roam-ui
  :type 'boolean)

(defcustom org-roam-ui-find-ref-title t
  "Should `org-roam-ui' use `org-roam-bibtex' to find a reference's title?"
  :group 'org-roam-ui
  :type 'boolean)

(defcustom org-roam-ui-retitle-ref-nodes t
  "Should `org-roam-ui' use `org-roam-bibtex' try to retitle reference nodes?"
  :group 'org-roam-ui
  :type 'boolean)

(defcustom org-roam-ui-ref-title-template
  "%^{author-abbrev} (%^{year}) %^{title}"
  "A template for title creation, used for references without associated nodes.

This uses `orb--pre-expand-template' under the hood and therefore only org-style
capture `%^{...}' are supported."
  :group 'org-roam-ui
  :type 'string)

(defcustom org-roam-ui-browser-function #'browse-url
  "When non-nil launch org-roam-ui with a different browser function.
Takes a function name, such as #'browse-url-chromium.
Defaults to #'browse-url."
  :group 'org-roam-ui
  :type 'function)

;;Hooks

(defcustom org-roam-ui-before-open-node-functions nil
  "Functions to run before a node is opened through org-roam-ui.
Take ID as string as sole argument."
  :group 'org-roam-ui
  :type 'hook)

(defcustom org-roam-ui-after-open-node-functions nil
  "Functions to run after a node is opened through org-roam-ui.
Take ID as string as sole argument."
  :group 'org-roam-ui
  :type 'hook)

(defcustom org-roam-ui-latex-macros nil
  "Alist of LaTeX macros to be passed to org-roam-ui.
Format as, i.e. with double backslashes for a single backslash:
'((\"\\macro\".\"\\something{#1}\"))"
  :group 'org-roam-ui
  :type 'alist)

;; Internal vars

(defvar org-roam-ui--ws-current-node nil
  "Var to keep track of which node you are looking at.")

(defvar org-roam-ui-ws-socket nil
  "The websocket for org-roam-ui.")

(defvar org-roam-ui--window nil
  "The window for displaying nodes opened from within ORUI.
This is mostly to prevent issues with EXWM and the Webkit browser.")

(defvar org-roam-ui-ws-server nil
  "The websocket server for org-roam-ui.")

;;;###autoload
(define-minor-mode
  org-roam-ui-mode
  "Enable org-roam-ui.
This serves the web-build and API over HTTP."
  :lighter " org-roam-ui"
  :global t
  :group 'org-roam-ui
  :init-value nil
  (cond
   (org-roam-ui-mode
   ;;; check if the default keywords actually exist on `orb-preformat-keywords'
   ;;; else add them
    (setq-local httpd-port org-roam-ui-port)
    (setq httpd-root org-roam-ui-app-build-dir)
    (httpd-start)
    (setq org-roam-ui-ws-server
          (websocket-server
           35903
           :host 'local
           :on-open #'org-roam-ui--ws-on-open
           :on-message #'org-roam-ui--ws-on-message
           :on-close #'org-roam-ui--ws-on-close))
    (when org-roam-ui-open-on-start (org-roam-ui-open)))
   (t
    (progn
      (websocket-server-close org-roam-ui-ws-server)
      (httpd-stop)
      (remove-hook 'after-save-hook #'org-roam-ui--on-save)
      (org-roam-ui-follow-mode -1)))))

(defun org-roam-ui--ws-on-open (ws)
  "Open the websocket WS to org-roam-ui and send initial data."
  (progn
    (setq org-roam-ui-ws-socket ws)
    (org-roam-ui--send-variables org-roam-ui-ws-socket)
    (org-roam-ui--send-graphdata)
    (when org-roam-ui-update-on-save
      (add-hook 'after-save-hook #'org-roam-ui--on-save))
    (message "Connection established with org-roam-ui")
    (when org-roam-ui-follow
      (org-roam-ui-follow-mode 1))))

(defun org-roam-ui--ws-on-message (_ws frame)
  "Functions to run when the org-roam-ui server receives a message.
Takes _WS and FRAME as arguments."
  (let* ((msg (json-parse-string
               (websocket-frame-text frame) :object-type 'alist))
         (command (alist-get 'command msg))
         (data (alist-get 'data msg)))
    (cond ((string= command "open")
           (org-roam-ui--on-msg-open-node data))
          ((string= command "delete")
           (org-roam-ui--on-msg-delete-node data))
          ((string= command "create")
           (org-roam-ui--on-msg-create-node data))
          (t
           (message
            "Something went wrong when receiving a message from org-roam-ui")))))

(defun org-roam-ui--on-msg-open-node (data)
  "Open a node when receiving DATA from the websocket."
  (let* ((id (alist-get 'id data))
          (node (org-roam-node-from-id id))
          (pos (org-roam-node-point node))
          (buf (find-file-noselect (org-roam-node-file node))))
    (run-hook-with-args 'org-roam-ui-before-open-node-functions id)
    (unless (window-live-p org-roam-ui--window)
      (if-let ((windows (window-list))
               (or-windows (seq-filter
                            (lambda (window)
                              (org-roam-buffer-p
                               (window-buffer window))) windows))
               (newest-window (car
                               (seq-sort-by
                                #'window-use-time #'> or-windows))))
          (setq org-roam-ui--window newest-window)
        (split-window-horizontally)
        (setq org-roam-ui--window (frame-selected-window))))
    (set-window-buffer org-roam-ui--window buf)
    (select-window org-roam-ui--window)
    (goto-char pos)
    (run-hook-with-args 'org-roam-ui-after-open-node-functions id)))

(defun org-roam-ui--on-msg-delete-node (data)
  "Delete a node when receiving DATA from the websocket.

TODO: Be able to delete individual nodes."
  (progn
    (message "Deleted %s" (alist-get 'file data))
    (delete-file (alist-get 'file data))
    (org-roam-db-sync)
    (org-roam-ui--send-graphdata)))

(defun org-roam-ui--on-msg-create-node (data)
  "Create a node when receiving DATA from the websocket."
  (progn
    (if (and (fboundp #'orb-edit-note) (alist-get 'ROAM_REFS data))
        (orb-edit-note (alist-get 'id data)))
    (org-roam-capture-
     :node (org-roam-node-create :title (alist-get 'title data))
     :props '(:finalize find-file))))

(defun org-roam-ui--ws-on-close (_websocket)
  "What to do when _WEBSOCKET to org-roam-ui is closed."
  (remove-hook 'after-save-hook #'org-roam-ui--on-save)
  (org-roam-ui-follow-mode -1)
  (message "Connection with org-roam-ui closed."))

(defun org-roam-ui--get-text (id)
  "Retrieve the text from org-node ID."
  (let*
      ((node (org-roam-populate (org-roam-node-create
                                 :id id)))
       (file (org-roam-node-file node)))
    (org-roam-with-temp-buffer
        file
      (when (> (org-roam-node-level node) 0)
        ;; Heading nodes have level 1 and greater.
        (goto-char (org-roam-node-point node))
        (org-narrow-to-element))
      (buffer-substring-no-properties (buffer-end -1) (buffer-end 1)))))

(defun org-roam-ui--send-text (id ws)
  "Send the text from org-node ID through the websocket WS."
  (let ((text (org-roam-ui--get-text id)))
    (websocket-send-text ws
                         (json-encode
                          `((type . "orgText")
                            (data . ,text))))))

(defservlet* node/:id text/plain ()
  "Servlet for accessing node content."
  (insert (org-roam-ui--get-text (org-link-decode id)))
  (httpd-send-header t "text/plain" 200 :Access-Control-Allow-Origin "*"))

(defservlet* img/:file text/plain ()
  "Servlet for accessing images found in org-roam files."
  (progn
    (httpd-send-file t (org-link-decode file))
    (httpd-send-header t "text/plain" 200 :Access-Control-Allow-Origin "*")))

(defun org-roam-ui--on-save ()
  "Send graphdata on saving an org-roam buffer.

TODO: Make this only send the changes to the graph data, not the complete graph."
  (when (org-roam-buffer-p)
    (org-roam-ui--send-variables org-roam-ui-ws-socket)
    (org-roam-ui--send-graphdata)))

(defun org-roam-ui--check-orb-keywords ()
  "Check if the default keywords are in `orb-preformat-keywords', if not, add them."
  (when (and org-roam-ui-retitle-ref-nodes (boundp 'orb-preformat-keywords))
    (dolist (keyword '("author-abbrev" "year" "title"))
      (unless (seq-contains-p orb-preformat-keywords keyword)
        (setq orb-preformat-keywords
              (append orb-preformat-keywords (list keyword)))))))

(defun org-roam-ui--find-ref-title (ref)
  "Find the title of the bibtex entry keyed by `REF'.

Requires `org-roam-bibtex' and `bibtex-completion' (a dependency of `orb') to be
loaded. Returns `ref' if an entry could not be found."
  (if (and org-roam-ui-find-ref-title
           (fboundp 'bibtex-completion-get-entry)
           (fboundp 'orb--pre-expand-template)
           (boundp 'orb-preformat-keywords))
      (if-let ((entry (bibtex-completion-get-entry ref))
               (orb-preformat-keywords
                (append orb-preformat-keywords
                        '("author-abbrev" "year" "title"))))
          ;; Create a fake capture template list, only the actual capture at 3
          ;; matters. Interpolate the bibtex entries, and extract the filled
          ;; template from the return value.
          (nth 3 (orb--pre-expand-template
                  `("" "" plain ,org-roam-ui-ref-title-template) entry))
        ref)
    ref))

(defun org-roam-ui--replace-nth (el n lst)
  "Non-destructively replace the `N'th element of `LST' with `EL'."
  (let ((head (butlast lst (- (length lst) n)))
        (tail (nthcdr (+ n 1) lst)))
    (append head (list el) tail)))

(defun org-roam-ui--citekey-to-ref (citekey)
  "Convert a CITEKEY property (most likely with a `cite:' prefix) to just a key.

This method is mostly taken from `org-roam-bibtex'
see https://github.com/org-roam/org-roam-bibtex/blob/919ec8d837a7a3bd25232bdba17a0208efaefb2a/orb-utils.el#L289
but is has been adapted to operate on a sting instead of a node. Requires
`org-ref' to be loaded. Returns the `key' or nil if the format does not match
the `org-ref-cite-re'"
  (if-let ((boundp 'org-ref-cite-re)
           (citekey-list (split-string-and-unquote citekey)))
      (catch 'found
        (dolist (c citekey-list)
          (when (string-match org-ref-cite-re c)
            (throw 'found (match-string 2 c)))))))

(defun org-roam-ui--retitle-node (node)
  "Replace the title of citation NODE with associated notes.

A new title is created using information from the bibliography and formatted
according to `org-roam-ui-ref-title-template', just like the citation nodes with
a note are. It requires `org-roam-bibtex' and it's dependencies
\(`bibtex-completion' and `org-ref'\) to be loaded.

Returns the node with an updated title if the current node is a reference node
and the key was found in the bibliography, otherwise the node is returned
unchanged."
  (if-let* (org-roam-ui-retitle-ref-nodes
            ;; set a fake var because if-let(((boundp 'fake-var))) returns true
            (orcr (boundp 'org-ref-cite-re))
            (citekey (cdr (assoc "ROAM_REFS" (nth 5 node))))
            (ref (org-roam-ui--citekey-to-ref citekey))
            (title (org-roam-ui--find-ref-title ref)))
      (org-roam-ui--replace-nth title 2 node)
    node))

(defun org-roam-ui--create-fake-node (ref)
  "Create a fake node for REF without a source note."
  (list
   ref
   ref
   (org-roam-ui--find-ref-title ref)
   0
   0
   'nil
   `(("ROAM_REFS" . ,(format "cite:%s" ref))
     ("FILELESS" . t))
   'nil))

(defun org-roam-ui--send-graphdata ()
  "Get roam data, make JSON, send through websocket to org-roam-ui."
  (let* ((nodes-names
          [id
           file
           title
           level
           pos
           olp
           properties
           tags])
         (old (not (fboundp 'org-roam-db-map-citations)))
         (links-db-rows (if old
                            (org-roam-ui--separate-ref-links
                             (org-roam-ui--get-links old))
                          (seq-concatenate
                           'list
                           (org-roam-ui--separate-ref-links
                            (org-roam-ui--get-cites))
                           (org-roam-ui--get-links))))
         (links-with-empty-refs (org-roam-ui--filter-citations links-db-rows))
         (empty-refs (delete-dups (seq-map
                                   (lambda (link)
                                     (nth 1 link))
                                   links-with-empty-refs)))
         (nodes-db-rows (org-roam-ui--get-nodes))
         (fake-nodes (seq-map #'org-roam-ui--create-fake-node empty-refs))
           ;; Try to update real nodes that are reference with a title build
           ;; from their bibliography entry. Check configuration here for avoid
           ;; unneeded iteration though nodes.
         (retitled-nodes-db-rows (if org-roam-ui-retitle-ref-nodes
                                    (seq-map #'org-roam-ui--retitle-node
                                             nodes-db-rows)
                                  nodes-db-rows))
         (complete-nodes-db-rows (append retitled-nodes-db-rows fake-nodes))
         (response `((nodes . ,(mapcar
                                (apply-partially
                                 #'org-roam-ui-sql-to-alist
                                 (append nodes-names nil))
                                complete-nodes-db-rows))
                     (links . ,(mapcar
                                (apply-partially
                                 #'org-roam-ui-sql-to-alist
                                 '(source target type))
                                links-db-rows))
                     (tags . ,(seq-mapcat
                               #'seq-reverse
                               (org-roam-db-query
                                [:select :distinct tag :from tags]))))))
    (when old
      (message "[org-roam-ui] You are not using the latest version of org-roam.
This database model won't be supported in the future, please consider upgrading."))
    (websocket-send-text org-roam-ui-ws-socket (json-encode
                                                `((type . "graphdata")
                                                  (data . ,response))))))


(defun org-roam-ui--filter-citations (links)
  "Filter out the citations from LINKS."
  (seq-filter
   (lambda (link)
     (string-match-p "cite" (nth 2 link)))
   links))

(defun org-roam-ui--get-nodes ()
  "."
  (org-roam-db-query [:select [id
                                file
                                title
                                level
                                pos
                                olp
                                properties
                                (funcall group-concat tag
                                         (emacsql-escape-raw \, ))]
                       :as tags
                       :from nodes
                       :left-join tags
                       :on (= id node_id)
                       :group :by id]))

(defun org-roam-ui--get-links (&optional old)
  "Get the cites and links tables as rows from the org-roam db.
Optionally set OLD to t to use the old db model (where the cites
were in the same table as the links)."
(if (not old)
    (org-roam-db-query
     `[:select  [links:source
                 links:dest
                 links:type]
       :from links
       :where (= links:type "id")])
  ;; Left outer join on refs means any id link (or cite link without a
  ;; corresponding node) will have 'nil for the `refs:node-id' value. Any
  ;; cite link where a node has that `:ROAM_REFS:' will have a value.
  (org-roam-db-query
   `[:select [links:source
              links:dest
              links:type
              refs:node-id]
     :from links
     :left :outer :join refs :on (= links:dest refs:ref)
     :where (or
             (= links:type "id")
             (like links:type "%cite%"))])))

(defun org-roam-ui--get-cites ()
  "Get the citations when using the new db-model."
  (org-roam-db-query
   `[:select [citations:node-id citations:cite-key refs:node-id]
     :from citations
     :left :outer :join refs :on (= citations:cite-key refs:ref)]))

(defun org-roam-ui--separate-ref-links (links &optional old)
  "Create separate entries for LINKS with existing reference nodes.
Optionally set OLD to t to support old citations db-model.

Convert any cite links that have nodes with associated refs to an
id based link of type `ref' while removing the 'nil `refs:node-id'
from all other links."

 (if (not old)
    (seq-map
     (lambda (link)
       (pcase-let ((`(,source ,dest ,node-id) link))
         (if node-id
             (list source node-id "ref")
           (list source dest "cite"))))
     links)
   (seq-map
    (lambda (link)
      (pcase-let ((`(,source ,dest ,type ,node-id) link))
        (if node-id
            (list source node-id "ref")
          (list source dest type))))
    links)))

(defun org-roam-ui--update-current-node ()
  "Send the current node data to the web-socket."
  (when (and (websocket-openp org-roam-ui-ws-socket)
             (org-roam-buffer-p)
             (buffer-file-name (buffer-base-buffer)))
    (let* ((node (org-roam-id-at-point)))
      (unless (string= org-roam-ui--ws-current-node node)
        (setq org-roam-ui--ws-current-node node)
        (websocket-send-text org-roam-ui-ws-socket
                             (json-encode `((type . "command")
                                            (data . ((commandName . "follow")
                                                     (id . ,node))))))))))


(defun org-roam-ui--update-theme ()
  "Send the current theme data to the websocket."
  (let  ((ui-theme (list nil)))
    (if org-roam-ui-sync-theme
        (if (boundp 'doom-themes--colors)
            (let*
                ((colors (butlast doom-themes--colors
                                  (- (length doom-themes--colors) 25)))
                 doom-theme)
              (progn
                (dolist (color colors)
                  (push
                   (cons (car color) (car (cdr color)))
                   doom-theme)))
              (setq ui-theme doom-theme))
          (setq ui-theme (org-roam-ui-get-theme)))
      (when org-roam-ui-custom-theme
		(setq ui-theme org-roam-ui-custom-theme)))
    ui-theme))


(defun org-roam-ui--send-variables (ws)
  "Send miscellaneous org-roam variables through the websocket WS."
    (let ((daily-dir (if (boundp 'org-roam-dailies-directory)
                         (if (file-name-absolute-p org-roam-dailies-directory)
                             (expand-file-name org-roam-dailies-directory)
                           (expand-file-name
                            org-roam-dailies-directory
                            org-roam-directory))
                       "/dailies"))
          (attach-dir (if (boundp 'org-attach-id-dir)
                          org-attach-id-dir
                        (expand-file-name ".attach/" org-directory)))
          (use-inheritance (if (boundp 'org-attach-use-inheritance)
                            org-attach-use-inheritance
                            nil))
          (sub-dirs (org-roam-ui-find-subdirectories)))
      (websocket-send-text org-roam-ui-ws-socket
                           (json-encode
                            `((type . "variables")
                              (data .
                                    (("subDirs".
                                      ,sub-dirs)
                                     ("dailyDir" .
                                      ,daily-dir)
                                     ("attachDir" .
                                      ,attach-dir)
                                     ("useInheritance" .
                                      ,use-inheritance)
                                     ("roamDir" . ,org-roam-directory)
                                     ("katexMacros" . ,org-roam-ui-latex-macros))))))))

(defun org-roam-ui-sql-to-alist (column-names rows)
  "Convert sql result to alist for json encoding.
ROWS is the sql result, while COLUMN-NAMES is the columns to use."
  (let (res)
    (while rows
      ;; I don't know how to get the tags as a simple list, so we post process it
      (if (not (string= (car column-names) "tags"))
          (push (cons (pop column-names) (pop rows)) res)
        (push (cons (pop column-names)
                    (seq-remove
                     (lambda (elt) (string= elt ","))
                     rows))
              res)
        (setq rows nil)))
    res))

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
        `(magenta . ,(face-foreground font-lock-preprocessor-face))))

(defun org-roam-ui-find-subdirectories ()
  "Find all the subdirectories in the org-roam directory.
TODO: Exclude org-attach dirs."
   (seq-filter
    (lambda (file) (and (file-directory-p file) (org-roam-ui-allowed-directory-p file)))
    (directory-files-recursively org-roam-directory
                                 ".*" t #'org-roam-ui-allowed-directory-p)))

(defun org-roam-ui-allowed-directory-p (dir)
  "Check whether a DIR should be listed as a filterable dir.
Hides . directories."
  (not (string-match-p "\\(\/\\|\\\\\\)\\..*?"  dir)))

;;;; interactive commands

;;;###autoload
(defun org-roam-ui-open ()
  "Ensure `org-roam-ui' is running, then open the `org-roam-ui' webpage."
  (interactive)
  (unless org-roam-ui-mode (org-roam-ui-mode))
  (funcall org-roam-ui-browser-function
           (format "http://localhost:%d" org-roam-ui-port)))

;;;###autoload
(defun org-roam-ui-node-zoom (&optional id speed padding)
  "Move the view of the graph to current node.
or optionally a node of your choosing.
Optionally takes three arguments:
The ID of the node you want to travel to.
The SPEED in ms it takes to make the transition.
The PADDING around the nodes in the viewport."
  (interactive)
  (if-let ((node (or id (org-roam-id-at-point))))
      (websocket-send-text org-roam-ui-ws-socket
                           (json-encode `((type . "command")
                                          (data . ((commandName . "zoom")
                                                   (id . ,node)
                                                   (speed . ,speed)
                                                   (padding . ,padding))))))
    (message "No node found.")))


;;;###autoload
(defun org-roam-ui-node-local (&optional id speed padding)
  "Open the local graph view of the current node.
Optionally with ID (string), SPEED (number, ms) and PADDING (number, px)."
  (interactive)
  (if-let ((node (or id (org-roam-id-at-point))))
      (websocket-send-text org-roam-ui-ws-socket
                           (json-encode `((type . "command")
                                          (data . ((commandName . "local")
                                                   (id . ,node)
                                                   (speed . ,speed)
                                                   (padding . ,padding))))))
    (message "No node found.")))


(defun org-roam-ui-change-local-graph (&optional id manipulation)
  "Add or remove current node to the local graph. If not in local mode, open local-graph for this node."  
  (interactive)
  (if-let ((node (or id (org-roam-id-at-point))))
      (websocket-send-text org-roam-ui-ws-socket
                           (json-encode `((type . "command")
                                          (data . ((commandName . "change-local-graph")
                                                   (id . ,node)
                                                   (manipulation . ,(or manipulation "add")))))))
    (message "No node found.")))

;;;###autoload
(defun org-roam-ui-add-to-local-graph (&optional id)
  "Add current node to the local graph. If not in local mode, open local-graph for this node."
  (interactive)
  (org-roam-ui-change-local-graph id "add"))

;;;###autoload
(defun org-roam-ui-remove-from-local-graph (&optional id)
  "Remove current node from the local graph. If not in local mode, open local-graph for this node."
  (interactive)
  (org-roam-ui-change-local-graph id "remove"))

;;;###autoload
(defun org-roam-ui-sync-theme ()
  "Sync your current Emacs theme with org-roam-ui."
  (interactive)
  (websocket-send-text org-roam-ui-ws-socket
                       (json-encode `((type . "theme")
                                      (data . ,(org-roam-ui--update-theme))))))

;;; Obsolete commands
(define-obsolete-function-alias #'orui-open #'org-roam-ui-open "0.1")
(define-obsolete-function-alias #'orui-node-local #'org-roam-ui-node-local "0.1")
(define-obsolete-function-alias #'orui-node-zoom #'org-roam-ui-node-zoom "0.1")
(define-obsolete-function-alias #'orui-sync-theme #'org-roam-ui-sync-theme "0.1")

;;;###autoload
(define-minor-mode org-roam-ui-follow-mode
  "Set whether ORUI should follow your every move in Emacs."
  :lighter " org-roam-ui"
  :global t
  :group 'org-roam-ui
  :init-value nil
  (if org-roam-ui-follow-mode
      (progn
        (add-hook 'post-command-hook #'org-roam-ui--update-current-node)
        (message "org-roam-ui will now follow you around."))
    (remove-hook 'post-command-hook #'org-roam-ui--update-current-node)
    (message "org-roam-ui will now leave you alone.")))

(provide 'org-roam-ui)
;;; org-roam-ui.el ends here
