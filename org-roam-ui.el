;;; org-roam-ui.el --- User Interface for Org-roam -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2021 Kirill Rogovoy, Thomas F. K. Jorna

;; author: Kirill Rogovoy, Thomas Jorna
;; URL: https://github.com/org-roam/org-roam-ui
;; Keywords: org-mode, roam
;; Version: 0
;; Package-Requires: ((emacs "27.1") (f "0.17.2") (org-roam "2.0.0") (simple-httpd "20191103.1446") (websocket "20210110.17") (json "1.2"))

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
(require 'org-roam-dailies)
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

(defcustom org-roam-ui-find-ref-title t
  "Should org-roam-ui use `org-roam-bibtex' to try to find the title of a reference in the bibliography?"
  :group 'org-roam-ui
  :type 'boolean)

(defcustom org-roam-ui-retitle-ref-nodes t
  "Should org-roam-ui use `org-roam-bibtex' try to retitle reference nodes?"
  :group 'org-roam-ui
  :type 'boolean)

(defcustom org-roam-ui-ref-title-template "%^{author-abbrev} (%^{year}) %^{title}"
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

(defvar org-roam-ui--ws-current-node nil
  "Var to keep track of which node you are looking at.")

(defvar oru-ws nil
  "The websocket for org-roam-ui.")

(defvar org-roam-ui--window nil
  "The window for displaying nodes opened from within ORUI.
This is mostly to prevent issues with EXWM and the Webkit browser.")

;; (defvar org-roam-ui-ws nil
;;   "The websocket server for org-roam-ui.")

;;;###autoload
(define-minor-mode
  org-roam-ui-mode
  "Enable org-roam-ui.
This serves the web-build and API over HTTP."
  :lighter " org-roam-ui"
  :global t
  :group 'org-roam-ui
  :init-value nil
  (if (fboundp #'org-roam-version)
    (when (and (eq (seq-first (org-roam-version)) 49) (eq (elt (org-roam-version) 1) "63"))
      (message "You are running org-roam %s. Org-roam-ui is only compatible with v2, please upgrade." (org-roam-version))
      (setq org-roam-ui-mode -1))
    (message "Org-roam is either not installed or not running. Please fix this.")
      (setq org-roam-ui-mode -1))
  (cond
   (org-roam-ui-mode
   ;;; check if the default keywords actually exist on `orb-preformat-keywords'
   ;;; else add them
    (setq-local httpd-port org-roam-ui-port)
    (setq httpd-root org-roam-ui/app-build-dir)
    (httpd-start)
    (setq org-roam-ui-ws
        (websocket-server
         35903
         :host 'local
         :on-open (lambda (ws) (progn
            (setq oru-ws ws)
            (org-roam-ui--send-variables ws)
            (org-roam-ui--send-graphdata)
            (when org-roam-ui-update-on-save
            (add-hook 'after-save-hook #'org-roam-ui--on-save))
            (message "Connection established with org-roam-ui")
            (when org-roam-ui-follow
              (org-roam-ui-follow-mode 1))))
        :on-message (lambda (_websocket frame)
                      (let* ((msg (json-parse-string (websocket-frame-text frame) :object-type 'alist))
                             (command (alist-get 'command msg))
                             (data (alist-get 'data msg)))
                (cond ((string= command "open")
                       (let* ((node (org-roam-populate (org-roam-node-create
                                :id (alist-get 'id data))))
                             (pos (org-roam-node-point node))
                             (buf (org-roam-node-find-noselect node)))
                         (unless (window-live-p org-roam-ui--window)
                           (if-let ((windows (window-list))
                                  (or-windows (seq-filter (lambda (window) (org-roam-buffer-p (window-buffer window))) windows))
                                  (newest-window (car (seq-sort-by #'window-use-time #'> or-windows))))
                             (setq org-roam-ui--window newest-window)
                           (split-window-horizontally)
                           (setq org-roam-ui--window (frame-selected-window))))
                        (set-window-buffer org-roam-ui--window buf)
                        (select-window org-roam-ui--window)
                        (goto-char pos)))
                      ((string= command "delete")
                       (progn
                       (message "Deleted %s" (alist-get 'file data))
                       (delete-file (alist-get 'file data))
                       (org-roam-db-sync)
                       (org-roam-ui--send-graphdata)))
                      ((string= command "create")
                       (progn
                        (if (and (fboundp #'orb-edit-note) (alist-get 'ROAM_REFS data))
                            (orb-edit-note (alist-get 'id data)))
                        (org-roam-capture-
                         :node (org-roam-node-create :title (alist-get 'title data))
                         :props '(:finalize find-file))))
                      (t (message "Something went wrong when receiving a message from Org-Roam-UI")))))
         :on-close (lambda (_websocket)
            (remove-hook 'after-save-hook #'org-roam-ui--on-save)
            (org-roam-ui-follow-mode -1)
            (message "Connection with org-roam-ui closed."))))
     (when org-roam-ui-open-on-start (orui-open)))
   (t
    (progn
    (websocket-server-close org-roam-ui-ws)
    (httpd-stop)
    (remove-hook 'after-save-hook #'org-roam-ui--on-save)
    (org-roam-ui-follow-mode -1)))))


(defun org-roam-ui--on-save ()
  "Send graphdata on saving an org-roam buffer."
  (when (org-roam-buffer-p)
    (org-roam-ui--send-graphdata)))


(defun org-roam-ui--check-orb-keywords ()
  "Check if the default keywords are in `orb-preformat-keywords', if not, add them."
  (when (and org-roam-ui-retitle-ref-nodes (boundp 'orb-preformat-keywords))
  (dolist (keyword '("author-abbrev" "year" "title"))
    (unless (seq-contains-p orb-preformat-keywords keyword)
      (setq orb-preformat-keywords (append orb-preformat-keywords (list keyword)))))))

(defun org-roam-ui--find-ref-title (ref)
  "Find the title of the bibtex entry keyed by `REF'.

Requires `org-roam-bibtex' and `bibtex-completion' (a dependency of `orb') to be
loaded. Returns `ref' if an entry could not be found."
  (if (and org-roam-ui-find-ref-title
           (fboundp 'bibtex-completion-get-entry)
           (fboundp 'orb--pre-expand-template)
           (boundp 'orb-preformat-keywords))
      (if-let ((entry (bibtex-completion-get-entry ref))
               (orb-preformat-keywords (append orb-preformat-keywords '("author-abbrev" "year" "title"))))
          ;; Create a fake capture template list, only the actual capture at 3
          ;; matters. Interpolate the bibtex entries, and extract the filled
          ;; template from the return value.
          (nth 3 (orb--pre-expand-template `("" "" plain ,org-roam-ui-ref-title-template) entry))
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
  (list ref ref (org-roam-ui--find-ref-title ref) 0 0 'nil `(("ROAM_REFS" . ,(format "cite:%s" ref)) ("FILELESS" . t)) 'nil))

(defun org-roam-ui--send-graphdata ()
  "Get roam data, make JSON, send through websocket to org-roam-ui."
  (let* ((nodes-columns [id file title level pos olp properties ,(funcall group-concat tag (emacsql-escape-raw \, ))])
         (nodes-names [id file title level pos olp properties tags])
         (links-columns [links:source links:dest links:type])
         (cites-columns [citations:node-id citations:cite-key refs:node-id])
         (nodes-db-rows (org-roam-db-query `[:select ,nodes-columns :as tags
                     :from nodes
                     :left-join tags
                     :on (= id node_id)
                     :group :by id]))
         links-db-rows
         cites-db-rows
         links-with-empty-refs)
         ;; Put this check in until Doom upgrades to the latest org-roam
         (if (fboundp 'org-roam-db-map-citations)
             (setq links-db-rows (org-roam-db-query `[:select ,links-columns
                                             :from links
                                             :where (= links:type "id")])
         ;; Left outer join on refs means any id link (or cite link without a
         ;; corresponding node) will have 'nil for the `refs:node-id' value. Any
         ;; cite link where a node has that `:ROAM_REFS:' will have a value.
                    cites-db-rows (org-roam-db-query `[:select ,cites-columns
                                             :from citations
                                             :left :outer :join refs :on (= citations:cite-key refs:ref)])
         ;; Convert any cite links that have nodes with associated refs to an
         ;; id based link of type `ref' while removing the 'nil `refs:node-id'
         ;; from all other links
                    cites-db-rows (seq-map (lambda (l)
                                   (pcase-let ((`(,source ,dest ,node-id) l))
                                     (if node-id
                                         (list source node-id "ref")
                                       (list source dest "cite")))) cites-db-rows)
                    links-db-rows (append links-db-rows cites-db-rows)
                    links-with-empty-refs (seq-filter (lambda (link) (string-match-p "cite" (nth 2 link))) cites-db-rows))
           (setq links-db-rows (org-roam-db-query `[:select [links:source links:dest links:type refs:node-id]
                                             :from links
                                             :left :outer :join refs :on (= links:dest refs:ref)
                                             :where (or (= links:type "id") (like links:type "%cite%"))])
                 links-db-rows (seq-map (lambda (l)
                                   (pcase-let ((`(,source ,dest ,type ,node-id) l))
                                     (if node-id
                                         (list source node-id "ref")
                                       (list source dest type)))) links-db-rows)
                links-with-empty-refs (seq-filter (lambda (link) (string-match-p "cite" (nth 2 link))) links-db-rows)))
         (let* ((empty-refs (delete-dups (seq-map (lambda (link) (nth 1 link)) links-with-empty-refs)))
                (fake-nodes (seq-map 'org-roam-ui--create-fake-node empty-refs))
         ;; Try to update real nodes that are reference with a title build from
         ;; their bibliography entry. Check configuration here for avoid unneeded
         ;; iteration though nodes.
                (nodes-db-rows (if org-roam-ui-retitle-ref-nodes (seq-map 'org-roam-ui--retitle-node nodes-db-rows) nodes-db-rows))
                (nodes-db-rows (append nodes-db-rows fake-nodes))
                (response `((nodes . ,(mapcar (apply-partially #'org-roam-ui-sql-to-alist (append nodes-names nil)) nodes-db-rows))
                                  (links . ,(mapcar (apply-partially #'org-roam-ui-sql-to-alist '(source target type)) links-db-rows))
                                  (tags . ,(seq-mapcat #'seq-reverse (org-roam-db-query [:select :distinct tag :from tags]))))))
    (websocket-send-text oru-ws (json-encode `((type . "graphdata") (data . ,response)))))))



(defun org-roam-ui--update-current-node ()
  "Send the current node data to the web-socket."
  (when (and (websocket-openp oru-ws) (org-roam-buffer-p) (buffer-file-name (buffer-base-buffer)))
  (let* ((node (org-roam-id-at-point)))
    (unless (string= org-roam-ui--ws-current-node node)
    (setq org-roam-ui--ws-current-node node)
      (websocket-send-text oru-ws (json-encode `((type . "command") (data . ((commandName . "follow") (id . ,node))))))))))


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


(defun org-roam-ui--send-variables (ws)
  "Send org-roam variables through the websocket WS."
  (when (boundp 'org-roam-dailies-directory)
  (websocket-send-text ws (json-encode `((type . "variables")
                                             (data .
                                                   (("dailyDir" . ,(concat org-roam-directory org-roam-dailies-directory))
                                                    ("roamDir" . ,org-roam-directory))))))))

(defun org-roam-ui-sql-to-alist (column-names rows)
  "Convert sql result to alist for json encoding.
ROWS is the sql result, while COLUMN-NAMES is the columns to use."
  (let (res)
    (while rows
      ;; emacsql does not want to give us the tags as a list, so we post process it
      (if (not (string= (car column-names) "tags"))
          (push (cons (pop column-names) (pop rows)) res)
      (push (cons (pop column-names)
                  (seq-remove
                   (lambda (elt) (string= elt ","))
                   rows))
                  res)
      (setq rows nil)))
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
        `(magenta . ,(face-foreground font-lock-preprocessor-face))))


;;;; commands
;;;###autoload
(defun orui-open ()
  "Ensure `org-roam-ui-mode' is enabled, then open the `org-roam-ui' webpage."
  (interactive)
  (or org-roam-ui-mode (org-roam-ui-mode))
  (funcall org-roam-ui-browser-function (format "http://localhost:%d" org-roam-ui-port)))

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
      ((commandName . "local") (id . ,node) (speed . ,speed) (padding . ,padding)))))))
  (message "No node found."))


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
    (message "Org-Roam-UI will now follow you around."))
      (remove-hook 'post-command-hook #'org-roam-ui--update-current-node)
      (message "Org-Roam-UI will now leave you alone.")))


(defun orui-sync-theme ()
  "Sync your current Emacs theme with org-roam-ui."
  (interactive)
  (websocket-send-text oru-ws (json-encode `((type . "theme") (data . ,(org-roam-ui--update-theme))))))

(provide 'org-roam-ui)
;;; org-roam-ui.el ends here
