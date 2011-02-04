(require 'cl)
(require 'org)
(require 'org-exp)
(require 'json)

(put 'org-redmine-no-project-id-exists 'error-conditions '(error org-redmine-no-project-id-exists))

(put 'org-redmine-no-project-id-exists 'error-message "project id does not exist in the heading")

(defun org-redmine-to-textile-href (s)
  ""
  (if (string-match "\\[\\[\\(.*?\\)\\]\\[\\(.*?\\)\\]\\]" s)
      (format "\"%s\":%s"
              (substring s (match-beginning 2) (match-end 2))
              (substring s (match-beginning 1) (match-end 1)))
    s))

(defun org-redmine-to-textile-href-recursively (s)
  ""
  (and s (if (string-match "\\(\\[\\[.*?\\]\\]\\)" s)
             (let ((mbeg (match-beginning 1))
                   (mend (match-end 1)))
               (concat (substring s 0 mbeg)
                       (org-redmine-to-textile-href (substring s mbeg mend))
                       (org-redmine-to-textile-href-recursively (substring s mend))))
           s)))

(defun org-redmine-to-textile-filter (buf f)
  ""
  (mapcan '(lambda (line) (list (funcall f line)))
          (org-split-string
           (replace-regexp-in-string    ; skip first heading
            "\\`\\(^[ \t\n]*\\*+\\ .*\n*\\)"
            ""
            (org-export-preprocess-string
             buf
             :emph-multiline t
             :for-html t
             ;;:for-ascii t
             ;;:for-docbook t
             :skip-before-1st-heading t
             ;;  (plist-get opt-plist :skip-before-1st-heading)
             ;;  :drawers (plist-get opt-plist :drawers)
             ;;  :todo-keywords (plist-get opt-plist :todo-keywords)
             ;;  :tags (plist-get opt-plist :tags)
             ;;  :priority (plist-get opt-plist :priority)
             ;;  :footnotes (plist-get opt-plist :footnotes)
             ;;  :timestamps (plist-get opt-plist :timestamps)
             :archived-trees
             ;;  (plist-get opt-plist :archived-trees)
             ;;  :select-tags (plist-get opt-plist :select-tags)
             ;;  :exclude-tags (plist-get opt-plist :exclude-tags)
             :add-text
             ;;  (plist-get opt-plist :text)
             ;;  :LaTeX-fragments
             ;;  (plist-get opt-plist :LaTeX-fragments)
             ) nil nil 1)
           "[\r\n]")))

;;
(defun org-redmine-make-to-textile-line-filter ()
  ""
  (lexical-let ((heading-level 0))
    (lambda (line)
      (catch 'nextline
        ;; skip the row separator of the table
        (when (string-match "^[ \t]*\|[-+]+\|[ \t]*$" line)
          (setq line nil)
          (throw 'nextline nil))
        ;; block
        (when (string-match "<pre\ .*?>" line)
          (setq line (replace-regexp-in-string "<pre\ .*?>" "<pre>" line))
          (throw 'nextline nil))
        ;; heading
        (when (string-match "^\\(\*+\\)\ .*" line)
          (and (zerop heading-level)
               (setq heading-level (- (- (match-end 1) (match-beginning 1)) 1)))
          (setq line
                (format "h%d.%s"
                        (- (- (match-end 1) (match-beginning 1)) heading-level) (substring line (match-end 1))))
          (throw 'nextline nil))
        ;; item
        (when (or (string-match "^\\([ \t]+\\)\\([+-]\\)\\ " line)
                  (string-match "^\\([ \t]+\\)\\([0-9]+[.)]\\)\\ " line))
          (setq line (concat (substring line 0 (match-end 1))
                             "*"
                             (substring line (match-end 2))
                             "\n"))
          (throw 'nextline nil)))
      (org-redmine-to-textile-href-recursively line))))

;;
(defun org-redmine-to-textile (buf)
  ""
  (let ((line-filter (org-redmine-make-to-textile-line-filter)))
    (mapconcat
     'identity
     (remove nil (org-redmine-to-textile-filter buf line-filter))
     "\n")))

;; performs as ruby open()
(defun org-redmine-open-temp-file-and-do (f)
  ""
  (let ((buffer (find-file-noselect (make-temp-file "org-redmine"))))
    (set-buffer buffer)
    (and (fboundp 'set-buffer-file-coding-system)
         (set-buffer-file-coding-system coding-system-for-write))
    (funcall f)
    (save-buffer)
    buffer))

;; based on org-copy-subtree
(defun org-redmine-subtree ()
  ""
  ;; (interactive "p")
  (let (beg end folded (beg0 (point)))
    (if (interactive-p)
        (org-back-to-heading nil)     ; take what looks like a subtree
      (org-back-to-heading t))        ; take what is really there
    (org-back-to-heading t)
    (org-back-over-empty-lines)
    (setq beg (point))
    (skip-chars-forward " \t\r\n")
    (save-match-data
      (save-excursion (outline-end-of-heading)
                      (setq folded (org-invisible-p)))
      (condition-case nil
          (org-forward-same-level (1- n) t)
        (error nil))
      (org-end-of-subtree t t))
    (org-back-over-empty-lines)
    (setq end (point))
    (goto-char beg0)
    (when (> end beg)
      (setq org-subtree-clip-folded folded)
      (buffer-substring-no-properties beg end))))

(defun org-redmine-head-properties ()
  ""
  (let ((properties (make-hash-table :test 'equal))
        (subject (org-get-heading t)))
    (if (string-match "^#\\([0-9]*\\)\\([ \t]+\\).*" subject)
        (progn
          (puthash "id" (substring subject (match-beginning 1) (match-end 1)) properties)
          (puthash "subject" (substring subject (match-end 2)) properties))
      (puthash "subject" subject properties))
    properties))

(defun org-redmine-get-properties ()
  ""
  (let ((properties (org-redmine-head-properties)))
    (map nil '(lambda (property)
                (let ((key (car property))
                      (val (cdr property)))
                      (case (intern key)
                        ('property_id
                         (puthash "property_id" val properties)
                         )
                        ('tracker_id
                         (puthash "tracker_id" val properties)
                         )
                        ('author_id
                         (puthash "author_id" val properties)
                         )
                        ('status_id
                         (puthash "status_id" val properties)
                         ))))
         (org-entry-properties))
    properties))

(defun org-redmine-get-property-value (key properties)
  ""
  (car (mapcan '(lambda (x)
                  (and (equal (car x) key) (list (cdr x))))
               properties)))

(defun org-redmine-html-protect (s)
  "convert & to &amp;, < to &lt; and > to &gt;"
  (let ((start 0))
    (while (string-match "&" s start)
      (setq s (replace-match "&amp;" t t s)
            start (1+ (match-beginning 0))))
    (while (string-match "<" s)
      (setq s (replace-match "&lt;" t t s)))
    (while (string-match ">" s)
      (setq s (replace-match "&gt;" t t s)))
    )
  s)

(defun org-redmine-write-json (body properties)
  ""
  (let ((issue (make-hash-table :test 'equal)))
    (puthash "description" (org-redmine-html-protect (org-redmine-to-textile body)) properties)
    (puthash "issue" properties issue)
    (json-encode issue)))

(defun org-redmine-write-out-temp-file ()
  ""
  (let ((buf (org-redmine-write-json (org-redmine-subtree) (org-redmine-get-properties))))
    (message "Generated JSON:%s" buf)
    (org-redmine-open-temp-file-and-do
     (lambda ()
       (insert buf)))))

(defvar org-redmine-curl-buffer "*Org redmine curl buffer*")

(defvar org-redmine-connect-timeout "8")

(defvar org-redmine-login "kinjo")

(defvar org-redmine-password "muchooza")

(defvar org-redmine-url "http://redmine-dev:3000")

(defun org-redmine-curl-post (data)
  ""
  (condition-case err
      (progn
        (ignore-errors (kill-buffer org-redmine-curl-buffer))
        (call-process "curl" nil `(,org-redmine-curl-buffer nil) nil
                      "-u" (format "%s:%s" org-redmine-login org-redmine-password)
                      "-X" "POST"
                      "-H" "Content-Type:application/json"
                      "-d" (format "@%s" data)
                      "--connect-timeout" org-redmine-connect-timeout
                      (format "%s/issues.json" org-redmine-url))
        (set-buffer org-redmine-curl-buffer)
        (let ((buf (buffer-string))
              (json-object-type 'hash-table))
          (condition-case err
              (json-read-from-string buf)
            (json-readtable-error
             (message "%s: Non JSON data because of a server side exception. See %s" (error-message-string err) org-redmine-curl-buffer)))))
    (file-error (message (format "%s" (error-message-string err))))))

(defun org-redmine-curl-get (issue-id)
  ""
  (condition-case err
      (progn
        (ignore-errors (kill-buffer org-redmine-curl-buffer))
        (call-process "curl" nil `(,org-redmine-curl-buffer nil) nil
                      "-u" (format "%s:%s" org-redmine-login org-redmine-password)
                      "-X" "GET"
                      "-H" "Content-Type:application/json"
                      "--connect-timeout" org-redmine-connect-timeout
                      (format "%s/issues/%s.json" org-redmine-url issue-id))
        (set-buffer org-redmine-curl-buffer)
        (let ((buf (buffer-string))
              (json-object-type 'hash-table))
          (condition-case err
              (json-read-from-string buf)
            (json-readtable-error
             (message "%s: Non JSON data because of a server side exception. See %s" (error-message-string err) org-redmine-curl-buffer)))))
    (file-error (message (format "%s" (error-message-string err))))))

(defun org-redmine-textile-to (s level)
  ""
  s)

(defun org-redmine-insert-subtree-json (issue)
  ""
  (let ((level (car (org-heading-components)))
        (attrs issue))
    (insert (format "%s #%s %s\n"
                    (make-string level ?*)
                    (gethash "id" attrs)
                    (gethash "subject" attrs)))
    (org-set-property "project_id" (format "%s" (gethash "project_id" attrs)))
    (org-set-property "property_id" (format "%s" (gethash "property_id" attrs)))
    (org-set-property "tracker_id" (format "%s" (gethash "tracker_id" attrs)))
    (org-set-property "author_id" (format "%s" (gethash "author_id" attrs)))
    (org-set-property "status_id" (format "%s" (gethash "status_id" attrs)))
    (goto-char (point-max))
    (insert (org-redmine-textile-to (gethash "description" attrs) level))
    (insert "\n")))

(defun org-redmine-update-subtree-json (issue)
  ""
  t)

(defun org-redmine-get ()
  ""
  (interactive)
  (let ((buf (current-buffer))
        (issue (org-redmine-curl-get (read-from-minibuffer "Issue ID:"))))
    (set-buffer buf)
    (org-redmine-insert-subtree-json issue)))

(defun org-redmine-post ()
  ""
  (interactive)
  (condition-case err
      (let* ((tempfile-buffer (org-redmine-write-out-temp-file))
             (data (buffer-file-name tempfile-buffer)))
        (let ((issue (org-redmine-curl-post data)))
          (and (hash-table-p issue)
               (org-redmine-update-subtree-json issue)))
        (kill-buffer tempfile-buffer)
        (delete-file data))
    (org-redmine-no-project-id-exists (message "%s" (error-message-string err)))))

(provide 'org-redmine)
