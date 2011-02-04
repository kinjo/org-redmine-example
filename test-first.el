(expectations

  (desc "org-redmine-insert-subtree-json inserts a subtree content")
  (expect "* issues
** #1235 heading
** #1234 heading
   :PROPERTIES:
   :project_id: 1
   :property_id: 1
   :tracker_id: 1
   :author_id: 1
   :status_id: 1
   :END:
description
"
    (let ((default-major-mode 'org-mode)
            (buf (get-buffer-create "*test*")))
        (set-buffer-major-mode buf)
        (set-buffer buf)
        (erase-buffer)
        (insert "* issues
** #1235 heading
")
        (let ((issue (make-hash-table :test 'equal)))
          (puthash "tracker_id" 1 issue)
          (puthash "author_id" 1 issue)
          (puthash "status_id" 1 issue)
          (puthash "property_id" 1 issue)
          (puthash "subject" "heading" issue)
          (puthash "id" 1234 issue)
          (puthash "project_id" 1 issue)
          (puthash "description" "description" issue)
          (org-redmine-insert-subtree-json issue)
          (buffer-string))))

  (desc "org-redmine-to-textile should skip first heading")
  (expect "xxx"
    (org-redmine-to-textile "\t
* first heading
xxx"))

  (desc "org-redmine-to-textile should return headings without first")
  (expect "h1. sub heading1\nh1. sub heading2\nh2. sub sub heading"
    (org-redmine-to-textile "\t
* heading
** sub heading1
** sub heading2
*** sub sub heading"))

  (desc "org-redmine-to-textile should convert block to pre tag")
  (expect "<pre>plain\n</pre>\n\n\n"
    (org-redmine-to-textile "#+begin_src text
  plain
#+end_src"))

  (desc "org-redmine-head-properties gets subject")
  (expect "heading"
    (with-mock
      (stub org-get-heading => "#1234 heading")
      (gethash "subject" (org-redmine-head-properties))))

  (desc "org-redmine-head-properties gets issue id")
  (expect "1234"
    (with-mock
      (stub org-get-heading => "#1234 heading")
      (gethash "id" (org-redmine-head-properties))))

;;   (desc "org-redmine-head-properties gets project id")
;;   (expect "org-redmine"
;;     (with-mock
;;       (stub org-get-heading => "org-redmine#1234 heading")
;;       (org-redmine-ticket-properties-project-id (org-redmine-head-properties (make-org-redmine-ticket-properties)))))

  (desc "org-redmine-head-properties gets subject when the heading does not have issue id")
  (expect "heading"
    (with-mock
      (stub org-get-heading => "heading")
      (gethash "subject" (org-redmine-head-properties))))

;;   (desc "org-redmine-head-properties gets project id when the heading does not have issue id")
;;   (expect "org-redmine"
;;     (with-mock
;;       (stub org-get-heading => "org-redmine# heading")
;;       (org-redmine-ticket-properties-project-id (org-redmine-head-properties (make-org-redmine-ticket-properties)))))

  (desc "org-redmine-head-properties gets no issue id when the heading does not have issue id")
  (expect nil
    (with-mock
      (stub org-get-heading => "heading")
      (gethash "id" (org-redmine-head-properties))))

;;   (desc "org-redmine-head-properties raise an error when the heading does not have project id")
;;   (expect "project id does not exist in the heading"
;;     (with-mock
;;       (stub org-get-heading => "heading")
;;       (condition-case err
;;           (org-redmine-head-properties (make-org-redmine-ticket-properties))
;;         (org-redmine-no-project-id-exists (error-message-string err)))))

  (desc "org-redmine-to-textile should skip clock")
  (expect "xxx"
    (org-redmine-to-textile "* heading
    CLOCK: [2010-09-23 木 01:54]--[2010-09-23 木 01:54] =>  0:00
xxx"))

  (desc "org-redmine-to-textile should skip properties")
  (expect "xxx"
    (org-redmine-to-textile "* heading
    :PROPERTIES:
    :redmine-property_id: 123
    :redmine-tracker_id: 123
    :redmine-author_id: 123
    :redmine-status_id: 123
    :END:
xxx"))

  (desc "org-redmine-get-properties should get expected properties")
  (expect "999"
    (with-mock
      (stub org-redmine-head-properties => (make-hash-table :test 'equal))
      (stub org-entry-properties => '(("tracker_id" . "999")))
      (gethash "tracker_id" (org-redmine-get-properties))))

  (desc "org-redmine-get-properties should return the value involves result of org-redmine-head-properties")
  (expect "1234"
    (with-mock
      (stub org-get-heading => "#1234 heading")
            (stub org-entry-properties => '(("tracker_id" . "1")))
      (gethash "id" (org-redmine-get-properties))))

  (desc "org-redmine-write-json returns expected json string")
  (expect "{\"issue\":{\"description\":\"xxx\", \"status_id\":\"4\", \"author_id\":\"3\", \"tracker_id\":\"2\", \"property_id\":\"1\", \"subject\":\"heading\", \"id\":\"1234\"}}"
    (with-mock
      (stub org-get-heading => "#1234 heading")
      (stub org-entry-properties => '(("property_id" . "1")
                                      ("tracker_id" . "2")
                                      ("author_id" . "3")
                                      ("status_id" . "4")))
      (org-redmine-write-json "* #1234 heading
 :PROPERTIES:
 :VISIBILITY: folded:
 :END:

xxx
"
                              (org-redmine-get-properties))))
)
