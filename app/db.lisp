;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:booker)

(defun connection ()
  `(,(database-name *config*)
    ,(database-user *config*)
    ""
    ,(database-host *config*)
    :port ,(database-port *config*)
    :pooled-p t))

(defun connect-database ()
  (pomo:connect (database-name *config*)
                (database-user *config*)
                ""
                (database-host *config*)
                :pooled-p t))

(defprepared all-bookmarks
    (:order-by
     (:select 'id 'url 'title
       :from 'bookmarks)
     (:desc 'created_at))
    :plists)

(defprepared-with-names find-bookmark (id)
    ((:select 'id 'title 'url
       :from 'bookmarks
       :where (:= 'id '$1))
     id)
    :plist)

(defprepared-with-names find-bookmark-with-body (id)
    ((:select 'id 'title 'url 'body
       :from 'bookmarks
       :where (:= 'id '$1))
     id)
    :plist)

(defprepared-with-names search-bookmarks (q)
    ("
SELECT id, url, title,
       ts_rank(search, websearch_to_tsquery('english', $1)) +
       ts_rank(search, websearch_to_tsquery('simple', $1)) AS rank
FROM bookmarks
WHERE search @@ websearch_to_tsquery('english', $1)
OR search @@ websearch_to_tsquery('simple', $1)
ORDER by rank DESC"
     q)
    :plists)

(defprepared-with-names create-bookmark (title url body)
    ((:insert-into 'bookmarks
      :set 'title '$1  'url '$2 'body '$3
      :returning 'id)
     title url body)
    :single!)

(defprepared-with-names update-bookmark (id title url)
    ((:update 'bookmarks
      :set 'title '$2
      'url '$3
      :where (:= 'id '$1))
     id title url))

(defprepared-with-names delete-bookmark (id)
    ((:delete-from 'bookmarks
      :where (:= 'id '$1))
     id))

(defmacro rundb (&body body)
  `(pomo:with-connection ',(connection)
     ,@body))
