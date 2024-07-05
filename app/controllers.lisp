;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:booker)


(ht:define-easy-handler (up :uri (match :get "/up"))
    ()
  (cond
    ((string= (ht:header-in* "Accept") "application/json")
     (jzon:stringify
      (alexandria:plist-hash-table (list :application :ok))))
    (t
     (djula:render-template* +up.html+ nil))))

(ht:define-easy-handler (home :uri "/")
    ()
  (bookmarks-index))

(defun apply-search-filter (search-term)
  (if (and (not (null search-term))
           (not (zerop (length search-term))))
      (booker/db:search-bookmarks search-term)
      (booker/db:all-bookmarks)))

(ht:define-easy-handler (bookmarks-index :uri (match :get "/bookmarks"))
    ()
  (let ((bookmarks (apply-search-filter (params :q))))
    (djula:render-template* +bookmarks-index.html+ nil
                            :bookmarks bookmarks)))

(defun retrieve-bookmark-content (url)
  (let* ((html (dexador:get url))
         (dom (plump:parse html))
         (main (or (first (plump:get-elements-by-tag-name dom "main"))
                   (first (plump:get-elements-by-tag-name dom "body")))))
    (dolist (script (plump:get-elements-by-tag-name main "script"))
      (plump:remove-child script))
    (dolist (style (plump:get-elements-by-tag-name main "style"))
      (plump:remove-child style))
    (dolist (iframe (plump:get-elements-by-tag-name main "iframe"))
      (plump:remove-child iframe))
    (values (plump:render-text (first (plump:get-elements-by-tag-name dom "title")))
            (plump:render-text main))))

(ht:define-easy-handler (bookmarks-create :uri (match :post "/bookmarks"))
    ()
  (multiple-value-bind (title body)
      (retrieve-bookmark-content (params :url))
    (booker/db:create-bookmark title (params :url) body))
  (redirect "/bookmarks"))


(ht:define-easy-handler (bookmarks-destory :uri (match :delete "/bookmarks/:id"))
    ()
  (when (booker/db:find-bookmark (params :id))
    (booker/db:delete-bookmark (params :id))
    (redirect "/bookmarks")))

