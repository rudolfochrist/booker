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
     (with-page (:title "Heartbeat")
       (:p "Site is running...")))))

(ht:define-easy-handler (bookmarks-index :uri (match :get "/bookmarks"))
    ()
  (let ((bookmarks (booker/db:all-bookmarks)))
    (with-page (:title "Bookmarks")
      (:a :href "/bookmarks/new" (:button "New bookmark"))
      (if (null bookmarks)
          (:p "No bookmarks")
          (:ul
           (loop for bookmark in bookmarks
                 do (:li (:div (:a :href (getf bookmark :url)
                                   (getf bookmark :title)))
                         (:div (:a :href (format nil "/bookmarks/~D/edit" (getf bookmark :id)) "Edit")
                               (:form :method :post :action (format nil "/bookmarks/~D" (getf bookmark :id))
                                      (:input :type "hidden" :name "_method" :value "delete")
                                      (:input :type "submit" :class "submit-link" :value "Delete"))))))))))

(ht:define-easy-handler (bookmarks-new :uri (match :get "/bookmarks/new"))
    ()
  (with-page (:title "New bookmark")
    (:h1 "New bookmark")
    (bookmark-form nil :method :post :action "/bookmarks")))

(ht:define-easy-handler (bookmarks-create :uri (match :post "/bookmarks"))
    ()
  (booker/db:save-bookmark (params :title) (params :url) "")
  (redirect "/bookmarks"))

(ht:define-easy-handler (bookmarks-edit :uri (match :get "/bookmarks/:id/edit"))
    ()
  (let ((bookmark (booker/db:find-bookmark (params :id))))
    (with-page (:title "Edit bookmark")
      (:h1 "Edit bookmark")
      (bookmark-form bookmark :method :put :action (format nil "/bookmarks/~D" (getf bookmark :id))))))

(ht:define-easy-handler (bookmarks-update :uri (match :put "/bookmarks/:id"))
    ()
  (let ((bookmark (booker/db:find-bookmark (params :id))))
    (setf (getf bookmark :title) (params :title)
          (getf bookmark :url) (params :url))
    (booker/db:update-bookmark bookmark))
  (redirect "/bookmarks"))

(ht:define-easy-handler (bookmarks-destory :uri (match :delete "/bookmarks/:id"))
    ()
  (when (booker/db:find-bookmark (params :id))
    (booker/db:delete-bookmark (params :id))
    (redirect "/bookmarks")))

