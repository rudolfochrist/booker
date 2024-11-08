;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:booker)

(defmacro with-page ((&key title) &body body)
  `(spinneret:with-html-string
     (:html :lang "en"
            (:head
             (:title ,title)
             (:link :rel "stylesheet" :href "/bootstrap.min.css")
             (:link :rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css")
             (:raw (csrf-meta-tags)))
            (:body
             (:div :class "container mt-5"
                   (:h1 :class "my-4"
                        (:a :href "/bookmarks" "Booker"))
                   (:main
                    ,@body))))))

(defun view/bookmarks-index (&key flash bookmarks search-query &allow-other-keys)
  (with-page (:title "Bookmarks")
    (dolist (f flash)
      (:div :class (format nil "alert alert-~A" (first f))
            :role "alert"
            (rest f)))
    (:div
     (:form :method "POST" :action "/bookmarks" :class "row"
            (:raw (hidden-authenticity-token))
            (:div :class "col"
                  (:input :class "form-control" :name "url" :type "url" :value ""))
            (:div :class "col"
                  (:button :type "submit" :class "btn btn-primary" "Add bookmark"))))
    (:div :class "mt-4"
          (:form :method "GET" :action "/bookmarks" :class "row"
                 (:div :class "col"
                       (:input :class "form-control" :name "q" :type "text" :value (or search-query "")))
                 (:div :class "col"
                       (:button :class "btn btn-primary" :type "submit" "Search")
                       (:a :class "btn btn-secondary" :href "/bookmarks" "Reset"))))
    (:div :class "mt-4"
          (if bookmarks
              (:ul :class "list-group"
                   (dolist (bookmark bookmarks)
                     (:li :class "list-group-item list-group-item-action d-flex justify-content-between"
                          (:a :class "link-dark" :href (getf bookmark :url) (getf bookmark :title))
                          (:div :class "d-flex column-gap-2"
                                (:form :method "POST" :action (format nil "/bookmarks/~A" (getf bookmark :id)) :onsubmit "return confirm('Are you sure?')"
                                       (:raw (hidden-authenticity-token))
                                       (:input :name "_method" :type "hidden" :value "delete")
                                       (:input :type "submit" :value "Delete" :class "btn btn-link link-danger"))
                                (:a :class "link-primary mt-2"
                                    :href (format nil "/bookmarks/~A" (getf bookmark :id))
                                    "View")))))
              (:p "No bookmarks")))))

(defun view/bookmarks-show (&key bookmark &allow-other-keys)
  (with-page (:title (getf bookmark :title))
    (:p (getf bookmark :body))))
