;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:booker)

(defmacro with-page ((&key title) &body body)
  `(spinneret:with-html-string
     (:!doctype)
     (:html
      (:head
       (:title ,title)
       (:link :rel "stylesheet" :type "text/css" :href "/application.css"))
      (:body
       (:h2 (:a :href "/bookmarks" "Booker"))
       ,@body))))


(defmacro bookmark-form (bookmark &key method action)
  `(spinneret:with-html
     (:form :method ,(if (member method (list :post :put :patch :delete))
                         :post
                         :get)
            :action ,action
            ,@(when (member method (list :put :patch :delete))
                `((:input :type "hidden" :name "_method" :value ,method)))
            (:label :for "title" "Title:")
            (:input :type "text" :name "title" :autofocus t :value (getf ,bookmark :title ""))
            (:label :for "url" "URL:")
            (:input :type "url" :name "url" :value (getf ,bookmark :url ""))
            (:input :type "submit"))))
