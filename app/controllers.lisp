;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:booker)

;;; views
(djula:add-template-directory (asdf:system-relative-pathname "booker" "app/views/"))
(defparameter +up.html+ (djula:compile-template* "up.html"))
(defparameter +bookmarks-index.html+ (djula:compile-template* "bookmarks-index.html"))

(defun render (view &key status arguments destination)
  (when status
    (setf (ht:return-code*) status))
  (apply #'djula:render-template* view destination
         :flash (ht:session-value :flash)
         arguments))

(defun flash (type message)
  (let* ((session (ht:start-session))
         (flash (ht:session-value :flash session)))
    (setf (ht:session-value :flash session)
          (cons (cons type message) flash))))

;;; delete flash after it is used.
(defmethod ht:handle-request :after (acceptor request)
  (ht:delete-session-value :flash))

(define-condition booker-error (error)
  ((reason :initarg :reason
           :reader booker-error-reason)))

(define-condition http-url-found (booker-error)
  ((url :initarg :url
        :reader http-url-found-url))
  (:default-initargs
   :reason "URLs must use HTTPS"))

(ht:define-easy-handler (up :uri (match :get "/up"))
    ()
  (cond
    ((string= (ht:header-in* "Accept") "application/json")
     (jzon:stringify
      (alexandria:plist-hash-table (list :application :ok))))
    (t
     (render +up.html+))))

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
    (render +bookmarks-index.html+ :arguments (list :bookmarks bookmarks))))

(defun retrieve-bookmark-content (url)
  (when (uiop:string-prefix-p "http://" url)
    (error  'http-url-found :url url))
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
  (handler-case
      (multiple-value-bind (title body)
          (retrieve-bookmark-content (params :url))
        (booker/db:create-bookmark title (params :url) body)
        (flash "info" "Bookmark created!")
        (redirect "/bookmarks"))
    (http-url-found (condition)
      (ht:log-message* :error "Error processing URL ~A" (http-url-found-url condition))
      (flash "danger" (booker-error-reason condition))
      (setf (ht:return-code*) ht:+http-bad-request+)
      (bookmarks-index))))


(ht:define-easy-handler (bookmarks-destory :uri (match :delete "/bookmarks/:id"))
    ()
  (when (booker/db:find-bookmark (params :id))
    (booker/db:delete-bookmark (params :id))
    (flash "info" "Bookmark deleted!")
    (redirect "/bookmarks")))

