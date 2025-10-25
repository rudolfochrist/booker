;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:booker)

(defun render (view &rest arguments &key status &allow-other-keys)
  (when status
    (setf (ht:return-code*) status))
  (apply view :flash (ht:session-value :flash) arguments))

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

(define-condition no-url-found (booker-error)
  ()
  (:default-initargs
   :reason "URL is nil or empty string"))

(ht:define-easy-handler (up :uri (match :get "/up/?"))
    ()
  (cond
    ((string= (ht:header-in* "Accept") "application/json")
     (jzon:stringify
      (alexandria:plist-hash-table (list :application :ok))))
    (t
     (with-page (:title "Heartbeat")
       (:p "Site is running")))))

(ht:define-easy-handler (home :uri "/")
    ()
  (bookmarks-index))

(defun apply-search-filter (search-term)
  (if (and (not (null search-term))
           (not (zerop (length search-term))))
      (rundb (search-bookmarks search-term))
      (rundb (all-bookmarks))))

(ht:define-easy-handler (bookmarks-index :uri (match :get "/bookmarks/?"))
    ()
  (let ((bookmarks (apply-search-filter (params :q))))
    (render 'view/bookmarks-index :bookmarks bookmarks :search-query (params :q))))

(defun retrieve-bookmark-content (url)
  (when (or (null url)
            (zerop (length url)))
    (error 'no-url-found))
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

(ht:define-easy-handler (bookmarks-create :uri (match :post "/bookmarks/?"))
    ()
  (handler-case
      (multiple-value-bind (title body)
          (retrieve-bookmark-content (params :url))
        (rundb (create-bookmark title (params :url) body))
        (flash "info" "Bookmark created!")
        (redirect "/bookmarks"))
    (http-url-found (condition)
      (ht:log-message* :error "Error processing URL ~A" (http-url-found-url condition))
      (flash "danger" (booker-error-reason condition))
      (setf (ht:return-code*) ht:+http-unprocessable-content+)
      (bookmarks-index))
    (no-url-found ()
      (ht:log-message* :error "No URL send")
      (flash "danger" "Please provide a URL!")
      (setf (ht:return-code*) ht:+http-unprocessable-content+)
      (bookmarks-index))
    (dexador.error:http-request-forbidden ()
      (ht:log-message* :error "403 Forbidden: ~A" (params :url))
      (flash "danger" "Cannot save URL - Server responded: 403 Access Forbidden.")
      (setf (ht:return-code*) ht:+http-unprocessable-content+)
      (bookmarks-index))
    (cl-postgres-error:unique-violation ()
      (flash "info" "Site already bookmarked!")
      (setf (ht:return-code*) ht:+http-unprocessable-content+)
      (bookmarks-index))))

(ht:define-easy-handler (bookmarks-show :uri (match :get "/bookmarks/:id/?"))
    ()
  (uiop:if-let ((bookmark (rundb (find-bookmark-with-body (params :id)))))
    (render 'view/bookmarks-show :bookmark bookmark)
    (format nil "~A" (setf (ht:return-code*) ht:+http-not-found+))))


(ht:define-easy-handler (bookmarks-destory :uri (match :delete "/bookmarks/:id/?"))
    ()
  (when (rundb (find-bookmark (params :id)))
    (rundb (delete-bookmark (params :id)))
    (flash "info" "Bookmark deleted!")
    (redirect "/bookmarks")))

