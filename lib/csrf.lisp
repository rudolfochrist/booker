;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; ----------------------------------------------------------------
;;;
;;; Most of this has been ported from Ruby's Rack, Rails and Sinatra
;;;
;;; ----------------------------------------------------------------

(in-package #:booker)

(defvar +authenticity-token-length+ 32)
(defvar +global-csrf-token-identifier+ "!real_csrf_token")

(defun generate-csrf-token ()
  (base64:usb8-array-to-base64-string
   (crypto:random-data +authenticity-token-length+)
   :uri t))

(defun encode-csrf-token (token)
  (base64:usb8-array-to-base64-string token :uri t))

(defun decode-csrf-token (encoded-token)
  (base64:base64-string-to-usb8-array encoded-token :uri t))

(defun real-csrf-token (session)
  (let ((token (or (hunchentoot:session-value :csrf-token session)
                   (setf (hunchentoot:session-value :csrf-token session)
                         (generate-csrf-token)))))
    (decode-csrf-token token)))

(defun csrf-token-hmac (session identifier)
  (let ((mac (crypto:make-mac :hmac (real-csrf-token session) :sha256)))
    (crypto:update-mac mac (crypto:ascii-string-to-byte-array identifier))
    (crypto:produce-mac mac)))

(defun global-csrf-token (session)
  (csrf-token-hmac session +global-csrf-token-identifier+))

(defun mask-token (raw-token)
  (let* ((one-time-pad (crypto:random-data +authenticity-token-length+))
         (xor (crypto:make-cipher :xor :key one-time-pad :mode :ecb))
         (encrypted-token (crypto:encrypt-message xor raw-token))
         (masked-token (concatenate '(vector (unsigned-byte 8))
                                    one-time-pad
                                    encrypted-token)))
    (encode-csrf-token masked-token)))


(defun unmask-token (masked-token)
  (let* ((one-time-pad (subseq masked-token 0 +authenticity-token-length+))
         (encrypted-token (subseq masked-token +authenticity-token-length+))
         (xor (crypto:make-cipher :xor :key one-time-pad :mode :ecb)))
    (crypto:decrypt-message xor encrypted-token)))


(defun form-authenticity-token (&optional session)
  (let ((the-session (or session (hunchentoot:start-session))))
    (mask-token (global-csrf-token the-session))))


(defvar +null-origin-message+
  "The browser returned a 'null' origin for a request with
origin-based forgery protection turned on. This usually means
you have the 'no-referrer' Referrer-Policy header enabled, or
that the request came from a site that refused to give its
origin. This makes it impossible for us to verify the
source of the requests. Likely the best solution is to change
your referrer policy to something less strict like same-origin
or strict-origin.  If you cannot change the referrer policy,
you can disable origin checking with the
\"forgery-protection-origin-check setting.")

(defvar +allowed-schemes+ (list "https" "http" "wss" "ws"))
(defvar +default-ports+ '(("http" . 80) ("https" . 443)))

(defun request-origin (request)
  (hunchentoot:header-in "Origin" request))

(defun scheme (request)
  (cond
    ((hunchentoot:ssl-p (hunchentoot:request-acceptor request))
     "https")
    ((member (ht:header-in :x-forwarded-proto request) +allowed-schemes+ :test 'string=)
     (ht:header-in :x-forwarded-proto request))
    (t
     "http")))

(defun split-authority (authority)
  (destructuring-bind (host &optional port)
      (uiop:split-string authority :separator '(#\:))
    (list authority
          host
          (when port
            (parse-integer port)))))

(defun host-with-port (request)
  (destructuring-bind (authority host &optional port)
      (split-authority
       (or (ht:header-in :x-forwarded-host request)
           (ht:header-in :host request)
           (let ((acceptor (hunchentoot:request-acceptor request)))
             (list (hunchentoot:acceptor-address acceptor)
                   (hunchentoot:acceptor-port acceptor)))))
    (unless port
      (setf port (cdr (assoc (scheme request) +default-ports+ :test #'string=))))
    (if (= port
           (cdr (assoc (scheme request) +default-ports+ :test #'string=)))
        host
        authority)))

(defun request-base-url (request)
  (format nil "~A://~A" (scheme request) (host-with-port request)))

(defun valid-request-origin-p (request)
  (if *forgery-protection-origin-check*
      (if (or (null (request-origin request))
              (string= (request-origin request) "null"))
          (error +null-origin-message+)
          (string= (request-origin request)
                   (request-base-url request)))
      t))

(defun valid-authenticity-token-p (session encoded-masked-token)
  (when (and (not (null encoded-masked-token))
             (typep encoded-masked-token 'string)
             (not (zerop (length encoded-masked-token))))
    (let ((masked-token (decode-csrf-token encoded-masked-token)))
      (crypto:constant-time-equal (unmask-token masked-token)
                                  (global-csrf-token session)))))

(defun any-authenticity-token-valid-p (request session)
  (some (lambda (token)
          (valid-authenticity-token-p session token))
        (list (ht:post-parameter "authenticity_token" request)
              (hunchentoot:header-in "X-CSRF-TOKEN" request))))

(defun verified-request-p (request session)
  (or (eq (hunchentoot:request-method request) :get)
      (eq (hunchentoot:request-method request) :head)
      (and (valid-request-origin-p request)
           (any-authenticity-token-valid-p request session))))

(defvar *xhr-regex* (ppcre:create-scanner "XMLHttpRequest" :case-insensitive-mode t))

(defun xhrp (request)
  (ppcre:scan *xhr-regex* (hunchentoot:header-in "X-REQUESTED-WITH" request)))

(defun xhrp* (&optional (request hunchentoot:*request*))
  (xhrp request))


(defvar *embedded-sript-warning-message*
  "Security warning: an embedded <script> tag on another site requested protected JavaScript. If you know what you're doing, go ahead and disable forgery protection on this action to permit cross-origin JavaScript embedding.")

(defmethod hunchentoot:acceptor-dispatch-request :before ((acceptor hunchentoot:easy-acceptor) request)
  (when *protect-against-forgery*
    (let ((session (hunchentoot:start-session)))
      (unless (verified-request-p request session)
        (hunchentoot:log-message* :security-warning "Can't verify CSRF token authenticity")
        (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+)
        (hunchentoot:abort-request-handler)))))

(defmethod hunchentoot:acceptor-dispatch-request :after ((acceptor hunchentoot:easy-acceptor) request)
  (when (and *protect-against-forgery*
             (eq (hunchentoot:request-method*) :get)
             (ppcre:scan "\\A(?:text|application)/javascript" (hunchentoot:content-type*))
             (not (xhrp*)))
    (hunchentoot:log-message* :security-warning *embedded-sript-warning-message*)
    (status hunchentoot:+http-unprocessable-content+)))
