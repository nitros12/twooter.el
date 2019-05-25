;;; twooter.el --- Twooter for emacs -*- lexical-binding: t; -*-

;; Author: Ben Simms <ben@bensimms.moe>
;; URL: https://github.com/nitros12/twooter.el
;; Version: 20190524.1
;; Package-Requires: ((emacs "26.1") (cl-lib "0.6") (names "20180321") (dash "2.16.0") (dash-functional "1.2.0") (s "1.12.0") (request "20181129") (aio "1.0"))

;;; Commentary:
;;
;; This is a simple Emacs client for twooter

;;; License
;;
;; MIT License
;;
;; Copyright (c) 2019 Ben Simms
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


;;; News:
;;; Code:

(require 'eieio)
(require 'json)
(require 'aio)
(require 'request)
(require 'cl-lib)
(require 'names)
(require 'dash)
(require 'dash-functional)
(require 'component)
(require 's)

(defgroup twooter nil
  "Emacs Twooter Client"
  :prefix "twooter-"
  :group 'tools)

(defcustom twooter-api-base "http://twooter.johnvidler.co.uk"
  "Base api endpoint for twooter."
  :type 'string
  :group 'twooter)

(define-error 'twooter-bad-http "Got a bad response from twooter")

(defun request-aio (url &rest args)
  "Send a request to URL with args ARGS wrapped in aio."
  (let* ((p (aio-promise))
         (callback-success (cl-function
                            (lambda (&key data &allow-other-keys)
                              (let ((data- data))
                                (aio-resolve p (lambda () data-))))))
         (callback-error (cl-function
                          (lambda (&key error-thrown &allow-other-keys&rest _)
                            (let ((error-thrown- error-thrown))
                              (aio-resolve p (lambda () (signal 'twooter-bad-http error-thrown-))))))))
    (--> args
        (plist-put it :success callback-success)
        (plist-put it :error callback-error)
        (setq args it))

    (apply #'request url args)
    p))


(define-derived-mode twooter-mode special-mode "Twooter"
  "Twooter mode for viewing twooter buffers."
  (setq-local buffer-read-only t))

(define-namespace twooter-
(defclass client ()
  ((name :initarg :name
         :type string
         :custom string
         :documentation "The user to interface with the twooter api as.")
   (token :initarg :token
          :type string
          :custom string
          :documentation "The token for this user's account."))
  "Represents a twooter client")

(aio-defun req (endpoint &rest args)
  (--> args
       (plist-put it :type "POST")
       (if (plist-member it :data)
           (plist-put it :data (json-encode (plist-get it :data)))
         it)
       (if (plist-member it :parser)
           it
         (plist-put it :parser 'buffer-string))
       (plist-put it :headers '(("User-Agent" . "Emacs")
                                ("Content-Type" . "application/json")))
       (setq args it))
  (aio-await (apply #'request-aio (s-concat twooter-api-base "/" endpoint) args)))

(aio-defun req-json (endpoint &rest args)
  (--> args
       (plist-put it :parser 'json-read)
       (setq args it))
  (aio-await (apply #'req endpoint args)))

(aio-defun register-name (name)
  "Register a name on twooter"
  (let ((token (aio-await
                (req "registerName"
                     :data `(("name" . ,name))))))
    (message "token: %S" token)
    (client :name name :token token)))

(cl-defmethod refresh-name ((c client))
  "Refresh a name on twooter."
  (req "refreshName"
       :data `(("name" . ,(oref c name))
               ("token" . ,(oref c token)))))

(cl-defmethod post-message ((c client) message)
  "Post a message to twooter."
  (req "postMessage"
       :data `(("token" . ,(oref c token))
               ("name" . ,(oref c name))
               ("message" . ,message))))

(defun get-messages ()
  "Get the latest 30 messages or so."
  (req-json "messages"))

(defun transform-image (text)
  "Transform a base64 encoded image TEXT into an image."
  (pcase (s-match "data:image/\\w+;base64,\\(.+\\)" text)
    (`(,_ ,content)
     (create-image (base64-decode-string content) nil t))))

(defun transform-message-content (content)
  "Transform a message CONTENT string into a tui message."
  (pcase (s-match "data:twooter/formatted;json,\\(.+\\)" content)
    (`(,_ ,json-content-)
     (let* ((json-content (json-read-from-string json-content-))
            (agent (alist-get 'agent json-content))
            (images (-non-nil
                     (cl-map 'list #'transform-image (alist-get 'images json-content))))
            (text (alist-get 'text json-content)))
       `(twooter-image-message ,agent ,images ,text)))

    (_ `(line ,(s-word-wrap 70 content)))))

(component-define twooter-image-image (image)
  `((image ,image)
    (padding)))

(component-define twooter-image-message (agent images text)
  `(,@(-map (lambda (im) `(twooter-image-image ,im)) images)
    ,@(when text `((line ,(s-word-wrap 70 text))))
    ,@(when agent `((line (propertize (face magit-dimmed) ,(s-concat "Agent: " agent)))
                    (padding)))))

(component-define twooter-message (author message)
  `((heading ,author)
    (indent ,message)))

(component-define twooter-message-list (state)
  `(section (root)
            ,@(-interpose
               '(padding)
               (cl-map 'list (lambda (msg)
                               `(twooter-message ,(alist-get 'name msg)
                                                 ,(twooter-transform-message-content
                                                   (decode-coding-string (alist-get 'message msg) 'utf-8))))
                       (gethash 'messages state)))))

(defun twooter-render (buf state)
  "Render twooter BUF with STATE when active."
  (when (buffer-live-p buf)
    (component-render buf `(twooter-message-list ,state))))

(defun show-messages ()
  "Show twooter messages in a buffer."
  (interactive)

  (let ((messages (aio-wait-for (get-messages)))
        (buf (get-buffer-create "*twooter*"))
        (state (make-hash-table)))

    (puthash 'messages messages state)
    (with-current-buffer buf
      (twooter-mode))

    (twooter-render buf state)
    (display-buffer buf))))

(provide 'twooter)
;;; twooter.el ends here
