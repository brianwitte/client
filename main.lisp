;;;; HTTP Client Example using Dexador
;;;; main.lisp

;;; ASDF System Definition and Loading
;;; First, ensure you have Quicklisp installed and Dexador available

;;#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;;; Load required systems
(ql:quickload '(:dexador :cl-json :alexandria))

;;; Package definition
(defpackage :http-client-example
  (:use :cl :dexador :alexandria)
  (:export #:main
           #:get-request
           #:post-request
           #:put-request
           #:delete-request
           #:handle-response))

(in-package :http-client-example)

;;; Error handling utilities
(define-condition http-request-error (error)
  ((status-code :initarg :status-code :reader status-code)
   (message :initarg :message :reader message)
   (url :initarg :url :reader url))
  (:report (lambda (condition stream)
             (format stream "HTTP Request Error (~A): ~A for URL: ~A"
                     (status-code condition)
                     (message condition)
                     (url condition)))))

(dexador:get "https://google.com")

;;; Response handling
(defun handle-response (body status headers uri)
  "Handle HTTP response with proper error checking and logging."
  (format t "~&Response Status: ~A~%" status)
  (format t "URL: ~A~%" uri)
  (format t "Content-Type: ~A~%" (gethash "content-type" headers))
  
  (cond
    ((< status 200)
      (format t "Informational response: ~A~%" body))
    ((< status 300)
      (format t "Success! Response body:~%~A~%" body))
    ((< status 400)
      (format t "Redirection: ~A~%" body))
    ((< status 500)
      (error 'http-request-error
             :status-code status
             :message "Client error"
             :url uri))
    (t
      (error 'http-request-error
            :status-code status
             :message "Server error"
             :url uri)))
  
  (values body status headers))

;;; HTTP request functions with best practices

(defun get-request (url &key headers (timeout 30))
  "Perform GET request with error handling and timeout."
  (handler-case
      (multiple-value-bind (body status response-headers uri)
          (dex:get url
                   :headers headers
                   :timeout timeout
                   :want-stream nil)
        (handle-response body status response-headers uri))
    (dex:http-request-error (e)
      (format t "HTTP Request failed: ~A~%" e)
      (values nil (dex:response-status e) nil))
    (error (e)
      (format t "General error: ~A~%" e)
      (values nil nil nil))))

(defun post-request (url data &key headers (content-type "application/json") (timeout 30))
  "Perform POST request with JSON data."
  (let ((default-headers `(("Content-Type" . ,content-type)
                          ("User-Agent" . "Lisp HTTP Client/1.0"))))
    (handler-case
        (multiple-value-bind (body status response-headers uri)
            (dex:post url
                      :content (if (stringp data)
                                   data
                                   (cl-json:encode-json-to-string data))
                      :headers (append headers default-headers)
                      :timeout timeout)
          (handle-response body status response-headers uri))
      (dex:http-request-error (e)
        (format t "HTTP Request failed: ~A~%" e)
        (values nil (dex:response-status e) nil))
      (error (e)
        (format t "General error: ~A~%" e)
        (values nil nil nil)))))

(defun put-request (url data &key headers (content-type "application/json") (timeout 30))
  "Perform PUT request with JSON data."
  (let ((default-headers `(("Content-Type" . ,content-type)
                          ("User-Agent" . "Lisp HTTP Client/1.0"))))
    (handler-case
        (multiple-value-bind (body status response-headers uri)
            (dex:put url
                     :content (if (stringp data)
                                  data
                                  (cl-json:encode-json-to-string data))
                     :headers (append headers default-headers)
                     :timeout timeout)
          (handle-response body status response-headers uri))
      (dex:http-request-error (e)
        (format t "HTTP Request failed: ~A~%" e)
        (values nil (dex:response-status e) nil))
      (error (e)
        (format t "General error: ~A~%" e)
        (values nil nil nil)))))

;; (defun delete-request (url &key headers (timeout 30))
;;   "Perform DELETE request."
;;   (handler-case
;;       (multiple-value-bind (body status response-headers uri)
;;           (dex:delete url
;;                       :headers headers
;;                       :timeout timeout)
;;         (handle-response body status response-headers uri))
;;     (dex:http-request-error (e)
;;       (format t "HTTP Request failed: ~A~%" e)
;;       (values nil (dex:response-status e) nil))
;;     (error (e)
;;       (format t "General error: ~A~%" e)
;;       (values nil nil nil))))

;;; Example usage functions
(defun demo-get-json-api ()
  "Demonstrate GET request to a JSON API."
  (format t "~&=== GET Request Demo ===~%")
  (get-request "https://httpbin.org/json"
               :headers '(("Accept" . "application/json"))))

(defun demo-post-json ()
  "Demonstrate POST request with JSON data."
  (format t "~&=== POST Request Demo ===~%")
  (let ((data '(("name" . "John Doe")
                ("email" . "john@example.com")
                ("age" . 30))))
    (post-request "https://httpbin.org/post" data)))

(defun demo-error-handling ()
  "Demonstrate error handling with invalid URL."
  (format t "~&=== Error Handling Demo ===~%")
  (get-request "https://httpbin.org/status/404"))

(defun demo-timeout ()
  "Demonstrate timeout handling."
  (format t "~&=== Timeout Demo ===~%")
  (get-request "https://httpbin.org/delay/5" :timeout 2))

;;; Main function
(defun main ()
  "Main entry point demonstrating HTTP client usage."
  (format t "HTTP Client Example using Dexador~%")
  (format t "=====================================~%")
  
  ;; Run demonstrations
  (demo-get-json-api)
  (terpri)
  
  (demo-post-json)
  (terpri)
  
  (demo-error-handling)
  (terpri)
  
  (demo-timeout)
  (terpri)
  
  (format t "~&All demos completed!~%"))

;;; Usage Instructions:
;;; 
;;; 1. Install Quicklisp if not already installed:
;;;    (load "quicklisp.lisp")
;;;    (quicklisp-quickstart:install)
;;;
;;; 2. Load this file in your Lisp REPL:
;;;    (load "main.lisp")
;;;
;;; 3. Run the main function:
;;;    (http-client-example:main)
;;;
;;; 4. Or use individual functions:
;;;    (http-client-example:get-request "https://api.github.com/users/octocat")
;;;    (http-client-example:post-request "https://httpbin.org/post" '(("key" . "value")))
;;;
;;; Alternative ASDF approach:
;;; Create a .asd file for a proper system definition:
;;;
;;; (defsystem "http-client-example"
;;;   :description "HTTP Client Example using Dexador"
;;;   :author "Your Name"
;;;   :license "MIT"
;;;   :depends-on (:dexador :cl-json :alexandria)
;;;   :components ((:file "main")))
;;;
;;; Then load with: (asdf:load-system :http-client-example)

;;; If running from command line with SBCL:
;;; sbcl --load main.lisp --eval "(http-client-example:main)" --quit
