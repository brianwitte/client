
;;;; Simple HTTP Client Example - KISS Style
;;;; For teaching basic Common Lisp HTTP operations

;;; Package management (keep as-is)
;;#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload '(:dexador :cl-json))

(defpackage :simple-http
  (:use :cl)
  (:export #:main #:fetch-users #:get-user-names))

(in-package :simple-http)

;;; Fetch JSON data from API
(defun fetch-users ()
  "Get users from JSONPlaceholder API"
  (let ((response (dexador:get "https://jsonplaceholder.typicode.com/users")))
    (cl-json:decode-json-from-string response)))

;;; Extract and sort user names
(defun get-user-names (users)
  "Extract names from user list and sort them"
  (sort (mapcar (lambda (user)
                  (cdr (assoc :name user)))
                users)
        #'string<))

;;; Print formatted output
(defun print-names (names)
  "Print names in a simple format"
  (format t "~&Users (sorted):~%")
  (dolist (name names)
    (format t "  ~A~%" name)))

;;; Main function - does everything
(defun main ()
  "Fetch users, sort names, and display"
  (format t "Fetching users from API...~%")
  (let* ((users (fetch-users))
         (names (get-user-names users)))
    (print-names names)
    (format t "~&Total users: ~D~%" (length names))))
(main)

;;; Usage:
;;; (load "main.lisp")
;;; (simple-http:main)
;;;
;;; Or run from command line:
;;; sbcl --load main.lisp --eval "(simple-http:main)" --quit
