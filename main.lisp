
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

(fetch-users)

;;; Extract and sort user names
(defun get-user-emails (users)
  "Extract names from user list and sort them"
  (sort (mapcar (lambda (user)
                  (cdr (assoc :email user)))
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
         (names (get-user-emails users)))
    (print-names names)
    (format t "~&Total users: ~D~%" (length names))))
(main)

;;; Usage:
;;; (load "main.lisp")
;;; (simple-http:main)
;;;
;;; Or run from command line:
;;; sbcl --load main.lisp --eval "(simple-http:main)" --quit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; $ curl -O https://beta.quicklisp.org/quicklisp.lisp
;;   % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
;;                                  Dload  Upload   Total   Spent    Left  Speed
;; 100 49843  100 49843    0     0  33639      0  0:00:01  0:00:01 --:--:-- 50397
;; 
;; $ curl -O https://beta.quicklisp.org/quicklisp.lisp.asc
;; ...
;; $ gpg --verify quicklisp.lisp.asc quicklisp.lisp
;; gpg: Signature made Sat Feb  1 09:25:28 2014 EST using RSA key ID 028B5FF7
;; gpg: Good signature from "Quicklisp Release Signing Key "
;; $ sbcl --load quicklisp.lisp
;; This is SBCL 1.0.42.52, an implementation of ANSI Common Lisp.
;; More information about SBCL is available at <http://www.sbcl.org/>.
;; 
;; SBCL is free software, provided as is, with absolutely no warranty.
;; It is mostly in the public domain; some portions are provided under
;; BSD-style licenses.  See the CREDITS and COPYING files in the
;; distribution for more information.
;; 
;;   ==== quicklisp quickstart loaded ====
;; 
;;     To continue, evaluate: (quicklisp-quickstart:install)
;; 
;; * (quicklisp-quickstart:install)
;; 
;; ; Fetching #<URL "http://beta.quicklisp.org/quickstart/asdf.lisp">
;; ; 144.48KB
;; ==================================================
;; 147,949 bytes in 0.64 seconds (226.11KB/sec)
;; ; Fetching #<URL "http://beta.quicklisp.org/quickstart/quicklisp.tar">
;; ; 160.00KB
;; ==================================================
;; 163,840 bytes in 0.76 seconds (211.36KB/sec)
;; ; Fetching #<URL "http://beta.quicklisp.org/quickstart/setup.lisp">
;; ; 2.78KB
;; ==================================================
;; 2,846 bytes in 0.001 seconds (2779.30KB/sec)
;; Upgrading ASDF package from version 2.004 to version 2.009
;; ; Fetching #<URL "http://beta.quicklisp.org/dist/quicklisp.txt">
;; ; 0.40KB
;; ==================================================
;; 408 bytes in 0.003 seconds (132.81KB/sec)
;; 
;;   ==== quicklisp installed ====
;; 
;;     To load a system, use: (ql:quickload "system-name")
;; 
;;     To find systems, use: (ql:system-apropos "term")
;; 
;;     To load Quicklisp every time you start Lisp, use: (ql:add-to-init-file)
;; 
;;     For more information, see http://www.quicklisp.org/beta/
;; 
;; NIL
; * (ql:system-apropos "vecto")
;; 
;; ; Fetching #<URL "http://beta.quicklisp.org/dist/quicklisp/2010-10-07/systems.txt">
;; ; 45.30KB
;; ==================================================
;; 46,386 bytes in 0.50 seconds (89.70KB/sec)
;; ; Fetching #<URL "http://beta.quicklisp.org/dist/quicklisp/2010-10-07/releases.txt">
;; ; 83.49KB
;; ==================================================
;; 85,490 bytes in 0.53 seconds (157.22KB/sec)
;; #<SYSTEM cl-vectors / cl-vectors-20101006-git / quicklisp 2010-10-07>
;; #<SYSTEM lispbuilder-sdl-cl-vectors / lispbuilder-20101006-svn / quicklisp 2010-10-07>
;; #<SYSTEM lispbuilder-sdl-cl-vectors-examples / lispbuilder-20101006-svn / quicklisp 2010-10-07>
;; #<SYSTEM lispbuilder-sdl-vecto / lispbuilder-20101006-svn / quicklisp 2010-10-07>
;; #<SYSTEM lispbuilder-sdl-vecto-examples / lispbuilder-20101006-svn / quicklisp 2010-10-07>
;; #<SYSTEM static-vectors / static-vectors-20101006-git / quicklisp 2010-10-07>
;; #<SYSTEM vecto / vecto-1.4.3 / quicklisp 2010-10-07>
;; NIL
;; * (ql:quickload "vecto")
;; To load "vecto":
;;   Install 5 Quicklisp releases:
;;     cl-vectors salza2 vecto zpb-ttf zpng
;; ; Fetching #<URL "http://beta.quicklisp.org/archive/salza2/2010-10-06/salza2-2.0.7.tgz">
;; ; 14.84KB
;; ==================================================
;; 15,197 bytes in 0.12 seconds (125.77KB/sec)
;; ; Fetching #<URL "http://beta.quicklisp.org/archive/zpng/2010-10-06/zpng-1.2.tgz">
;; ; 38.59KB
;; ==================================================
;; 39,521 bytes in 0.37 seconds (103.47KB/sec)
;; ; Fetching #<URL "http://beta.quicklisp.org/archive/zpb-ttf/2010-10-06/zpb-ttf-1.0.tgz">
;; ; 42.59KB
;; ==================================================
;; 43,611 bytes in 0.39 seconds (110.33KB/sec)
;; ; Fetching #<URL "http://beta.quicklisp.org/archive/cl-vectors/2010-10-06/cl-vectors-20101006-git.tgz">
;; ; 40.40KB
;; ==================================================
;; 41,374 bytes in 0.37 seconds (109.20KB/sec)
;; ; Fetching #<URL "http://beta.quicklisp.org/archive/vecto/2010-10-06/vecto-1.4.3.tgz">
;; ; 75.71KB
;; ==================================================
;; 77,526 bytes in 0.49 seconds (153.57KB/sec)
;; ; Loading "vecto"
;; ..................................................
;; [package zpb-ttf].................................
;; [package salza2]..................................
;; [package zpng]....................................
;; [package net.tuxee.paths].........................
;; [package net.tuxee.aa]............................
;; [package net.tuxee.aa-bin]........................
;; [package net.tuxee.vectors].......................
;; [package vecto]........
;; ("vecto")
;; * (ql:add-to-init-file)
;; I will append the following lines to #P"/Users/quicklisp/.sbclrc":
;; 
;;   ;;; The following lines added by ql:add-to-init-file:
;;   #-quicklisp
;;   (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
;;                                          (user-homedir-pathname))))
;;     (when (probe-file quicklisp-init)
;;       (load quicklisp-init)))
;; 
;; Press Enter to continue.
;; 
;; 
;; #P"/Users/quicklisp/.sbclrc"
;; * (quit)
;; $ 


;;;; Single function to hit GoDaddy API

(ql:quickload :dexador)

(defun godaddy-check-domain (domain api-key api-secret)
  "Check if domain is available via GoDaddy API"
  (dexador:get (format nil "https://api.ote-godaddy.com/v1/domains/available?domain=~A" domain)
               :headers `(("Authorization" . ,(format nil "sso-key ~A:~A" api-key api-secret)))))


;;; Usage:
;;; (godaddy-check-domain "example.guru" "YOUR_API_KEY" "YOUR_API_SECRET")

;; (godaddy-check-domain "example77.xyz" "scrubbed-works!!!" "scrubbed-works!!!")
;;;; Simple SQLite example with Mito and CL-DBI - KISS style

(ql:quickload '(:mito :cl-dbi))

;;; Define user table using mito:deftable (preferred way)
(mito:deftable user ()
  ((name :col-type (:varchar 64))
   (email :col-type (:varchar 128))))

;;; Demo Mito operations (high-level ORM)
(defun mito-demo ()
  "Demo Mito ORM operations"
  ;; Connect using Mito
  (mito:connect-toplevel :sqlite3 :database-name "test.db")
  
  ;; Create table
  (mito:ensure-table-exists 'user)
  
  ;; Add users
  (mito:create-dao 'user :name "Alice" :email "alice@example.com")
  (mito:create-dao 'user :name "Bob" :email "bob@example.com")
  
  ;; Query users
  (format t "Mito - All users:~%")
  (dolist (user (mito:select-dao 'user))
    (format t "  ID:~A ~A <~A>~%" 
            (mito:object-id user) (user-name user) (user-email user))))

;;; Demo CL-DBI operations (low-level SQL)
(defun dbi-demo ()
  "Demo CL-DBI raw SQL operations"
  ;; Connect using DBI directly
  (defvar *conn* (dbi:connect :sqlite3 :database-name "test.db"))
  
  ;; Raw SQL query
  (let ((query (dbi:prepare *conn* "SELECT * FROM user")))
    (format t "~%DBI - Raw SQL results:~%")
    (loop for row = (dbi:fetch (dbi:execute query))
          while row
          do (format t "  ~A~%" row))))

;;; Combined demo
(defun demo ()
  "Demo both Mito and CL-DBI"
  (mito-demo)
  (dbi-demo))

;;; Usage:
(demo)

