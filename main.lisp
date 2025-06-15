;;;; User API Example - KISS Style
;;;; Complete user management with HTTP client, database, and JSON

;;; Dependencies
;;;; #-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload '(:dexador :cl-json :mito))

(defpackage :user-api
  (:use :cl)
  (:export #:user #:demo-all #:fetch-external-users #:create-local-user
           #:find-user #:list-users #:update-user #:delete-user))

(in-package :user-api)

;;; Database Setup
(mito:connect-toplevel :sqlite3 :database-name "users.db")

;;; User Model
(mito:deftable user ()
  ((name :col-type (:varchar 100) :initarg :name :accessor user-name)
   (email :col-type (:varchar 150) :initarg :email :accessor user-email)
   (phone :col-type (:varchar 20) :initarg :phone :accessor user-phone)
   (company :col-type (:varchar 100) :initarg :company :accessor user-company))
  (:documentation "User table with basic contact info"))

(mito:ensure-table-exists 'user)

(comment
  ;; HTTP Client Operations - Fetching external data
  
  ;; Get all users from JSONPlaceholder
  (defun fetch-external-users ()
    "Fetch users from external API and return parsed JSON"
    (let ((response (dexador:get "https://jsonplaceholder.typicode.com/users")))
      (cl-json:decode-json-from-string response)))
  
  ;; Get single user by ID
  (defun fetch-external-user (id)
    "Fetch single user by ID from external API"
    (let ((response (dexador:get (format nil "https://jsonplaceholder.typicode.com/users/~A" id))))
      (cl-json:decode-json-from-string response)))
  
  ;; Post new user to external API (for testing)
  (defun post-external-user (user-data)
    "Post user data to external API"
    (dexador:post "https://jsonplaceholder.typicode.com/users"
                  :headers '(("Content-Type" . "application/json"))
                  :content (cl-json:encode-json-to-string user-data)))
  
  ;; Update user via external API
  (defun update-external-user (id user-data)
    "Update user via external API"
    (dexador:put (format nil "https://jsonplaceholder.typicode.com/users/~A" id)
                 :headers '(("Content-Type" . "application/json"))
                 :content (cl-json:encode-json-to-string user-data)))
  
  ;; Delete user via external API
  (defun delete-external-user (id)
    "Delete user via external API"
    (dexador:delete (format nil "https://jsonplaceholder.typicode.com/users/~A" id)))

  ;; Database Operations - Local user management
  
  ;; Create new user
  (defun create-local-user (name email &optional phone company)
    "Create new user in local database"
    (mito:create-dao 'user :name name :email email 
                     :phone (or phone "") :company (or company "")))
  
  ;; Find user by ID
  (defun find-user (id)
    "Find user by ID"
    (mito:find-dao 'user :id id))
  
  ;; Find user by email
  (defun find-user-by-email (email)
    "Find user by email address"
    (first (mito:select-dao 'user (mito:where (:= :email email)))))
  
  ;; List all users
  (defun list-users ()
    "Get all users from database"
    (mito:select-dao 'user))
  
  ;; List users with limit and offset
  (defun list-users-paginated (limit &optional (offset 0))
    "Get users with pagination"
    (mito:select-dao 'user (mito:limit limit) (mito:offset offset)))
  
  ;; Search users by name pattern
  (defun search-users (name-pattern)
    "Search users by name pattern"
    (mito:select-dao 'user (mito:where (:like :name (format nil "%~A%" name-pattern)))))
  
  ;; Update user
  (defun update-user (user &key name email phone company)
    "Update existing user with new data"
    (when name (setf (user-name user) name))
    (when email (setf (user-email user) email))
    (when phone (setf (user-phone user) phone))
    (when company (setf (user-company user) company))
    (mito:save-dao user))
  
  ;; Delete user
  (defun delete-user (user)
    "Delete user from database"
    (mito:delete-dao user))
  
  ;; Count users
  (defun count-users ()
    "Count total users in database"
    (mito:count-dao 'user))

  ;; Data Import/Export - Moving between external API and local DB
  
  ;; Import users from external API to local database
  (defun import-external-users ()
    "Import all users from external API to local database"
    (let ((external-users (fetch-external-users)))
      (dolist (user-data external-users)
        (let ((name (cdr (assoc :name user-data)))
              (email (cdr (assoc :email user-data)))
              (phone (cdr (assoc :phone user-data)))
              (company (cdr (assoc :company (cdr (assoc :company user-data))))))
          (unless (find-user-by-email email)
            (create-local-user name email phone company)))))
    (format t "Import completed. Total users: ~A~%" (count-users)))
  
  ;; Export local user to external format
  (defun export-user-to-json (user)
    "Convert local user to JSON format"
    (cl-json:encode-json-to-string
     `((:name . ,(user-name user))
       (:email . ,(user-email user))
       (:phone . ,(user-phone user))
       (:company . ,(user-company user)))))
  
  ;; Sync user with external API
  (defun sync-user-to-external (user)
    "Post local user to external API"
    (let ((user-json `((:name . ,(user-name user))
                      (:email . ,(user-email user))
                      (:phone . ,(user-phone user))
                      (:company . ,(user-company user)))))
      (post-external-user user-json)))

  ;; Utility Functions - Pretty printing and debugging
  
  ;; Pretty print user
  (defun print-user (user)
    "Pretty print user information"
    (format t "User #~A: ~A <~A>~%" 
            (mito:object-id user) (user-name user) (user-email user))
    (when (and (user-phone user) (not (string= (user-phone user) "")))
      (format t "  Phone: ~A~%" (user-phone user)))
    (when (and (user-company user) (not (string= (user-company user) "")))
      (format t "  Company: ~A~%" (user-company user))))
  
  ;; Pretty print all users
  (defun print-all-users ()
    "Print all users in database"
    (let ((users (list-users)))
      (format t "~%=== Local Users (Total: ~A) ===~%" (length users))
      (dolist (user users)
        (print-user user))))
  
  ;; Print external users
  (defun print-external-users ()
    "Fetch and print external users"
    (let ((users (fetch-external-users)))
      (format t "~%=== External Users (Total: ~A) ===~%" (length users))
      (dolist (user users)
        (format t "~A <~A> - ~A~%" 
                (cdr (assoc :name user))
                (cdr (assoc :email user))
                (cdr (assoc :phone user))))))

  ;; Demo Functions - Show all features working together
  
  ;; Complete demo
  (defun demo-all ()
    "Demonstrate all user operations"
    (format t "=== User API Demo ===~%")
    
    ;; 1. Create some local users
    (format t "~%1. Creating local users...~%")
    (create-local-user "Alice Smith" "alice@local.com" "555-0101" "Tech Corp")
    (create-local-user "Bob Jones" "bob@local.com" "555-0102" "Design Inc")
    
    ;; 2. List local users
    (print-all-users)
    
    ;; 3. Fetch external users
    (format t "~%2. Fetching external users...~%")
    (print-external-users)
    
    ;; 4. Import external users
    (format t "~%3. Importing external users...~%")
    (import-external-users)
    (print-all-users)
    
    ;; 5. Search and update
    (format t "~%4. Searching and updating...~%")
    (let ((user (find-user-by-email "alice@local.com")))
      (when user
        (format t "Found user: ~A~%" (user-name user))
        (update-user user :company "Updated Corp")
        (format t "Updated company to: ~A~%" (user-company user))))
    
    ;; 6. Export user to JSON
    (format t "~%5. Exporting user to JSON...~%")
    (let ((user (first (list-users))))
      (when user
        (format t "JSON: ~A~%" (export-user-to-json user))))
    
    (format t "~%Demo completed!~%"))
  
  ;; Quick test functions
  (defun quick-test ()
    "Quick test of basic operations"
    (create-local-user "Test User" "test@example.com")
    (print-all-users)
    (let ((user (find-user-by-email "test@example.com")))
      (when user (delete-user user)))
    (format t "Quick test completed~%"))

  ;; Usage examples:
  ;; (user-api:demo-all)                    ; Full demo
  ;; (user-api:create-local-user "John" "john@test.com")
  ;; (user-api:list-users)                  ; List all users
  ;; (user-api:fetch-external-users)        ; Get external users
  ;; (user-api:import-external-users)       ; Import from API
  ;; (user-api:print-all-users)             ; Pretty print users
)
