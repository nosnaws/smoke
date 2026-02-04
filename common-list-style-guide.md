# Modern Common Lisp Style Guide

A comprehensive guide to writing idiomatic, maintainable Common Lisp code. Based on the [Google Common Lisp Style Guide](https://google.github.io/styleguide/lispguide.xml), [lisp-lang.org Style Guide](https://lisp-lang.org/style-guide/), and the [Common Lisp Cookbook](https://lispcookbook.github.io/cl-cookbook/).

---

## Table of Contents

1. [Philosophy and Principles](#philosophy-and-principles)
2. [Naming Conventions](#naming-conventions)
3. [Formatting](#formatting)
4. [Documentation and Comments](#documentation-and-comments)
5. [Flow Control](#flow-control)
6. [Functions and Macros](#functions-and-macros)
7. [CLOS (Common Lisp Object System)](#clos-common-lisp-object-system)
8. [Conditions and Restarts](#conditions-and-restarts)
9. [Packages](#packages)
10. [Project Structure](#project-structure)
11. [ASDF System Definitions](#asdf-system-definitions)
12. [Standard Libraries](#standard-libraries)
13. [Common Patterns](#common-patterns)
14. [Testing](#testing)
15. [Performance Considerations](#performance-considerations)
16. [Resources](#resources)

---

## Philosophy and Principles

### Core Principles

1. **Readability**: Code must be easy for another developer to read, understand, and modify.
2. **Consistency**: Code should look the same across a project. There should be no way to recognize code by its author's style.
3. **Precision**: Be precise in what you write.
4. **Conciseness**: Be concise, but not at the expense of clarity.
5. **Simplicity**: Use the smallest hammer for the job. KISS — Keep It Simple, Stupid.
6. **Locality**: Keep related code together. Minimize the amount of jumping around needed to understand code.

### Priority Order

When making decisions about how to write code, aim for these qualities in this order:

1. **Usability** — Does it work correctly?
2. **Security** — Is it safe?
3. **Readability** — Can others understand it?
4. **Simplicity** — Is the design minimal?
5. **Efficiency** — Does it perform well?

### Use Libraries

Look for libraries that solve your problems before writing new code. Making a project with no dependencies is not a virtue. Check [Quicklisp](https://www.quicklisp.org/) and the [Awesome-CL](https://github.com/CodyReichert/awesome-cl) list.

### Write Libraries

Think of your project as a collection of independent libraries bound together by domain-specific functionality. If you lose interest in a project, you'll have left useful libraries for others.

---

## Naming Conventions

### General Style

- Names are **lowercase**, separated by single dashes (`-`)
- Use **complete words**, not abbreviations
- Good: `user-count`, `make-text-node`
- Bad: `user-cnt`, `mk-txt-node`

### Variables

**Special variables** (mutable dynamic variables) use "earmuffs" (asterisks):

```lisp
(defparameter *database-connection* nil)
(defparameter *default-timeout* 30)
```

**Constants** use plus signs:

```lisp
(defconstant +golden-ratio+ 1.6180339)
(defconstant +max-retries+ 3)
```

**Lexical variables** use plain names:

```lisp
(let ((user-name "Alice")
      (retry-count 0))
  ...)
```

### Predicates

Predicates return `t` or `nil`. Use the following suffixes:

- `p` — If the name is a single word: `evenp`, `listp`, `stringp`
- `-p` — If the name is multiple words: `circular-list-p`, `request-valid-p`

```lisp
(defun prime-p (n) ...)        ; Bad: multi-word, use -p
(defun primep (n) ...)         ; Good: single word concept
(defun user-admin-p (user) ...)  ; Good: multi-word
```

### Slot Accessors

For CLOS classes, prefix accessors with the class name:

```lisp
(defclass request ()
  ((url :reader request-url)
   (method :reader request-method)))
```

### Don't Prefix Package Names

This is what packages are for. A function in package `myapp.parser` should not start with `parser-`.

For C library bindings where functions are `library_name_function_name`, use the library name as the package name.

---

## Formatting

### Indentation

- Use **2 spaces** per indentation level
- Let your editor (Emacs with SLIME/SLY) handle indentation
- Never use tabs

```lisp
(defun example ()
  (let ((x 1)
        (y 2))
    (when (> x 0)
      (format t "X=~A, Y=~A~%" x y))
    (+ x y)))
```

### Special Form Indentation

**`if`** — Both branches on same column:

```lisp
(if (> x 5)
    (handle-large x)
    (handle-small x))
```

**`let`/`let*`** — Bindings aligned:

```lisp
(let ((name "Alice")
      (age 30)
      (occupation "Engineer"))
  (create-user name age occupation))
```

**`cond`** — Test and body on same line if short:

```lisp
(cond ((null list) 0)
      ((atom list) 1)
      (t (+ 1 (count-atoms (cdr list)))))
```

Or body on next line if longer:

```lisp
(cond ((eq type :error)
       (log-error message)
       (signal-condition message))
      ((eq type :warning)
       (log-warning message))
      (t
       (log-info message)))
```

### Line Length

Lines should not exceed **100 columns**. The traditional 80-column limit is less fitting for Common Lisp, which encourages descriptive names.

### Parentheses

- **Never** put closing parentheses on their own line
- Let parentheses "flow" at the end of expressions

```lisp
;; Good
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (1- n)))))

;; Bad - closing parens on own lines
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (1- n)))
  )
)
```

### Blank Lines

- One blank line between top-level forms
- No blank lines inside function bodies (unless separating logical sections)

---

## Documentation and Comments

### Docstrings

Add docstrings to all public functions, macros, classes, and packages:

```lisp
(defun parse-config (path)
  "Parse the configuration file at PATH and return a plist.
   
   PATH should be a pathname designator pointing to a valid config file.
   Returns NIL if the file cannot be parsed."
  ...)

(defclass user ()
  ((name :reader user-name
         :initarg :name
         :type string
         :documentation "The user's display name."))
  (:documentation "Represents a system user with authentication credentials."))
```

### Comment Hierarchy

| Semicolons | Usage | Placement |
|------------|-------|-----------|
| `;;;;` | File-level comments | Top of file, column 0 |
| `;;;` | Section separators | Column 0, before groups of functions |
| `;;` | Code explanations | Above code, aligned with code |
| `;` | End-of-line notes | After code on same line |

```lisp
;;;; user-management.lisp — User authentication and authorization

;;; User Creation

(defun create-user (name email)
  "Create a new user with NAME and EMAIL."
  ;; Validate inputs before creating
  (check-type name string)
  (check-type email string)
  (make-instance 'user
                 :name name
                 :email email
                 :created-at (get-universal-time)))  ; UTC timestamp
```

### File Header

Every file should start with a four-semicolon comment describing its purpose:

```lisp
;;;; database.lisp — Database connection and query utilities
;;;;
;;;; This module provides connection pooling and query builders
;;;; for PostgreSQL databases.
```

**Do not include** copyright or authorship in file headers. Put that in the README and system definition.

### Don't Comment Out Code

Use version control. Delete code instead of commenting it out.

---

## Flow Control

### Use `when` and `unless`

For single-branch conditionals:

```lisp
;; Bad
(if (engine-running-p car)
    (drive car)
    nil)

;; Good
(when (engine-running-p car)
  (drive car))

;; Bad
(if (not (valid-p input))
    (signal-error input)
    nil)

;; Good
(unless (valid-p input)
  (signal-error input))
```

### Use `if` for Two Branches

```lisp
(if (admin-p user)
    (show-admin-panel user)
    (show-user-dashboard user))
```

### Use `cond` for Multiple Branches

```lisp
(cond ((null items) (error "No items"))
      ((= (length items) 1) (process-single (first items)))
      ((< (length items) 10) (process-batch items))
      (t (process-large-batch items)))
```

### Use `case`/`ecase` for Dispatch on Constants

```lisp
(case command
  (:start (start-server))
  (:stop (stop-server))
  ((:pause :suspend) (pause-server))
  (otherwise (error "Unknown command: ~A" command)))

;; ecase signals error if no match
(ecase status
  (:pending (handle-pending))
  (:complete (handle-complete))
  (:failed (handle-failure)))
```

### Keep Conditions Short

Factor complex conditions into named functions:

```lisp
;; Bad
(when (and (authenticated-p user)
           (has-permission-p user :admin)
           (not (suspended-p user))
           (< (failed-login-count user) 3))
  (grant-access user))

;; Good
(defun can-grant-access-p (user)
  (and (authenticated-p user)
       (has-permission-p user :admin)
       (not (suspended-p user))
       (< (failed-login-count user) 3)))

(when (can-grant-access-p user)
  (grant-access user))
```

### Pattern Matching with Trivia

Use [Trivia](#trivia) for complex destructuring:

```lisp
(match response
  ((list :ok result) (process-result result))
  ((list :error code message) (handle-error code message))
  (_ (error "Unexpected response format")))
```

---

## Functions and Macros

### Prefer Functions Over Macros

Write functions when possible. Macros should be used when you need:

- New control flow
- To avoid evaluation of arguments
- To generate code at compile time

### Function Design

- Keep functions small and focused
- Prefer multiple return values over out-parameters
- Use `&key` arguments for optional parameters with defaults
- Use `&rest` sparingly

```lisp
(defun fetch-user (id &key (include-profile t) (timeout 30))
  "Fetch user by ID with optional profile data."
  ...)

(defun parse-response (response)
  "Parse RESPONSE, returning (values data status headers)."
  (values (extract-data response)
          (extract-status response)
          (extract-headers response)))
```

### Use `&key` Over `&optional` When Appropriate

```lisp
;; Confusing - which boolean is which?
(send-email to subject body nil t nil)

;; Clear
(send-email to subject body
            :html-p t
            :track-opens-p nil)
```

### Macro Hygiene

- Use `with-gensyms` (from Alexandria) to avoid variable capture
- Evaluate arguments only once
- Use clear macro naming: `with-*`, `do-*`, `define-*`

```lisp
(defmacro with-timing ((var) &body body)
  "Execute BODY and bind elapsed seconds to VAR."
  (with-gensyms (start)
    `(let ((,start (get-internal-real-time)))
       (prog1 (progn ,@body)
         (setf ,var (/ (- (get-internal-real-time) ,start)
                       internal-time-units-per-second))))))
```

### Avoid Excessive `setf`

Favor functional style:

```lisp
;; Imperative
(let ((result nil))
  (dolist (item items)
    (when (valid-p item)
      (push (process item) result)))
  (nreverse result))

;; Functional
(mapcar #'process (remove-if-not #'valid-p items))

;; Or with loop
(loop for item in items
      when (valid-p item)
      collect (process item))
```

---

## CLOS (Common Lisp Object System)

### Class Definition Order

```lisp
(defclass request ()
  ((url :reader request-url
        :initarg :url
        :type string
        :documentation "Request URL.")
   (method :reader request-method
           :initarg :method
           :initform :get
           :type keyword
           :documentation "HTTP method.")
   (headers :accessor request-headers
            :initarg :headers
            :initform nil
            :type list
            :documentation "HTTP headers as alist."))
  (:documentation "Represents an HTTP request."))
```

### Slot Option Order

1. `:accessor`, `:reader`, or `:writer`
2. `:initarg`
3. `:initform` (if any)
4. `:type` (if known)
5. `:documentation`

### Always Use `:type`

Types serve as documentation and enable some compile-time checks:

```lisp
(defclass point ()
  ((x :accessor point-x :initarg :x :type real)
   (y :accessor point-y :initarg :y :type real)))
```

Use [trivial-types](https://github.com/m2ym/trivial-types) for complex types:

```lisp
(:type (or null string))
(:type (integer 0 100))
(:type (proper-list keyword))
```

### Use `defgeneric` for Documentation

Define generic functions explicitly for documentation:

```lisp
(defgeneric serialize (object format)
  (:documentation "Serialize OBJECT to FORMAT (:json, :xml, etc.)"))

(defmethod serialize ((object user) (format (eql :json)))
  ...)
```

### Constructor Functions

Provide constructor functions for complex initialization:

```lisp
(defun make-request (url &key (method :get) headers body)
  "Create a new request to URL with optional METHOD, HEADERS, and BODY."
  (make-instance 'request
                 :url url
                 :method method
                 :headers headers
                 :body body))
```

---

## Conditions and Restarts

### Define Custom Conditions

```lisp
(define-condition validation-error (error)
  ((field :initarg :field :reader validation-error-field)
   (message :initarg :message :reader validation-error-message))
  (:report (lambda (condition stream)
             (format stream "Validation error on ~A: ~A"
                     (validation-error-field condition)
                     (validation-error-message condition)))))
```

### Condition Hierarchy

- Inherit from `error` for serious errors
- Inherit from `warning` for warnings
- Inherit from `condition` for notifications

```lisp
(define-condition parse-error (error) ...)
(define-condition deprecated-feature-warning (warning) ...)
(define-condition progress-notification (condition) ...)
```

### Provide Restarts

Allow callers to choose recovery strategies:

```lisp
(defun parse-config-file (path)
  "Parse configuration file, providing restarts for errors."
  (restart-case
      (with-open-file (stream path)
        (parse-config stream))
    (use-default-config ()
      :report "Use default configuration"
      *default-config*)
    (use-value (config)
      :report "Specify configuration to use"
      :interactive (lambda () (list (read)))
      config)))
```

### Handler Usage

Use `handler-case` for simple error handling (like try/catch):

```lisp
(handler-case (parse-file path)
  (file-error (e)
    (log-error "File error: ~A" e)
    nil)
  (parse-error (e)
    (log-error "Parse error: ~A" e)
    nil))
```

Use `handler-bind` when you need restarts or stack inspection:

```lisp
(handler-bind ((validation-error
                 (lambda (e)
                   (invoke-restart 'skip-invalid))))
  (process-all-records records))
```

### Standard Restarts

Common restart names to use:

- `abort` — Abort the operation entirely
- `continue` — Continue with default behavior
- `use-value` — Use a specified value instead
- `store-value` — Store a value and retry
- `retry` — Retry the operation

---

## Packages

### One Package Per File (Usually)

Unless a package naturally spans multiple files.

### Prefer `:import-from` Over `:use`

Be explicit about what you import:

```lisp
;; Bad - pollutes namespace, unclear origins
(defpackage :myapp
  (:use :cl :alexandria :bordeaux-threads))

;; Good - explicit imports
(defpackage :myapp
  (:use :cl)
  (:import-from :alexandria
                :with-gensyms
                :once-only
                :curry
                :compose)
  (:import-from :bordeaux-threads
                :make-thread
                :join-thread))
```

### Use Package-Local Nicknames

Modern CL implementations support local nicknames:

```lisp
(defpackage :myapp
  (:use :cl)
  (:local-nicknames
   (:a :alexandria)
   (:bt :bordeaux-threads)
   (:sera :serapeum)))

(in-package :myapp)

(a:curry #'+ 1)  ; Clearly from alexandria
```

### Hierarchical Package Names

For projects with multiple packages:

```
myapp
myapp.db
myapp.db.migrations
myapp.api
myapp.api.routes
myapp.api.middleware
```

### Export Explicitly

List all exports in the package definition:

```lisp
(defpackage :myapp.api
  (:use :cl)
  (:export
   ;; Classes
   :request
   :response
   ;; Constructors
   :make-request
   ;; Accessors
   :request-url
   :request-method
   ;; Functions
   :handle-request
   :send-response))
```

---

## Project Structure

### Standard Directory Layout

Small project:

```
my-project/
├── src/
│   └── my-project.lisp
├── t/
│   └── my-project-test.lisp
├── .gitignore
├── README.md
├── my-project.asd
└── my-project-test.asd
```

Larger project:

```
my-project/
├── src/
│   ├── package.lisp
│   ├── conditions.lisp
│   ├── utils.lisp
│   ├── core.lisp
│   └── api/
│       ├── package.lisp
│       ├── routes.lisp
│       └── handlers.lisp
├── t/
│   ├── package.lisp
│   ├── core-test.lisp
│   └── api-test.lisp
├── docs/
├── .gitignore
├── README.md
├── my-project.asd
└── my-project-test.asd
```

### README Format

Use Markdown with these sections:

1. **Title** — Project name
2. **Description** — One-line summary
3. **Overview** — Longer description, features, motivation
4. **Installation** — How to install
5. **Usage** — Code examples
6. **API Reference** — For smaller projects
7. **License** — License name and copyright

---

## ASDF System Definitions

### Main System Definition

```lisp
(defsystem "my-project"
  :author "Your Name <you@example.com>"
  :maintainer "Your Name <you@example.com>"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/you/my-project"
  :description "Short description of the project."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :depends-on ("alexandria"
               "serapeum"
               "trivia")
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "conditions")
                 (:file "utils")
                 (:file "core"))))
  :in-order-to ((test-op (test-op "my-project-test"))))
```

### Test System Definition

```lisp
(defsystem "my-project-test"
  :author "Your Name <you@example.com>"
  :license "MIT"
  :depends-on ("my-project"
               "fiveam")
  :serial t
  :components ((:module "t"
                :components
                ((:file "package")
                 (:file "core-test"))))
  :perform (test-op (o c)
             (symbol-call :fiveam '#:run!
                          (find-symbol* :my-project-tests
                                        :my-project-test))))
```

### Required Options

Always include:

- `:author` / `:maintainer`
- `:license`
- `:version`
- `:description`
- `:depends-on`

---

## Standard Libraries

These libraries are community standards. Learn and use them.

### Alexandria

**Purpose**: Core utilities that should have been in the standard.

**Install**: `(ql:quickload :alexandria)`

**Key Functions**:

```lisp
;; Hash tables
(alexandria:hash-table-keys ht)
(alexandria:hash-table-values ht)
(alexandria:alist-hash-table alist)

;; Lists
(alexandria:flatten nested-list)
(alexandria:ensure-list x)  ; Wraps non-lists
(alexandria:mappend fn list)  ; mapcar + append

;; Functions
(alexandria:curry #'+ 1)
(alexandria:compose #'1+ #'length)
(alexandria:rcurry #'- 1)

;; Control flow
(alexandria:if-let (x (find-thing))
  (use x)
  (handle-missing))

(alexandria:when-let ((x (find-x))
                      (y (find-y)))
  (use x y))

;; Macros
(alexandria:with-gensyms (a b c) ...)
(alexandria:once-only (arg) ...)

;; Sequences
(alexandria:last-elt sequence)
(alexandria:emptyp sequence)
(alexandria:shuffle list)

;; Strings/Symbols
(alexandria:make-keyword "foo")  ; => :FOO
(alexandria:symbolicate 'foo '- 'bar)  ; => FOO-BAR
```

### Serapeum

**Purpose**: Utilities beyond Alexandria. Use together.

**Install**: `(ql:quickload :serapeum)`

**Key Functions**:

```lisp
;; Safe package setup
(defpackage :my-app
  (:use :cl :alexandria :serapeum))

;; Hash tables
(serapeum:dict :a 1 :b 2)  ; Create hash-table
(serapeum:@ ht :key)  ; Access (gethash alternative)

;; Strings
(serapeum:string+ "Hello, " name "!")
(serapeum:lines string)
(serapeum:words string)
(serapeum:trim-whitespace string)

;; Sequences
(serapeum:filter #'evenp list)
(serapeum:partition #'evenp list)  ; => (evens odds)
(serapeum:batches list 3)  ; Split into groups of 3

;; Definitions
(serapeum:defalias new-name old-name)
(serapeum:defconst +name+ value "doc")

;; Control flow
(serapeum:~> x (fn1) (fn2 arg))  ; Threading macro
(serapeum:~>> x (fn1) (fn2))  ; Thread last

;; Types (exhaustive case)
(serapeum:ecase-of (member :a :b :c) value
  (:a (handle-a))
  (:b (handle-b))
  (:c (handle-c)))  ; Warns if not exhaustive
```

### Trivia

**Purpose**: Pattern matching (community standard, replaces Optima).

**Install**: `(ql:quickload :trivia)`

**Usage**:

```lisp
(use-package :trivia)

;; Basic matching
(match value
  (0 :zero)
  (1 :one)
  (_ :other))

;; Destructuring lists
(match '(1 2 3)
  ((list a b c) (+ a b c)))

;; Cons cells
(match list
  ((cons head tail) (process head tail))
  (nil :empty))

;; Vectors
(match #(1 2 3)
  ((vector a b c) (list a b c)))

;; Structs/Classes
(defstruct point x y)
(match (make-point :x 1 :y 2)
  ((point :x x :y y) (sqrt (+ (* x x) (* y y)))))

;; Guards
(match value
  ((guard x (plusp x)) :positive)
  ((guard x (minusp x)) :negative)
  (_ :zero))

;; Multiple values
(multiple-value-match (values 1 2)
  ((a b) (+ a b)))

;; Or patterns
(match x
  ((or :start :begin) :starting)
  ((or :stop :end) :stopping))

;; Property lists
(match '(:name "Alice" :age 30)
  ((plist :name name :age age)
   (format nil "~A is ~A years old" name age)))
```

### str (cl-str)

**Purpose**: Modern string manipulation.

**Install**: `(ql:quickload :str)`

```lisp
(str:concat "Hello, " "World")
(str:join ", " '("a" "b" "c"))
(str:split "," "a,b,c")
(str:trim "  hello  ")
(str:starts-with-p "Hello" "He")
(str:ends-with-p "Hello" "lo")
(str:containsp "Hello" "ell")
(str:replace-all "hello" "l" "L")
(str:empty? "")  ; => t
(str:blankp "   ")  ; => t
```

### Bordeaux-Threads

**Purpose**: Portable threading.

**Install**: `(ql:quickload :bordeaux-threads)`

```lisp
(bt:make-thread (lambda () (do-work)) :name "worker")
(bt:join-thread thread)
(bt:current-thread)
(bt:threadp object)
(bt:thread-alive-p thread)

;; Locks
(defvar *lock* (bt:make-lock "my-lock"))
(bt:with-lock-held (*lock*)
  (critical-section))

;; Condition variables
(defvar *cv* (bt:make-condition-variable))
(bt:condition-wait *cv* *lock*)
(bt:condition-notify *cv*)
```

### Local-Time

**Purpose**: Date/time handling.

**Install**: `(ql:quickload :local-time)`

```lisp
(local-time:now)
(local-time:today)
(local-time:timestamp+ (local-time:now) 1 :day)
(local-time:format-timestring nil timestamp
  :format local-time:+iso-8601-format+)
(local-time:parse-timestring "2024-01-15T10:30:00Z")
```

### Log4CL

**Purpose**: Logging framework.

**Install**: `(ql:quickload :log4cl)`

```lisp
(log:config :info)  ; Set level
(log:debug "Debug message: ~A" value)
(log:info "Info message")
(log:warn "Warning: ~A" condition)
(log:error "Error occurred: ~A" error)
```

### UIOP

**Purpose**: OS and implementation portability. Included with ASDF.

```lisp
(uiop:getenv "HOME")
(uiop:getcwd)
(uiop:run-program "ls" :output t)
(uiop:read-file-string #p"file.txt")
(uiop:write-string-into-file "content" #p"file.txt")
(uiop:file-exists-p path)
(uiop:directory-files directory)
(uiop:quit 0)  ; Exit
```

---

## Common Patterns

### The `with-` Pattern

For resource management (like Python's context managers):

```lisp
(defmacro with-database ((db-var connection-string) &body body)
  `(let ((,db-var (connect ,connection-string)))
     (unwind-protect
         (progn ,@body)
       (disconnect ,db-var))))
```

### Multiple Return Values

Use values for related returns:

```lisp
(defun divide (numerator denominator)
  "Returns (values quotient remainder)."
  (values (floor numerator denominator)
          (mod numerator denominator)))

(multiple-value-bind (quotient remainder) (divide 10 3)
  (format t "~A remainder ~A" quotient remainder))
```

### Keyword Arguments with Defaults

```lisp
(defun make-server (&key (host "localhost")
                         (port 8080)
                         (workers 4)
                         debug)
  ...)
```

### Property Lists for Configuration

```lisp
(defun configure-app (&rest config &key debug port &allow-other-keys)
  (when debug (enable-debug-mode))
  (start-server :port port :config config))
```

### The Loop Macro

Learn it well:

```lisp
;; Collection
(loop for x in list collect (1+ x))

;; Filtering
(loop for x in list when (evenp x) collect x)

;; Accumulation
(loop for x in list sum x)
(loop for x in list maximize x)

;; Hash tables
(loop for key being the hash-keys of ht
      using (hash-value val)
      collect (cons key val))

;; Multiple lists
(loop for x in list1
      for y in list2
      collect (+ x y))

;; Indices
(loop for item in list
      for i from 0
      do (format t "~A: ~A~%" i item))

;; Early exit
(loop for x in list
      when (target-p x) return x)
```

### Do Notation for Iteration

```lisp
;; dolist - iterate over list
(dolist (item items)
  (process item))

;; dotimes - iterate n times
(dotimes (i 10)
  (print i))

;; do - general iteration
(do ((i 0 (1+ i))
     (sum 0 (+ sum i)))
    ((>= i 10) sum))
```

---

## Testing

### Use FiveAM

**Install**: `(ql:quickload :fiveam)`

```lisp
(defpackage :my-project-test
  (:use :cl :fiveam :my-project)
  (:export :run-tests))

(in-package :my-project-test)

;; Define a test suite
(def-suite my-project-tests
  :description "Tests for my-project")

(in-suite my-project-tests)

;; Define tests
(test addition-works
  "Test that addition works correctly."
  (is (= 4 (add 2 2)))
  (is (= 0 (add -1 1))))

(test handles-errors
  "Test error handling."
  (signals validation-error
    (validate nil)))

;; Run tests
(defun run-tests ()
  (run! 'my-project-tests))
```

### FiveAM Assertions

```lisp
(is (= expected actual))           ; Equality
(is (equal expected actual))       ; EQUAL comparison
(is (equalp expected actual))      ; EQUALP comparison
(is-true expression)               ; Expression is truthy
(is-false expression)              ; Expression is NIL
(signals error-type expression)    ; Signals expected error
(finishes expression)              ; Completes without error
```

### Test Organization

- Mirror source structure in tests
- One test file per source file
- Use descriptive test names
- Test edge cases

```lisp
(test parse-empty-string
  (is (null (parse ""))))

(test parse-whitespace-only
  (is (null (parse "   "))))

(test parse-valid-input
  (is (equal '(:a 1) (parse "a=1"))))
```

---

## Performance Considerations

### Type Declarations

Use `declare` for performance-critical code:

```lisp
(defun fast-sum (list)
  (declare (optimize (speed 3) (safety 1)))
  (loop for x fixnum in list
        sum x fixnum))
```

### Avoid Consing in Hot Paths

Reuse data structures when performance matters:

```lisp
;; Conses on every call
(defun bad-append (list item)
  (append list (list item)))

;; Destructive, doesn't cons
(defun good-append (list item)
  (nconc list (list item)))
```

### Use Appropriate Data Structures

- `list` — Small, frequent modification
- `vector` — Random access, known size
- `hash-table` — Key-value lookup
- `array` — Multi-dimensional data

### Profile Before Optimizing

Use SBCL's profiler:

```lisp
(require :sb-sprof)
(sb-sprof:with-profiling (:report :flat)
  (my-slow-function))
```

---

## Resources

### Essential References

- [Common Lisp HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/) — The ANSI CL standard
- [Common Lisp Cookbook](https://lispcookbook.github.io/cl-cookbook/) — Practical recipes
- [Practical Common Lisp](https://gigamonkeys.com/book/) — Free online book

### Libraries

- [Quicklisp](https://www.quicklisp.org/) — Package manager
- [Awesome-CL](https://github.com/CodyReichert/awesome-cl) — Curated library list
- [Quickdocs](http://quickdocs.org/) — Library documentation

### Development Tools

- [SBCL](http://sbcl.org/) — Recommended implementation
- [Emacs + SLIME](https://common-lisp.net/project/slime/) — Classic IDE
- [Emacs + SLY](https://github.com/joaotavora/sly) — Modern SLIME fork
- [Lem](https://github.com/lem-project/lem) — Editor written in CL

### Community

- [/r/lisp](https://reddit.com/r/lisp) — Reddit
- [Discord](https://discord.gg/hhk46CE) — Common Lisp Discord
- [Libera Chat #commonlisp](https://web.libera.chat/#commonlisp) — IRC

---

## Changelog

- **v1.0.0** — Initial release

---

*This guide is licensed under CC-BY-4.0. Contributions welcome.*
