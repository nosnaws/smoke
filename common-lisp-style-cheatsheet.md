# Common Lisp Style Cheatsheet

Condensed rules for idiomatic Common Lisp. See [full guide](common-lisp-style-guide.md) for rationale and examples.

---

## Naming ([more →](common-lisp-style-guide.md#naming-conventions))

| Type | Convention | Example |
|------|------------|---------|
| Variables | `lowercase-with-dashes` | `user-count`, `max-retries` |
| Special/Dynamic | `*earmuffs*` | `*database*`, `*debug-mode*` |
| Constants | `+plus-signs+` | `+max-size+`, `+pi+` |
| Predicates (1 word) | `-p` suffix | `evenp`, `listp` |
| Predicates (multi) | `-p` suffix | `valid-user-p`, `file-exists-p` |
| Accessors | `class-slot` | `request-url`, `user-name` |

**Don't**: Abbreviate (`mk-txt-node`), prefix with package name (`parser-parse`).

---

## Formatting ([more →](common-lisp-style-guide.md#formatting))

- **Indent**: 2 spaces, no tabs
- **Line length**: 100 columns max
- **Parens**: Never on their own line—close at end of expression
- **Blank lines**: One between top-level forms

```lisp
(defun example (x)
  (let ((y (1+ x)))       ; Bindings aligned
    (when (plusp y)       ; 2-space indent
      (* y y))))          ; Parens close together
```

**`if`**: Both branches same column. **`let`**: Bindings aligned. **`cond`**: Test+short-body on one line.

---

## Comments ([more →](common-lisp-style-guide.md#documentation-and-comments))

| Semicolons | Use |
|------------|-----|
| `;;;;` | File header (top of file) |
| `;;;` | Section separators |
| `;;` | Code explanation (above code) |
| `;` | End-of-line note |

Always add docstrings to public functions, classes, and slots.

---

## Flow Control ([more →](common-lisp-style-guide.md#flow-control))

| Situation | Use |
|-----------|-----|
| One true branch | `when` |
| One false branch | `unless` |
| Two branches | `if` |
| Multiple branches | `cond` |
| Dispatch on constants | `case` / `ecase` |
| Complex destructuring | `trivia:match` |

```lisp
(when condition body...)           ; Not (if condition (progn body...) nil)
(unless condition body...)         ; Not (if (not condition) body...)
(if test then else)                ; Both branches, no progn needed
(cond (test1 result1) (t default)) ; Multiple conditions
```

---

## Functions & Macros ([more →](common-lisp-style-guide.md#functions-and-macros))

- Prefer functions over macros
- Use `&key` over `&optional` when >1 optional arg
- Use multiple return values over out-params
- Keep functions small and focused

```lisp
(defun fetch (url &key (timeout 30) headers)  ; &key with defaults
  "Fetch URL. Returns (values body status)."  ; Docstring + multiple values
  (values body status))

(multiple-value-bind (body status) (fetch url)
  (process body status))
```

**Macros**: Use `alexandria:with-gensyms`, evaluate args once, name as `with-*`/`do-*`/`define-*`.

---

## CLOS ([more →](common-lisp-style-guide.md#clos-common-lisp-object-system))

**Slot option order**: `:reader`/`:accessor` → `:initarg` → `:initform` → `:type` → `:documentation`

```lisp
(defclass request ()
  ((url :reader request-url
        :initarg :url
        :type string
        :documentation "Request URL."))
  (:documentation "HTTP request."))
```

Always use `:type`. Use `trivial-types` for complex types.

---

## Conditions & Restarts ([more →](common-lisp-style-guide.md#conditions-and-restarts))

```lisp
;; Define condition
(define-condition my-error (error)
  ((message :initarg :message :reader my-error-message)))

;; Signal with restarts
(restart-case (error 'my-error :message "Failed")
  (use-default () default-value)
  (retry () (try-again)))

;; Handle (simple try/catch style)
(handler-case (risky-operation)
  (my-error (e) (handle-it e)))

;; Handle (with restart invocation)
(handler-bind ((my-error (lambda (e) (invoke-restart 'use-default))))
  (risky-operation))
```

---

## Packages ([more →](common-lisp-style-guide.md#packages))

```lisp
(defpackage :myapp
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))  ; Modern approach
  (:import-from :alexandria :with-gensyms :curry)     ; Explicit imports
  (:export :main :start :stop))
```

**Don't**: `(:use :cl :alexandria :everything-else)` — pollutes namespace.

---

## Project Structure ([more →](common-lisp-style-guide.md#project-structure))

```
my-project/
├── src/
│   ├── package.lisp
│   └── core.lisp
├── t/
│   └── core-test.lisp
├── README.md
├── my-project.asd
└── my-project-test.asd
```

---

## ASDF System ([more →](common-lisp-style-guide.md#asdf-system-definitions))

```lisp
(defsystem "my-project"
  :author "Name <email>"  :license "MIT"  :version "0.1.0"
  :depends-on ("alexandria" "serapeum")
  :serial t
  :components ((:module "src" :components ((:file "package") (:file "core"))))
  :in-order-to ((test-op (test-op "my-project-test"))))
```

---

## Standard Libraries ([more →](common-lisp-style-guide.md#standard-libraries))

### Alexandria — Core utilities
```lisp
(a:hash-table-keys ht)              (a:flatten list)
(a:curry #'fn arg)                  (a:compose #'f #'g)
(a:if-let (x (find-it)) use-x)      (a:when-let ((x (find-x))) use-x)
(a:with-gensyms (a b) body)         (a:once-only (arg) body)
(a:emptyp seq)                      (a:last-elt seq)
(a:make-keyword "foo")              (a:symbolicate 'a '- 'b)
```

### Serapeum — Extended utilities (complements Alexandria)
```lisp
(s:dict :a 1 :b 2)                  (s:@ ht :key)
(s:~> x (f1) (f2 arg))              (s:~>> x (f1) (f2))
(s:filter #'pred list)              (s:partition #'pred list)
(s:string+ "a" "b")                 (s:lines str)  (s:words str)
```

### Trivia — Pattern matching
```lisp
(match value
  ((list a b c) (use a b c))        ; Destructure list
  ((cons h t) (use h t))            ; Head/tail
  ((guard x (plusp x)) :positive)   ; With guard
  (_ :default))                     ; Wildcard
```

### str — String manipulation
```lisp
(str:concat "a" "b")                (str:join "," list)
(str:split "," string)              (str:trim string)
(str:starts-with-p str prefix)      (str:containsp str sub)
(str:empty? "")                     (str:blankp "  ")
```

### Bordeaux-Threads — Threading
```lisp
(bt:make-thread #'fn :name "worker")    (bt:join-thread thread)
(bt:make-lock)                          (bt:with-lock-held (lock) body)
```

### UIOP — Portability (included with ASDF)
```lisp
(uiop:getenv "VAR")                     (uiop:run-program "cmd" :output t)
(uiop:read-file-string path)            (uiop:write-string-into-file str path)
(uiop:file-exists-p path)               (uiop:quit 0)
```

---

## Loop Patterns

```lisp
(loop for x in list collect (f x))            ; Map
(loop for x in list when (p x) collect x)     ; Filter
(loop for x in list sum x)                    ; Reduce
(loop for k being the hash-keys of ht using (hash-value v) ...)  ; Hash
(loop for x in list for i from 0 ...)         ; With index
(loop for x in list when (target-p x) return x)  ; Find
```

---

## Testing with FiveAM

```lisp
(def-suite my-tests)
(in-suite my-tests)

(test name
  (is (= expected actual))            ; Equality
  (is-true expr)                      ; Truthy
  (signals error-type expr))          ; Expect error

(run! 'my-tests)
```

---

## Common Mistakes

| Don't | Do |
|-------|-----|
| `(if x (progn ...))` | `(when x ...)` |
| `(if (not x) ...)` | `(unless x ...)` |
| `(:use :alexandria)` | `(:import-from :alexandria ...)` |
| `(setf x (+ x 1))` | `(incf x)` |
| `(nth 0 list)` | `(first list)` |
| `(car (last list))` | `(alexandria:last-elt list)` |
| Close parens on own lines | Close at end of expression |
| Abbreviations in names | Full words with dashes |
| Comment out dead code | Delete it (use VCS) |

---

## Quick Reference Links

- [HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/) — Language reference
- [Cookbook](https://lispcookbook.github.io/cl-cookbook/) — Practical recipes
- [Awesome-CL](https://github.com/CodyReichert/awesome-cl) — Library list
- [Quickdocs](http://quickdocs.org/) — Library docs
