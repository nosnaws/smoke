(in-package :smoke-test)

(def-suite smoke-tests
  :description "Tests for smoke internals.")

(in-suite smoke-tests)

;;; init-state

(test init-state-structure
  (let ((state (smoke::init-state "feature-branch")))
    (is (string= "feature-branch" (cdr (assoc :branch state))))
    (is (null (cdr (assoc :stack state))))))

;;; remove-merged-prs-from-state

(defun make-state-with-entries (branch entries)
  "Helper: ENTRIES is list of (index pr smoke-branch)."
  (list (cons :branch branch)
        (cons :stack
              (mapcar (lambda (e)
                        (list (cons :index (first e))
                              (cons :pr (second e))
                              (cons :smoke--branch (third e))))
                      entries))))

(test remove-merged-prs-from-state-removes-correct
  (let* ((state (make-state-with-entries "feat"
                  '((0 1 "smoke/feat/01")
                    (1 2 "smoke/feat/02")
                    (2 3 "smoke/feat/03"))))
         (cleaned (smoke::remove-merged-prs-from-state state '(1 3))))
    (is (= 1 (length (cdr (assoc :stack cleaned)))))
    (let ((entry (first (cdr (assoc :stack cleaned)))))
      (is (= 2 (cdr (assoc :pr entry)))))))

(test remove-merged-prs-from-state-preserves-branch
  (let* ((state (make-state-with-entries "my-branch"
                  '((0 1 "smoke/my-branch/01"))))
         (cleaned (smoke::remove-merged-prs-from-state state '(1))))
    (is (string= "my-branch" (cdr (assoc :branch cleaned))))))

;;; smoke-branch-name

(test smoke-branch-name-format
  (let ((name (smoke::smoke-branch-name "feature" 1)))
    (is (string= "smoke/feature/01" name))))

(test smoke-branch-name-double-digit
  (let ((name (smoke::smoke-branch-name "feature" 12)))
    (is (string= "smoke/feature/12" name))))

(test smoke-branch-name-zero-padded
  (let ((name (smoke::smoke-branch-name "my-branch" 3)))
    (is (string= "smoke/my-branch/03" name))))

;;; extract-pr-number

(test extract-pr-number-https
  (is (= 42 (smoke::extract-pr-number "https://github.com/user/repo/pull/42"))))

(test extract-pr-number-with-extra-path
  (is (= 123 (smoke::extract-pr-number "https://github.com/user/repo/pull/123/files"))))

(test extract-pr-number-no-match
  (is (null (smoke::extract-pr-number "https://github.com/user/repo/issues/42"))))

;;; repo-name regex (test the pattern directly, not the git call)

(test repo-name-ssh-url
  (multiple-value-bind (match groups)
      (re:scan-to-strings "github\\.com[:/](.+?)(?:\\.git)?$"
                          "git@github.com:owner/repo.git")
    (declare (ignore match))
    (is (string= "owner/repo" (str:replace-all ".git" "" (aref groups 0))))))

(test repo-name-https-url
  (multiple-value-bind (match groups)
      (re:scan-to-strings "github\\.com[:/](.+?)(?:\\.git)?$"
                          "https://github.com/owner/repo.git")
    (declare (ignore match))
    (is (string= "owner/repo" (str:replace-all ".git" "" (aref groups 0))))))

(test repo-name-https-no-git-suffix
  (multiple-value-bind (match groups)
      (re:scan-to-strings "github\\.com[:/](.+?)(?:\\.git)?$"
                          "https://github.com/owner/repo")
    (declare (ignore match))
    (is (string= "owner/repo" (str:replace-all ".git" "" (aref groups 0))))))

;;; pr-ci-status-from-info

(test pr-ci-status-from-info-success
  (let ((pr-info `((:status-check-rollup
                    ((:conclusion . "SUCCESS"))
                    ((:conclusion . "SUCCESS"))))))
    (is (eq :success (smoke::pr-ci-status-from-info pr-info)))))

(test pr-ci-status-from-info-failure
  (let ((pr-info `((:status-check-rollup
                    ((:conclusion . "SUCCESS"))
                    ((:conclusion . "FAILURE"))))))
    (is (eq :failure (smoke::pr-ci-status-from-info pr-info)))))

(test pr-ci-status-from-info-pending
  (let ((pr-info `((:status-check-rollup
                    ((:conclusion . "SUCCESS"))
                    ((:conclusion . nil))))))
    (is (eq :pending (smoke::pr-ci-status-from-info pr-info)))))

(test pr-ci-status-from-info-none
  (let ((pr-info `((:status-check-rollup))))
    (is (eq :none (smoke::pr-ci-status-from-info pr-info)))))

;;; migrate-state-entry

(test migrate-state-entry-legacy-patch-id
  "Legacy entry with :patch--id gets converted to index-based with smoke-branch."
  (let ((entry (list (cons :patch--id "abcdef1234567890") (cons :pr 42))))
    (let ((migrated (smoke::migrate-state-entry entry "feature" 0)))
      (is (= 0 (cdr (assoc :index migrated))))
      (is (= 42 (cdr (assoc :pr migrated))))
      (is (string= "smoke/feature/01" (cdr (assoc :smoke--branch migrated))))
      (is (null (assoc :patch--id migrated))))))

(test migrate-state-entry-index-no-smoke-branch
  "Entry with :index but no :smoke--branch gets smoke-branch added."
  (let ((entry (list (cons :index 1) (cons :pr 42))))
    (let ((migrated (smoke::migrate-state-entry entry "feature" 1)))
      (is (= 1 (cdr (assoc :index migrated))))
      (is (= 42 (cdr (assoc :pr migrated))))
      (is (string= "smoke/feature/02" (cdr (assoc :smoke--branch migrated)))))))

(test migrate-state-entry-already-migrated
  "Entry with :smoke--branch is returned unchanged."
  (let ((entry (list (cons :index 0)
                     (cons :pr 42)
                     (cons :smoke--branch "smoke/feature/01"))))
    (let ((migrated (smoke::migrate-state-entry entry "feature" 0)))
      (is (string= "smoke/feature/01" (cdr (assoc :smoke--branch migrated))))
      (is (= 42 (cdr (assoc :pr migrated)))))))

;;; migrate-state

(test migrate-state-legacy-patch-ids
  "migrate-state converts legacy patch-id entries to index-based."
  (let* ((state (list (cons :branch "feat")
                      (cons :stack
                            (list (list (cons :patch--id "aaaa1111bbbb") (cons :pr 1))
                                  (list (cons :patch--id "cccc2222dddd") (cons :pr 2))))))
         (migrated (smoke::migrate-state state)))
    (is (string= "feat" (cdr (assoc :branch migrated))))
    (let ((stack (cdr (assoc :stack migrated))))
      (is (= 2 (length stack)))
      (is (= 0 (cdr (assoc :index (first stack)))))
      (is (string= "smoke/feat/01" (cdr (assoc :smoke--branch (first stack)))))
      (is (= 1 (cdr (assoc :index (second stack)))))
      (is (string= "smoke/feat/02" (cdr (assoc :smoke--branch (second stack))))))))

(test migrate-state-already-migrated
  "migrate-state preserves already-migrated entries."
  (let* ((state (make-state-with-entries "feat"
                  '((0 10 "smoke/feat/01")
                    (1 20 "smoke/feat/02"))))
         (migrated (smoke::migrate-state state)))
    (let ((stack (cdr (assoc :stack migrated))))
      (is (= 2 (length stack)))
      (is (string= "smoke/feat/01" (cdr (assoc :smoke--branch (first stack)))))
      (is (string= "smoke/feat/02" (cdr (assoc :smoke--branch (second stack))))))))

;;; reconcile-stack

(defun make-commit (hash subject)
  "Helper to create a commit plist for testing."
  (list :hash hash :short (subseq hash 0 (min 7 (length hash)))
        :subject subject))

(test reconcile-position-match
  "Commits matched by position preserve their PR and branch."
  (let* ((commits (list (make-commit "aaa" "First")
                        (make-commit "bbb" "Second")))
         (state (make-state-with-entries "feat"
                  '((0 10 "smoke/feat/01")
                    (1 20 "smoke/feat/02")))))
    (multiple-value-bind (reconciled orphans)
        (smoke::reconcile-stack commits state)
      (is (= 2 (length reconciled)))
      (is (null orphans))
      (is (= 10 (getf (first reconciled) :pr)))
      (is (string= "smoke/feat/01" (getf (first reconciled) :branch)))
      (is (= 20 (getf (second reconciled) :pr)))
      (is (string= "smoke/feat/02" (getf (second reconciled) :branch))))))

(test reconcile-amended-commit
  "Amended commit (different hash, same position) still matches by position."
  (let* ((commits (list (make-commit "aaa-amended" "First amended")
                        (make-commit "bbb" "Second")))
         (state (make-state-with-entries "feat"
                  '((0 10 "smoke/feat/01")
                    (1 20 "smoke/feat/02")))))
    (multiple-value-bind (reconciled orphans)
        (smoke::reconcile-stack commits state)
      (is (= 2 (length reconciled)))
      (is (null orphans))
      (is (= 10 (getf (first reconciled) :pr)))
      (is (string= "smoke/feat/01" (getf (first reconciled) :branch)))
      (is (= 20 (getf (second reconciled) :pr))))))

(test reconcile-new-commit-appended
  "New commit appended gets no PR and no branch."
  (let* ((commits (list (make-commit "aaa" "First")
                        (make-commit "bbb" "Second")
                        (make-commit "ccc" "Third (new)")))
         (state (make-state-with-entries "feat"
                  '((0 10 "smoke/feat/01")
                    (1 20 "smoke/feat/02")))))
    (multiple-value-bind (reconciled orphans)
        (smoke::reconcile-stack commits state)
      (is (= 3 (length reconciled)))
      (is (null orphans))
      (is (= 10 (getf (first reconciled) :pr)))
      (is (= 20 (getf (second reconciled) :pr)))
      (is (null (getf (third reconciled) :pr)))
      (is (null (getf (third reconciled) :branch))))))

(test reconcile-commit-removed
  "Removed commit produces an orphan."
  (let* ((commits (list (make-commit "bbb" "Second")))
         (state (make-state-with-entries "feat"
                  '((0 10 "smoke/feat/01")
                    (1 20 "smoke/feat/02")))))
    (multiple-value-bind (reconciled orphans)
        (smoke::reconcile-stack commits state)
      (is (= 1 (length reconciled)))
      (is (= 1 (length orphans)))
      ;; The remaining commit matches position 0 (first state entry)
      (is (= 10 (getf (first reconciled) :pr)))
      ;; The orphan is the second state entry
      (is (= 20 (cdr (assoc :pr (first orphans))))))))

(test reconcile-empty-state
  "Empty state means all commits are unmatched."
  (let* ((commits (list (make-commit "aaa" "First")))
         (state (smoke::init-state "feat")))
    (multiple-value-bind (reconciled orphans)
        (smoke::reconcile-stack commits state)
      (is (= 1 (length reconciled)))
      (is (null orphans))
      (is (null (getf (first reconciled) :pr))))))

(test reconcile-empty-commits
  "No commits means no results and all state entries are orphans."
  (let* ((commits nil)
         (state (make-state-with-entries "feat"
                  '((0 10 "smoke/feat/01")))))
    (multiple-value-bind (reconciled orphans)
        (smoke::reconcile-stack commits state)
      (is (= 0 (length reconciled)))
      (is (= 1 (length orphans))))))

;;; build-state-from-reconciliation

(test build-state-from-reconciliation-basic
  "Builds state with only entries that have PRs."
  (let* ((reconciled (list (list :commit (make-commit "a" "First")
                                 :pr 10 :branch "smoke/feat/01"
                                 :position 1)
                           (list :commit (make-commit "b" "Second")
                                 :pr nil :branch nil
                                 :position 2)
                           (list :commit (make-commit "c" "Third")
                                 :pr 30 :branch "smoke/feat/03"
                                 :position 3)))
         (state (smoke::build-state-from-reconciliation "feat" reconciled)))
    (is (string= "feat" (cdr (assoc :branch state))))
    (let ((stack (cdr (assoc :stack state))))
      (is (= 2 (length stack)))
      (is (= 0 (cdr (assoc :index (first stack)))))
      (is (= 10 (cdr (assoc :pr (first stack)))))
      (is (string= "smoke/feat/01" (cdr (assoc :smoke--branch (first stack)))))
      (is (= 2 (cdr (assoc :index (second stack)))))
      (is (= 30 (cdr (assoc :pr (second stack))))))))
