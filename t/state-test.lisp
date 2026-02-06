(in-package :smoke-test)

(def-suite smoke-tests
  :description "Tests for smoke internals.")

(in-suite smoke-tests)

;;; init-state

(test init-state-structure
  (let ((state (smoke::init-state "feature-branch")))
    (is (string= "feature-branch" (cdr (assoc :branch state))))
    (is (null (cdr (assoc :stack state))))))

;;; update-state-mapping

(test update-state-mapping-adds-entry
  (let* ((state (smoke::init-state "main"))
         (updated (smoke::update-state-mapping state "abc123" 42 "smoke/main/01")))
    (is (= 1 (length (cdr (assoc :stack updated)))))
    (is (= 42 (smoke::find-pr-for-patch-id updated "abc123")))
    (let ((entry (first (cdr (assoc :stack updated)))))
      (is (string= "smoke/main/01" (cdr (assoc :smoke-branch entry)))))))

(test update-state-mapping-updates-existing
  (let* ((state (smoke::init-state "main"))
         (state (smoke::update-state-mapping state "abc123" 42 "smoke/main/01"))
         (updated (smoke::update-state-mapping state "abc123" 99 "smoke/main/01")))
    (is (= 1 (length (cdr (assoc :stack updated)))))
    (is (= 99 (smoke::find-pr-for-patch-id updated "abc123")))))

(test update-state-mapping-preserves-others
  (let* ((state (smoke::init-state "main"))
         (state (smoke::update-state-mapping state "aaa" 1 "smoke/main/01"))
         (state (smoke::update-state-mapping state "bbb" 2 "smoke/main/02"))
         (updated (smoke::update-state-mapping state "aaa" 10 "smoke/main/01")))
    (is (= 2 (length (cdr (assoc :stack updated)))))
    (is (= 10 (smoke::find-pr-for-patch-id updated "aaa")))
    (is (= 2 (smoke::find-pr-for-patch-id updated "bbb")))))

;;; find-pr-for-patch-id

(test find-pr-for-patch-id-found
  (let* ((state (smoke::init-state "main"))
         (state (smoke::update-state-mapping state "patch1" 10 "smoke/main/01")))
    (is (= 10 (smoke::find-pr-for-patch-id state "patch1")))))

(test find-pr-for-patch-id-missing
  (let ((state (smoke::init-state "main")))
    (is (null (smoke::find-pr-for-patch-id state "nonexistent")))))

;;; remove-merged-from-state

(test remove-merged-from-state-removes-correct
  (let* ((state (smoke::init-state "main"))
         (state (smoke::update-state-mapping state "aaa" 1 "smoke/main/01"))
         (state (smoke::update-state-mapping state "bbb" 2 "smoke/main/02"))
         (state (smoke::update-state-mapping state "ccc" 3 "smoke/main/03"))
         (cleaned (smoke::remove-merged-from-state state '("aaa" "ccc"))))
    (is (= 1 (length (cdr (assoc :stack cleaned)))))
    (is (null (smoke::find-pr-for-patch-id cleaned "aaa")))
    (is (= 2 (smoke::find-pr-for-patch-id cleaned "bbb")))
    (is (null (smoke::find-pr-for-patch-id cleaned "ccc")))))

(test remove-merged-from-state-preserves-branch
  (let* ((state (smoke::init-state "my-branch"))
         (state (smoke::update-state-mapping state "aaa" 1 "smoke/my-branch/01"))
         (cleaned (smoke::remove-merged-from-state state '("aaa"))))
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

(test migrate-state-entry-legacy
  "Legacy entry without :smoke-branch gets reconstructed branch name."
  (let ((entry (list (cons :patch--id "abcdef1234567890") (cons :pr 42)))
        (branch "feature"))
    (let ((migrated (smoke::migrate-state-entry entry branch)))
      (is (string= "smoke/feature/abcdef12" (cdr (assoc :smoke-branch migrated))))
      (is (= 42 (cdr (assoc :pr migrated))))
      (is (string= "abcdef1234567890" (cdr (assoc :patch--id migrated)))))))

(test migrate-state-entry-already-migrated
  "Entry with :smoke-branch is returned unchanged."
  (let ((entry (list (cons :patch--id "abcdef1234567890")
                     (cons :pr 42)
                     (cons :smoke-branch "smoke/feature/01"))))
    (let ((migrated (smoke::migrate-state-entry entry "feature")))
      (is (string= "smoke/feature/01" (cdr (assoc :smoke-branch migrated)))))))

(test migrate-state-entry-short-patch-id
  "Legacy entry with short patch ID doesn't error."
  (let ((entry (list (cons :patch--id "abc") (cons :pr 1))))
    (let ((migrated (smoke::migrate-state-entry entry "main")))
      (is (string= "smoke/main/abc" (cdr (assoc :smoke-branch migrated)))))))

;;; migrate-state

(test migrate-state-full
  "migrate-state processes all entries."
  (let* ((state (list (cons :branch "feat")
                      (cons :stack
                            (list (list (cons :patch--id "aaaa1111bbbb") (cons :pr 1))
                                  (list (cons :patch--id "cccc2222dddd") (cons :pr 2))))))
         (migrated (smoke::migrate-state state)))
    (is (string= "feat" (cdr (assoc :branch migrated))))
    (let ((stack (cdr (assoc :stack migrated))))
      (is (= 2 (length stack)))
      (is (string= "smoke/feat/aaaa1111" (cdr (assoc :smoke-branch (first stack)))))
      (is (string= "smoke/feat/cccc2222" (cdr (assoc :smoke-branch (second stack))))))))

;;; reconcile-stack

(defun make-commit (hash patch-id subject)
  "Helper to create a commit plist for testing."
  (list :hash hash :short (subseq hash 0 (min 7 (length hash)))
        :subject subject :patch-id patch-id))

(defun make-state-with-entries (branch entries)
  "Helper: ENTRIES is list of (patch-id pr smoke-branch)."
  (list (cons :branch branch)
        (cons :stack
              (mapcar (lambda (e)
                        (list (cons :patch--id (first e))
                              (cons :pr (second e))
                              (cons :smoke-branch (third e))))
                      entries))))

(test reconcile-patch-id-match
  "Commits matched by patch ID preserve their PR and branch."
  (let* ((commits (list (make-commit "aaa" "p1" "First")
                        (make-commit "bbb" "p2" "Second")))
         (state (make-state-with-entries "feat"
                  '(("p1" 10 "smoke/feat/01")
                    ("p2" 20 "smoke/feat/02")))))
    (multiple-value-bind (reconciled orphans)
        (smoke::reconcile-stack commits state)
      (is (= 2 (length reconciled)))
      (is (null orphans))
      (is (= 10 (getf (first reconciled) :pr)))
      (is (string= "smoke/feat/01" (getf (first reconciled) :branch)))
      (is (= 20 (getf (second reconciled) :pr)))
      (is (string= "smoke/feat/02" (getf (second reconciled) :branch))))))

(test reconcile-position-fallback-amend
  "Amended commit (different patch ID, same position) falls back to position match."
  (let* ((commits (list (make-commit "aaa" "p1-amended" "First amended")
                        (make-commit "bbb" "p2" "Second")))
         (state (make-state-with-entries "feat"
                  '(("p1-original" 10 "smoke/feat/01")
                    ("p2" 20 "smoke/feat/02")))))
    (multiple-value-bind (reconciled orphans)
        (smoke::reconcile-stack commits state)
      (is (= 2 (length reconciled)))
      (is (null orphans))
      ;; First commit matched by position fallback
      (is (= 10 (getf (first reconciled) :pr)))
      (is (string= "smoke/feat/01" (getf (first reconciled) :branch)))
      ;; Second commit matched by patch ID
      (is (= 20 (getf (second reconciled) :pr))))))

(test reconcile-new-commit-appended
  "New commit appended gets no PR and no branch."
  (let* ((commits (list (make-commit "aaa" "p1" "First")
                        (make-commit "bbb" "p2" "Second")
                        (make-commit "ccc" "p3" "Third (new)")))
         (state (make-state-with-entries "feat"
                  '(("p1" 10 "smoke/feat/01")
                    ("p2" 20 "smoke/feat/02")))))
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
  (let* ((commits (list (make-commit "bbb" "p2" "Second")))
         (state (make-state-with-entries "feat"
                  '(("p1" 10 "smoke/feat/01")
                    ("p2" 20 "smoke/feat/02")))))
    (multiple-value-bind (reconciled orphans)
        (smoke::reconcile-stack commits state)
      (is (= 1 (length reconciled)))
      (is (= 1 (length orphans)))
      ;; The remaining commit matched by patch ID
      (is (= 20 (getf (first reconciled) :pr)))
      ;; The orphan is the removed commit's entry
      (is (string= "p1" (cdr (assoc :patch--id (first orphans))))))))

(test reconcile-reordered-commits
  "Reordered commits still match by patch ID."
  (let* ((commits (list (make-commit "bbb" "p2" "Second")
                        (make-commit "aaa" "p1" "First")))
         (state (make-state-with-entries "feat"
                  '(("p1" 10 "smoke/feat/01")
                    ("p2" 20 "smoke/feat/02")))))
    (multiple-value-bind (reconciled orphans)
        (smoke::reconcile-stack commits state)
      (is (= 2 (length reconciled)))
      (is (null orphans))
      ;; p2 is now first, p1 is now second
      (is (= 20 (getf (first reconciled) :pr)))
      (is (= 10 (getf (second reconciled) :pr))))))

(test reconcile-empty-state
  "Empty state means all commits are unmatched."
  (let* ((commits (list (make-commit "aaa" "p1" "First")))
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
                  '(("p1" 10 "smoke/feat/01")))))
    (multiple-value-bind (reconciled orphans)
        (smoke::reconcile-stack commits state)
      (is (= 0 (length reconciled)))
      (is (= 1 (length orphans))))))

;;; build-state-from-reconciliation

(test build-state-from-reconciliation-basic
  "Builds state with only entries that have PRs."
  (let* ((reconciled (list (list :commit (make-commit "a" "p1" "First")
                                 :pr 10 :branch "smoke/feat/01"
                                 :patch-id "p1" :position 1)
                           (list :commit (make-commit "b" "p2" "Second")
                                 :pr nil :branch nil
                                 :patch-id "p2" :position 2)
                           (list :commit (make-commit "c" "p3" "Third")
                                 :pr 30 :branch "smoke/feat/03"
                                 :patch-id "p3" :position 3)))
         (state (smoke::build-state-from-reconciliation "feat" reconciled)))
    (is (string= "feat" (cdr (assoc :branch state))))
    (let ((stack (cdr (assoc :stack state))))
      (is (= 2 (length stack)))
      (is (string= "p1" (cdr (assoc :patch--id (first stack)))))
      (is (= 10 (cdr (assoc :pr (first stack)))))
      (is (string= "smoke/feat/01" (cdr (assoc :smoke-branch (first stack)))))
      (is (string= "p3" (cdr (assoc :patch--id (second stack)))))
      (is (= 30 (cdr (assoc :pr (second stack))))))))
