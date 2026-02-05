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
         (updated (smoke::update-state-mapping state "abc123" 42)))
    (is (= 1 (length (cdr (assoc :stack updated)))))
    (is (= 42 (smoke::find-pr-for-patch-id updated "abc123")))))

(test update-state-mapping-updates-existing
  (let* ((state (smoke::init-state "main"))
         (state (smoke::update-state-mapping state "abc123" 42))
         (updated (smoke::update-state-mapping state "abc123" 99)))
    (is (= 1 (length (cdr (assoc :stack updated)))))
    (is (= 99 (smoke::find-pr-for-patch-id updated "abc123")))))

(test update-state-mapping-preserves-others
  (let* ((state (smoke::init-state "main"))
         (state (smoke::update-state-mapping state "aaa" 1))
         (state (smoke::update-state-mapping state "bbb" 2))
         (updated (smoke::update-state-mapping state "aaa" 10)))
    (is (= 2 (length (cdr (assoc :stack updated)))))
    (is (= 10 (smoke::find-pr-for-patch-id updated "aaa")))
    (is (= 2 (smoke::find-pr-for-patch-id updated "bbb")))))

;;; find-pr-for-patch-id

(test find-pr-for-patch-id-found
  (let* ((state (smoke::init-state "main"))
         (state (smoke::update-state-mapping state "patch1" 10)))
    (is (= 10 (smoke::find-pr-for-patch-id state "patch1")))))

(test find-pr-for-patch-id-missing
  (let ((state (smoke::init-state "main")))
    (is (null (smoke::find-pr-for-patch-id state "nonexistent")))))

;;; remove-merged-from-state

(test remove-merged-from-state-removes-correct
  (let* ((state (smoke::init-state "main"))
         (state (smoke::update-state-mapping state "aaa" 1))
         (state (smoke::update-state-mapping state "bbb" 2))
         (state (smoke::update-state-mapping state "ccc" 3))
         (cleaned (smoke::remove-merged-from-state state '("aaa" "ccc"))))
    (is (= 1 (length (cdr (assoc :stack cleaned)))))
    (is (null (smoke::find-pr-for-patch-id cleaned "aaa")))
    (is (= 2 (smoke::find-pr-for-patch-id cleaned "bbb")))
    (is (null (smoke::find-pr-for-patch-id cleaned "ccc")))))

(test remove-merged-from-state-preserves-branch
  (let* ((state (smoke::init-state "my-branch"))
         (state (smoke::update-state-mapping state "aaa" 1))
         (cleaned (smoke::remove-merged-from-state state '("aaa"))))
    (is (string= "my-branch" (cdr (assoc :branch cleaned))))))

;;; smoke-branch-name

(test smoke-branch-name-format
  ;; smoke-branch-name calls current-branch, so we test indirectly
  ;; by checking the format with a known patch-id length
  (let ((name (smoke::smoke-branch-name "abcdef1234567890")))
    (is (str:starts-with-p "smoke/" name))
    (is (str:ends-with-p "/abcdef12" name))))

(test smoke-branch-name-short-patch-id
  (let ((name (smoke::smoke-branch-name "abc")))
    (is (str:ends-with-p "/abc" name))))

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
