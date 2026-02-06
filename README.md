# Smoke

A CLI for managing stacked diffs with squash-merge workflows.

## Overview

Smoke manages a stack of commits as individual pull requests. Each commit becomes one PR, with only the bottom PR (closest to `main`) marked ready for review. All others remain in draft until their parent merges.

This approach works seamlessly with squash-merge workflows where traditional stacked diff tools break.

## Commands

| Command       | Action                                           |
|---------------|--------------------------------------------------|
| `smoke`       | Show stack status (commits, PRs, CI, draft state)|
| `smoke push`  | Create/update PRs for each commit                |
| `smoke pull`  | Rebase onto main, update PRs, manage draft states|
| `smoke amend` | Pick a commit to amend, then rebase stack        |

## Model

```
main
 └── commit A  →  PR #1 (ready)
      └── commit B  →  PR #2 (draft)
           └── commit C  →  PR #3 (draft)
```

- Single branch with N commits ahead of `main`
- Each commit = one PR
- Only the bottom PR is ready for review
- When a PR merges, `smoke pull` rebases the stack and promotes the next PR

## Tracking

Smoke tracks commit-to-PR mappings locally using `git patch-id` for stable identification across rebases. No modification to commit messages required.

State is stored in `.smoke/state.json`:

```json
{
  "branch": "my-feature",
  "stack": [
    {"patch_id": "abc123", "pr": 42},
    {"patch_id": "def456", "pr": 43},
    {"patch_id": "ghi789", "pr": 44}
  ]
}
```

Merged PRs are detected via `gh pr view --json state,mergeCommit`.

## Installation

```bash
curl -fsSL https://raw.githubusercontent.com/nosnaws/smoke/main/install.sh | bash
```

This downloads the latest release binary for your platform and installs it to `/usr/local/bin`. To install elsewhere:

```bash
curl -fsSL https://raw.githubusercontent.com/nosnaws/smoke/main/install.sh | INSTALL_DIR=~/.local/bin bash
```

### Requirements

- Git
- GitHub CLI (`gh`) authenticated

### Building from source

```bash
sbcl --load build.lisp
```

## Future

- Worktree support: `smoke worktree <position>` to work on multiple stack positions simultaneously

## License

MIT
